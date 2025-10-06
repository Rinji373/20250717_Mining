library(terra)
library(fs)

input_dir <- "C:\\Users\\atori\\Downloads\\20250909_test\\20250909_test"
output_dir <- "C:\\Users\\atori\\Downloads\\20250909_test\\20250909_output"
polygon_path <- "C:\\Users\\atori\\Downloads\\Global_mining_Tang\\MiningPolygon_73943_Tang.shp"
#polygon_path <- "C:\\Users\\atori\\Downloads\\Polygon_Bare_20250819_2021ver\\Polygon_Bare_20250819_2021ver\\polygon_bare20250819_2021ver.shp"
dir_create(output_dir)

all_tif_files <- dir_ls(input_dir, regexp = "\\.tif$", recurse = TRUE)
print("all_tif_files:")
print(all_tif_files)

# シーンごとにグループ化（ファイル名の共通部分でグループ化）
scene_names <- unique(sub("(_SR_B[0-9]+|_QA_PIXEL)?\\.tif$", "", basename(all_tif_files)))

poly <- vect(polygon_path)
# 画像のCRS取得用に一つサンプル画像を読む
sample_img <- rast(all_tif_files[1])
if (crs(poly) != crs(sample_img)) {
  print("CRSが異なるためポリゴンを画像のCRSに変換します")
  poly <- project(poly, crs(sample_img))
}

for (scene in scene_names) {
  band_files <- all_tif_files[grepl(scene, all_tif_files)]
  # stackで一括読み込み
  s <- rast(band_files)
  
  # QA_PIXELバンドのインデックスを特定
  qa_idx <- grep("QA_PIXEL", names(s), ignore.case = TRUE)
  if (length(qa_idx) == 0) {
    print("QA_PIXELバンドが見つかりません。スキップします。")
    next
  }
  qa <- s[[qa_idx]]
  
  # マスク条件（GEE Landsat Collection 2仕様例: 32=雲影, 64=雪, 128=雲, 256=薄雲, 1=欠損画素）
  mask <- (bitwAnd(values(qa), 32) != 0) |  # 雲影
          (bitwAnd(values(qa), 64) != 0) |  # 雪
          (bitwAnd(values(qa), 128) != 0) | # 雲
          (bitwAnd(values(qa), 256) != 0) | # 薄雲
          (bitwAnd(values(qa), 1) != 0)     # 欠損画素
  valid_ratio <- sum(!mask, na.rm = TRUE) / length(mask)
  print(paste("valid_ratio:", valid_ratio))
  
  # 有効画素が90%以上の場合のみ後続処理
  if (valid_ratio >= 0.9) {
    print("有効画素が90%未満のためスキップ")
    next
  }
  
  # 画像を640×640pixに切り抜く（マスク前）
  half_size <- 320
  center_x <- ext(s)[1] + (ext(s)[2] - ext(s)[1]) / 2
  center_y <- ext(s)[3] + (ext(s)[4] - ext(s)[3]) / 2
  crop_ext <- ext(center_x - half_size * res(s)[1], center_x + half_size * res(s)[1],
                  center_y - half_size * res(s)[2], center_y + half_size * res(s)[2])
  cropped <- crop(s, crop_ext)
  cropped_size <- dim(cropped)
  new_raster <- rast(nrow = cropped_size[1], ncol = cropped_size[2], nlyrs = nlyr(s), ext = crop_ext)
  res(new_raster) <- res(cropped)
  cropped <- resample(cropped, new_raster, method = "near")
  names(cropped) <- names(s)

  # 保存先フォルダ指定
  cropped_img_dir <- file.path(output_dir, "cropped_img")
  cropped_label_dir <- file.path(output_dir, "cropped_label")
  dir_create(cropped_img_dir)
  dir_create(cropped_label_dir)

  # マスク前画像を保存
  out_img_path <- file.path(cropped_img_dir, paste0(scene, ".tif"))
  writeRaster(cropped, out_img_path, overwrite = TRUE)

  # ここではマスクで有効画素:1, 無効画素:0 のラベル画像を作成
  label_raster <- rast(nrows = nrow(cropped), ncols = ncol(cropped_qa), ext = ext(cropped_qa))
  values(label_raster) <- as.integer(!mask_cropped)
  names(label_raster) <- "annotation"
 # polyをcroppedのCRSに揃えてからcrop
  poly_proj <- project(poly, crs(cropped))
  poly_clip <- crop(poly_proj, ext(cropped))
  print("poly_clipポリゴン数:")
  print(length(poly_clip))
  label_raster <- rasterize(poly_clip, cropped, field=1, background=0)
  # QAバンドで欠損画素(1)を0に上書き
  cropped_qa <- cropped[[qa_idx]]
  missing_mask <- (bitwAnd(values(cropped_qa), 1) != 0)  # 欠損画素
  label_vals <- values(label_raster)
  label_vals[missing_mask] <- 0
  values(label_raster) <- label_vals
  names(label_raster) <- "annotation"
  
  # 5. 切り取り後の画像のうち指定したバンドのみを抽出する
  extract_bands <- c("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B7")  # 必要なバンド名を指定
  band_idx <- which(names(cropped) %in% extract_bands)
  cropped_selected <- cropped[[band_idx]]

  # 6. 切り取ってバンド抽出した画像とアノテーションラベル画像をそれぞれ別で指定したフォルダに保存する
  out_img_path <- file.path(cropped_img_dir, paste0(scene, ".tif"))
  writeRaster(cropped_selected, out_img_path, overwrite = TRUE)

  out_label_path <- file.path(cropped_label_dir, paste0(scene, ".tif"))
  writeRaster(label_raster, out_label_path, overwrite = TRUE)
}
