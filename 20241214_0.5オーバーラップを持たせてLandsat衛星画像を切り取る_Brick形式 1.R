# ライブラリ
library(raster)
library(tools)

# 入力フォルダのパス
input_folder <- "F:/2024_hirora_Toyota6/2023_hirota/tool/ArcGis/2024_hirota/20250717/Cropp_Image/Sentinel-2_Mincomposit_3840/"
image_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)  # フォルダ内の.tifファイルを取得

# タイルサイズとオーバーラップ
tile_width <- 640
tile_height <- 640
overlap <- 0.5
step_size <- tile_width * (1 - overlap)  # ステップサイズ

# 出力フォルダ
output_folder <- 
dir.create(output_folder, showWarnings = FALSE)

# 各画像ファイルに対して処理を行う
for (image_file in image_files) {
  # 画像ファイルをstackで読み込む
  img_stack <- stack(image_file)
  # 元画像のサイズ
  img_width <- ncol(img_stack)
  img_height <- nrow(img_stack)
  num_bands <- nlayers(img_stack)  # バンド数
  # タイルの数
  x_tiles <- ceiling((img_width - tile_width) / step_size) + 1  # 横方向
  y_tiles <- ceiling((img_height - tile_height) / step_size) + 1  # 縦方向
  # 元画像のベース名を取得
  base_name <- file_path_sans_ext(basename(image_file))  # 画像ファイルの拡張子を除いた名前
  # タイルを切り出し、保存
  for (y in 0:(y_tiles - 1)) {
    for (x in 0:(x_tiles - 1)) {
      # タイルの切り出し座標
      start_x <- round(x * step_size)
      start_y <- round(y * step_size)
      # 終了座標を計算（画像の境界を越えないように調整）
      end_x <- min(start_x + tile_width, img_width)  # 画像の右端を越えないように
      end_y <- min(start_y + tile_height, img_height)  # 画像の下端を越えないように
      # extentの設定
      extent_to_crop <- extent(start_x, end_x, start_y, end_y)
      print(extent_to_crop)
      # バンドごとに切り出し、リストで保持
      tile_bands <- list()
      for (b in 1:num_bands) {
        band <- raster(img_stack, layer = b)
        tile_bands[[b]] <- crop(band, extent(
          xmin(img_stack) + (start_x) * res(img_stack)[1],
          xmin(img_stack) + (end_x) * res(img_stack)[1],
          ymax(img_stack) - (end_y) * res(img_stack)[2],
          ymax(img_stack) - (start_y) * res(img_stack)[2]
        ))
      }
      # すべてのバンドをstackに結合
      tile_stack <- stack(tile_bands)
      # 出力ファイル名に元画像名を保持
      output_file <- file.path(
        output_folder,
        paste0(base_name, "_tile_", x + 1, "_", y + 1, ".tif")
      )
      # タイルを保存
      tile_brick <- brick(tile_stack)
      # タイルを保存（tryCatchを使用してエラーをキャッチ）
      tryCatch(
        {
          # 出力ファイルを保存
          writeRaster(tile_brick, filename = output_file, format = "GTiff", overwrite = TRUE)
          
          print(paste("Saved:", output_file, "with", nlayers(tile_stack), "bands"))
        },
        error = function(e) {
          print(paste("Error saving tile:", output_file, "->", e$message))
        }
      )
      
      # 確認: 出力画像のバンド数
      print(paste("Saved:", output_file, "with", nlayers(tile_stack), "bands"))
    }
  }
}