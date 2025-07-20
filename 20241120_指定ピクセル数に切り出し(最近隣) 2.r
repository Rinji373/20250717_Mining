# 1280pix -----------------------------------------------------------------
library(raster)
library(tools)


directory_path <- "I:/2024_hirora_Toyota5/tool/ArcGIS/fold_images2/train_All_6band/"
file_list <- list.files(directory_path, pattern = "\\.tif$", full.names = TRUE)

for (file_path in file_list) {
  # 1. Landsat 画像を読み込む
  landsat_image <- stack(file_path)
  
  # ファイル名を取得
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # 2. 関心領域のサイズを定義
  half_size <- 320  # ピクセル単位でROIの半径を定義
  
  # 3. 中心座標を取得して関心領域を作成
  center_lon <- extent(landsat_image)[1] + (extent(landsat_image)[2] - extent(landsat_image)[1]) / 2
  center_lat <- extent(landsat_image)[3] + (extent(landsat_image)[4] - extent(landsat_image)[3]) / 2
  
  polygon <- extent(center_lon - half_size * res(landsat_image)[1], center_lon + half_size * res(landsat_image)[1],
                    center_lat - half_size * res(landsat_image)[2], center_lat + half_size * res(landsat_image)[2])
  
  # 4. 関心領域に画像を切り抜く
  cropped_image <- crop(landsat_image, polygon)
  
  # 5. 切り抜いた画像をリサンプリング
  cropped_size <- dim(cropped_image)
  new_raster <- raster(nrow = cropped_size[1], ncol = cropped_size[2], ext = extent(polygon))
  res(new_raster) <- res(cropped_image)
  
  cropped_image <- resample(cropped_image, new_raster, method = "ngb")
  
  # Band名を引き継ぐ
  if (!is.null(names(landsat_image))) {
    names(cropped_image) <- names(landsat_image)
  }
  
  # 6. 切り抜いた画像を保存
  output_path <- paste0("I:/2024_hirora_Toyota5/tool/ArcGIS/fold_image3/tarin_All_6band_640pix/", file_name, ".tif")
  writeRaster(cropped_image, output_path, format = "GTiff", datatype = 'INT2U', options = "COMPRESS=LZW", overwrite = TRUE)
}