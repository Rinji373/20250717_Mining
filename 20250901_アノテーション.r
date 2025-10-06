library(terra)

# 1. 画像とポリゴンのパスを指定
image_folder <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat7_2000_サイト統一"
polygon_path <- "F:/2024_hirora_Toyota6/2023_hirota/polygon_Bare_2000-2010/polygon_Bare_2000.shp"
output_folder <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat7_2000_label_サイト統一"

# 2. 画像とポリゴンを読み込む
image_files <- list.files(image_folder, pattern = "\\.tif$", full.names = TRUE)
poly <- vect(polygon_path)

# 3. 各画像ごとにアノテーション画像を作成
for (img_path in image_files) {
  img <- rast(img_path)
  # ポリゴンと画像の重なり部分を1、それ以外を0に
  mask_raster <- rasterize(poly, img, field=1, background=0)
  # 4. 保存
  out_name <- file.path(output_folder, paste0(basename(img_path)))
  writeRaster(mask_raster, out_name, overwrite=TRUE)
}