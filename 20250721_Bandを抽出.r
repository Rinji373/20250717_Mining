library(terra)

# 1. 入力・出力フォルダのパスを指定
input_folder <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_16bit_6band/All"
output_folder <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_16bit_6band/All_6band"

# 2. 対象Bandのインデックスまたは名前を指定（例: 2,3,4,8,11,12）
# target_bands <- c(SR_2,SR_3,SR_4)このように指定すると名前がしっかり残る
target_bands <- c(2,3,4,5,6,7)

# 3. フォルダ内のtifファイル一覧取得
tif_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

# 4. 各ファイルごとに処理
for (file in tif_files) {
  # 衛星画像読み込み
  img <- rast(file)
  
  # 指定Bandのみ抽出し、元のバンド名を保持
  img_selected <- img[[target_bands]]
  # 元のバンド名を取得し、抽出後の画像に付与
  orig_names <- names(img)
  sel_names <- orig_names[target_bands]
  names(img_selected) <- sel_names
  
  # 出力ファイル名作成
  out_name <- paste0(output_folder, "/", basename(file))
  
  # 画像保存
  writeRaster(img_selected, out_name, overwrite=TRUE)
}
