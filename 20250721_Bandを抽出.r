library(terra)

# 1. 入力・出力フォルダのパスを指定
input_folder <- "C:/Users/casakuma-lab-04-std/Downloads/test2/"
output_folder <- "C:/Users/casakuma-lab-04-std/Downloads/test2/testoutput"

# 2. 対象Bandのインデックスまたは名前を指定（例: 2,3,4,8,11,12）
target_bands <- c(2,3,4,8,11,12)

# 3. フォルダ内のtifファイル一覧取得
tif_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

# 4. 各ファイルごとに処理
for (file in tif_files) {
  # 衛星画像読み込み
  img <- rast(file)
  
  # 指定Bandのみ抽出
  img_selected <- img[[target_bands]]
  
  # 出力ファイル名作成
  out_name <- paste0(output_folder, "/", basename(file))
  
  # 画像保存
  writeRaster(img_selected, out_name, overwrite=TRUE)
}