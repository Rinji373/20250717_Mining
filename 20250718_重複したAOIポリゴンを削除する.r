# 必要なパッケージ
if (!require("fs")) install.packages("fs")
if (!require("tools")) install.packages("tools")

library(fs)
library(tools)

# フォルダのパスを指定
input_dir <- "F:/2024_hirora_Toyota6/2023_hirota/tool/ArcGis/2024_hirota/20250717/Original_Image/Sentinel-2_Mincomposit/"
output_dir <- "F:/2024_hirora_Toyota6/2023_hirota/tool/ArcGis/2024_hirota/20250717/Original_Image/OUTPUT"

# 出力フォルダがなければ作成
if (!dir_exists(output_dir)) {
  dir_create(output_dir)
}

# zipファイル一覧取得
zip_files <- dir_ls(input_dir, regexp = "\\.zip$")

# 画像拡張子リスト
img_ext <- c("png", "jpg", "jpeg", "tif", "tiff", "bmp", "gif")

for (zip_file in zip_files) {
  # 一時解凍先
  temp_dir <- tempfile()
  dir_create(temp_dir)
  
  # 解凍
  unzip(zip_file, exdir = temp_dir)
  
  # 画像ファイルを検索
  img_files <- dir_ls(temp_dir, recurse = TRUE, regexp = paste0("\\.(", paste(img_ext, collapse="|"), ")$"))
  
  # 画像ファイルを出力フォルダにコピー
  for (img in img_files) {
    file_copy(img, path(output_dir, path_file(img)), overwrite = TRUE)
  }
  
  # 一時フォルダ削除
  dir_delete(temp_dir)
}

# 必要なパッケージ
if (!require("sf")) install.packages("sf")
if (!require("fs")) install.packages("fs")
library(sf)
library(fs)

# 1. 指定フォルダから画像とpolygon(shp)を読み込む
input_dir <- "F:/2024_hirora_Toyota6/2023_hirota/tool/ArcGis/2024_hirota/20250717/Original_Image/OUTPUT/" # 画像フォルダのパス
shp_path <- "F:/2024_hirora_Toyota6/2023_hirota/Polygon/Polygon_Bare_20250715/20250715_画像ダウンロード/20240709_all_overlapping_polygons_unique.shp" # シェープファイルのパス

# 画像ファイル一覧取得
img_ext <- c("png", "jpg", "jpeg", "tif", "tiff", "bmp", "gif")
img_files <- dir_ls(input_dir, regexp = paste0("\\.(", paste(img_ext, collapse="|"), ")$"))

# ポリゴンデータ読み込み
polygon <- st_read(shp_path)

# 2. polygonのgroup_id列をリスト化する
polygon_group_ids <- unique(polygon$group_id)

# 3. 画像ファイル名からgroup_idを抽出してリスト化
extract_group_id <- function(filename) {
  # ファイル名からgroup_idを抽出
  m <- regexpr("polygon_([0-9]+)_", filename)
  if (m[1] != -1) {
    as.integer(regmatches(filename, m)[[1]][1] %>% sub("polygon_", "", .) %>% sub("_", "", .))
  } else {
    NA
  }
}
img_group_ids <- unique(na.omit(sapply(basename(img_files), extract_group_id)))

# 4. polygonのリストに対して画像のgroup_idリストを比較し、重複の無いgroup_idを出力
unique_polygon_group_ids <- setdiff(polygon_group_ids, img_group_ids)
print(unique_polygon_group_ids)