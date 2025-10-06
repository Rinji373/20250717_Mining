#検証用サイトを統一させる

# 必要なパッケージの読み込み
library(sf)
library(stringr)
library(dplyr)
library(magrittr)

# 1. ポリゴン(shp)の読み込みと対応リスト作成
shp_path <- "C:/Users/casakuma-lab-04-std/Downloads/polygon_Bare_2000-2010 3/polygon_Bare_2000-2010/polygon_Bare_2000_ダウンロード.shp"
polygon_sf <- st_read(shp_path)

# OBJECTID2とgroup_idの対応リスト作成
correspondence_tbl <- polygon_sf %>%
  select(OBJECTID2, group_id) %>%
  distinct()

unique(correspondence_tbl$group_id)

# 2. 画像ファイルの読み込み
img_folder <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat7_2000/"
img_files <- list.files(img_folder, pattern = "\\.tif$", full.names = TRUE)

# 3. 画像名からOBJECTID2を抽出
extract_objectid2 <- function(filename) {
  # 例: ..._polygon273 の部分から273を抽出
  str_match(filename, "polygon(\\d+)")[,2] %>% as.integer()
}

img_objectid2 <- sapply(img_files, extract_objectid2)

# 4. OBJECTID2からgroup_idを分類し、ユニーク値をリスト化
img_group_ids <- correspondence_tbl %>%
  filter(OBJECTID2 %in% img_objectid2) %>%
  pull(group_id) %>%
  unique()

print(img_group_ids)

# ...existing code...

# 5. もう一つの画像フォルダを読み込む
img_folder2 <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat7_2000"
img_files2 <- list.files(img_folder2, pattern = "\\.tif$", full.names = TRUE)

# 6. 画像名からOBJECTID2を抽出し、group_idを分類
correspondence_tbl$OBJECTID2 <- as.integer(correspondence_tbl$OBJECTID2)
img_objectid2_2 <- sapply(img_files2, extract_objectid2)
length(img_objectid2_2) # 1708 になるはず
img_objectid2_2 <- as.integer(img_objectid2_2)

img_group_ids_2 <- correspondence_tbl %>%
  filter(OBJECTID2 %in% img_objectid2_2) %>%
  pull(group_id)
unique(img_objectid2_2)
unique(img_group_ids_2)

# 7. img_group_idsに存在するgroup_idを持つ画像をコピーして出力
output_folder <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat7_2000_サイト統一"
dir.create(output_folder, showWarnings = FALSE)

length(img_files2)
length(img_objectid2_2)
sum(is.na(img_objectid2_2))

# データフレーム化
img_df <- data.frame(
  file = img_files2,
  OBJECTID2 = img_objectid2_2
)

# joinでgroup_idを紐付け
img_df <- img_df %>%
  left_join(correspondence_tbl, by = "OBJECTID2")

# 判定とコピー
for (i in seq_len(nrow(img_df))) {
  判定 <- !is.na(img_df$group_id[i]) && img_df$group_id[i] %in% img_group_ids
  cat("画像:", img_df$file[i], "OBJECTID2:", img_df$OBJECTID2[i], "group_id:", img_df$group_id[i], "判定:", 判定, "\n")
  if (判定) {
    res <- file.copy(img_df$file[i], file.path(output_folder, basename(img_df$file[i])))
    cat("コピー結果:", res, "\n")
  }
}


###################2010年の画像を対象とした場合#################################################################################
library(sf)
library(stringr)
library(dplyr)
library(magrittr)

# 1. 1つ目のポリゴン(shp)の読み込みと対応リスト作成
shp_path1 <- "C:/Users/casakuma-lab-04-std/Downloads/polygon_Bare_2000-2010 3/polygon_Bare_2000-2010/polygon_Bare_2000_ダウンロード.shp"
polygon_sf1 <- st_read(shp_path1)
correspondence_tbl1 <- polygon_sf1 %>%
  select(OBJECTID2, group_id) %>%
  distinct()

# 2. 1つ目の画像フォルダの画像ファイル取得
img_folder1 <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat7_2000/"
img_files1 <- list.files(img_folder1, pattern = "\\.tif$", full.names = TRUE)

# 3. 画像名からOBJECTID2を抽出
extract_objectid2 <- function(filename) {
  str_match(filename, "polygon(\\d+)")[,2] %>% as.integer()
}
img_objectid2_1 <- sapply(img_files1, extract_objectid2)

# 4. group_idを分類し、ユニーク値をリスト化
img_df1 <- data.frame(
  file = img_files1,
  OBJECTID2 = img_objectid2_1
)
img_df1 <- img_df1 %>%
  left_join(correspondence_tbl1, by = "OBJECTID2")
img_group_ids <- unique(img_df1$group_id)

# 5. 2つ目のポリゴン(shp)の読み込みと対応リスト2作成
shp_path2 <- "C:/Users/casakuma-lab-04-std/Downloads/polygon_Bare_2000-2010 3/polygon_Bare_2000-2010/polygon_Bare_2010.shp"

sf::sf_use_s2(FALSE)
polygon_sf2 <- st_read(shp_path2)
correspondence_tbl2 <- polygon_sf2 %>%
  select(OBJECTID2, group_id) %>%
  distinct()

# 6. 2つ目の画像フォルダの画像ファイル取得
img_folder2 <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat5_2010/"
img_files2 <- list.files(img_folder2, pattern = "\\.tif$", full.names = TRUE)

# 7. 画像名からOBJECTID2を抽出し、group_idを分類
img_objectid2_2 <- sapply(img_files2, extract_objectid2)
img_df2 <- data.frame(
  file = img_files2,
  OBJECTID2 = img_objectid2_2
)
img_df2 <- img_df2 %>%
  left_join(correspondence_tbl2, by = "OBJECTID2")

# 8. img_group_idsに存在するgroup_idを持つ画像をコピーして出力
output_folder <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat5_2010_サイトを統一/"
dir.create(output_folder, showWarnings = FALSE)

for (i in seq_len(nrow(img_df2))) {
  判定 <- !is.na(img_df2$group_id[i]) && img_df2$group_id[i] %in% img_group_ids
  if (判定) {
    file.copy(img_df2$file[i], file.path(output_folder, basename(img_df2$file[i])))
  }
}