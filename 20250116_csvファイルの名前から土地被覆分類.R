library(dplyr)     # データ操作
library(stringr)   # 文字列操作
library(sf)        # シェープファイル操作

# ファイルのパスを指定
csv_path <- "C:/study/2024/Research/mining/Result/640pix_output/640pix_output/640pix_0.5_common_Landsat7/results.csv"
shp_path <- "C:/study/2024/Research/mining/Polygon/polygon_LC/polygon_LC/buffers_dissolved_LC2.shp"  # シェープファイルのパス
output_csv_path <- "C:/study/2024/Research/mining/Result/640pix_output/640pix_output/640pix_0.5_common_Landsat7/results_attribt.csv" # 保存するCSVファイルのパス

# CSVファイルを読み込み
csv_data <- read.csv(csv_path, stringsAsFactors = FALSE)

# シェープファイルを読み込み
shp_data <- st_read(shp_path)

# CSVのImage列からgroup_dを抽出 (修正後)
csv_data <- csv_data %>%
  mutate(group_d = as.numeric(str_extract(Image, "(?<=classified_polygon )\\d+")))
# group_dとシェープファイルのattribt列を結合し、属性分類
classified_data <- csv_data %>%
  left_join(st_set_geometry(shp_data, NULL), by = c("group_d" = "group_d")) %>%
  mutate(attribute_classification = attribt)

# 結果を確認
head(classified_data)

# 属性分類結果を含むCSVを保存
write.csv(classified_data, output_csv_path, row.names = FALSE)