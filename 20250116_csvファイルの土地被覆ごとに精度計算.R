#属性ごとの計算
library(dplyr) # データ操作用

# CSVファイルのパス
csv_path <- "F:/2024_hirora_Toyota5/tool/ArcGIS/fold_image6_val_134/640pix_output/640pix_0.5_common_Landsat7/results_true2.csv"

# CSVファイルを読み込み
csv_data <- read.csv(csv_path, stringsAsFactors = FALSE)

# 必要な列が含まれているか確認
if (!all(c("attribute_classification", "Precision", "Recall", "Reference_size") %in% names(csv_data))) {
  stop("CSVファイルに必要な列 (attribute_classification, Precision, Recall, Reference_size) が含まれていません。")
}

# F1-scoreの計算
csv_data <- csv_data %>%
  mutate(F1_score = 2 * ((Precision * Recall) / (Precision + Recall))) %>%
  mutate(F1_score = ifelse(is.na(F1_score), 0, F1_score)) # NAを0で置き換え

# 各属性ごとに平均を計算
attribute_summary <- csv_data %>%
  group_by(attribute_classification) %>%
  summarize(
    Avg_Precision = mean(Precision, na.rm = TRUE),
    Avg_Recall = mean(Recall, na.rm = TRUE),
    Avg_Reference_size = mean(Reference_size, na.rm = TRUE),
    Avg_F1_score = mean(F1_score, na.rm = TRUE),
    Avg_FalseNegativeRate = mean(FalseNegativeRate, na.rm = TRUE),
    Avg_FalsePositiveRate = mean(FalsePositiveRate, na.rm = TRUE)
  )

# 全体の平均を計算
overall_summary <- csv_data %>%
  summarize(
    Avg_Precision = mean(Precision, na.rm = TRUE),
    Avg_Recall = mean(Recall, na.rm = TRUE),
    Avg_Reference_size = mean(Reference_size, na.rm = TRUE),
    Avg_F1_score = mean(F1_score, na.rm = TRUE),
    Avg_FalseNegativeRate = mean(FalseNegativeRate, na.rm = TRUE),
    Avg_FalsePositiveRate = mean(FalsePositiveRate, na.rm = TRUE)
  ) %>%
  mutate(attribute_classification = "Overall")

# 各属性の結果と全体の結果を結合
final_summary <- bind_rows(attribute_summary, overall_summary)

# 結果を表示
print(final_summary)

# 結果をCSVに保存する場合
write.csv(final_summary, "F:/2024_hirora_Toyota5/tool/ArcGIS/fold_image6_val_134/640pix_output/640pix_0.5_common_Landsat7/results_true3.csv", row.names = FALSE)