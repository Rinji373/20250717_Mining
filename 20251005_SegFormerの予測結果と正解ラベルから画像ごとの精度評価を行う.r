# 必要なライブラリをロード

library(raster)
library(sf)
library(caret)

# --- ユーザ設定: 予測フォルダ、ラベルフォルダ、出力ファイルパスを指定してください ---
# 例: predict_folder <- "G:/predictions/"
predict_folder <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat8_2021_predict/"
label_folder <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/train_label/" # ラベル（参照）ラスタが置かれたフォルダ
output_csv_path <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat8_2021_predict/results_true.csv"

# フォルダ内の TIFF ファイル一覧
predict_files <- list.files(predict_folder, pattern = "\\.tif$", full.names = TRUE)
label_files <- list.files(label_folder, pattern = "\\.tif$", full.names = TRUE)

if (length(predict_files) == 0) stop("予測フォルダに .tif ファイルが見つかりません: ", predict_folder)
if (length(label_files) == 0) stop("ラベルフォルダに .tif ファイルが見つかりません: ", label_folder)

# 結果格納用データフレーム
results_all <- data.frame(
  Image = character(),
  Precision = numeric(),
  Recall = numeric(),
  F1_Score = numeric(),
  FalseNegativeRate = numeric(),
  FalsePositiveRate = numeric(),
  Reference_size = numeric(),
  stringsAsFactors = FALSE
)

# ファイル名（拡張子なし）をキーにマップを作る
predict_map <- setNames(predict_files, tools::file_path_sans_ext(basename(predict_files)))
label_map <- setNames(label_files, tools::file_path_sans_ext(basename(label_files)))

# 共通のファイル名のみ処理
common_names <- intersect(names(predict_map), names(label_map))
if (length(common_names) == 0) stop("予測フォルダとラベルフォルダに共通のファイル名がありません。")

cat("Found", length(common_names), "common files to process.\n")

idx <- 1
for (name in common_names) {
  cat(sprintf("\n[%d/%d] %s\n", idx, length(common_names), name))
  idx <- idx + 1

  pred_path <- predict_map[[name]]
  lab_path <- label_map[[name]]

  pred_r <- tryCatch(raster(pred_path), error = function(e) { cat("予測ラスタ読み込み失敗:", pred_path, "\n"); return(NULL) })
  lab_r <- tryCatch(raster(lab_path), error = function(e) { cat("ラベルラスタ読み込み失敗:", lab_path, "\n"); return(NULL) })
  if (is.null(pred_r) || is.null(lab_r)) next

  # ラベルを予測ラスタのグリッドに合わせる
  # まず、画像サイズ（ピクセル数: nrow x ncol）が同じならピクセルインデックスで対応させる
  lab_aligned <- NULL
  if (!is.null(pred_r) && !is.null(lab_r) && ncol(pred_r) == ncol(lab_r) && nrow(pred_r) == nrow(lab_r)) {
    # サイズが一致する場合は、座標系やextentが正しくなくてもピクセル単位で対応できる
    lab_aligned <- lab_r
    cat("サイズ一致のためピクセルインデックスでの対応を使用します。\n")
  } else {
    # サイズが異なる場合は従来の方法を試す
    if (!compareCRS(pred_r, lab_r)) {
      lab_aligned <- tryCatch(projectRaster(lab_r, pred_r, method = "ngb"), error = function(e) { cat("projectRasterに失敗:", name, "\n"); return(NULL) })
    } else if (!all(res(pred_r) == res(lab_r)) || !all(as.numeric(extent(pred_r)) == as.numeric(extent(lab_r)))) {
      lab_aligned <- tryCatch(resample(lab_r, pred_r, method = "ngb"), error = function(e) { cat("resampleに失敗:", name, "\n"); return(NULL) })
    } else {
      lab_aligned <- lab_r
    }
  }

  if (is.null(lab_aligned)) {
    cat("ラベルのアラインに失敗したためスキップします: ", name, "\n")
    results_all <- rbind(results_all, data.frame(Image = name, Precision = NA_real_, Recall = NA_real_, F1_Score = NA_real_, FalseNegativeRate = NA_real_, FalsePositiveRate = NA_real_, Reference_size = 0, stringsAsFactors = FALSE))
    next
  }

  # 値を抽出
  predicted <- values(pred_r)
  reference <- values(lab_aligned)

  # 有効画素のみ
  valid_idx <- which(!is.na(predicted) & !is.na(reference))
  if (length(valid_idx) == 0) {
    cat("有効な画素がありません: ", name, "\n")
    results_all <- rbind(results_all, data.frame(Image = name, Precision = NA_real_, Recall = NA_real_, F1_Score = NA_real_, FalseNegativeRate = NA_real_, FalsePositiveRate = NA_real_, Reference_size = 0, stringsAsFactors = FALSE))
    next
  }

  predicted <- predicted[valid_idx]
  reference <- reference[valid_idx]

  # 0/1 に変換（必要ならユーザー側で閾値を適用してください）
  predicted_f <- factor(as.integer(predicted), levels = c(0,1))
  reference_f <- factor(as.integer(reference), levels = c(0,1))

  # 混合行列（table を用いる）
  mat <- table(predicted_f, reference_f)
  # 常に 2x2 にする
  if (!all(dim(mat) == c(2,2))) {
    full_mat <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))
    rr <- rownames(mat); cc <- colnames(mat)
    full_mat[rr, cc] <- mat
    mat <- full_mat
  }

  TP <- as.numeric(mat["1","1"])
  FN <- as.numeric(mat["0","1"])
  FP <- as.numeric(mat["1","0"])
  TN <- as.numeric(mat["0","0"])

  reference_size <- sum(reference == 1, na.rm = TRUE)

  precision_1 <- ifelse((TP + FP) > 0, TP / (TP + FP), NA_real_)
  recall_1 <- ifelse((TP + FN) > 0, TP / (TP + FN), NA_real_)
  FalseNegativeRate <- ifelse((TP + FN) > 0, FN / (TP + FN), NA_real_)
  FalsePositiveRate <- ifelse((TN + FP) > 0, FP / (TN + FP), NA_real_)
  f1_score_1 <- ifelse(!is.na(precision_1) & !is.na(recall_1) & (precision_1 + recall_1) > 0, 2 * (precision_1 * recall_1) / (precision_1 + recall_1), NA_real_)

  cat(sprintf("Precision: %s, Recall: %s, F1: %s, Reference_size: %d\n", format(precision_1, digits = 4), format(recall_1, digits = 4), format(f1_score_1, digits = 4), reference_size))

  results_all <- rbind(results_all, data.frame(Image = name, Precision = precision_1, Recall = recall_1, F1_Score = f1_score_1, FalseNegativeRate = FalseNegativeRate, FalsePositiveRate = FalsePositiveRate, Reference_size = reference_size, stringsAsFactors = FALSE))
}

# CSV 出力
write.csv(results_all, output_csv_path, row.names = FALSE)
cat("All results written to", output_csv_path, "\n")
