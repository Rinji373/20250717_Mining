# 必要なライブラリをロード
library(raster)
library(sf)
library(caret)

# フォルダ内の全ての画像ファイルをリスト
image_folder <- "F:/2024_hirora_Toyota5/tool/ArcGIS/fold_image6_val_134/640pix_output/640pix_0.5_common_Landsat5/"
image_files <- list.files(image_folder, pattern = "\\.tif$", full.names = TRUE)  # .tif ファイルを対象

# ポリゴンファイルを読み込む
polygon_shp_path <- "C:/Users/casakuma-lab-02-std/Downloads/Mining200_All.shp"
polygons <- st_read(polygon_shp_path)
# . ポリゴンの座標次元を修正（XYZ → XY）
polygons <- st_zm(polygons, drop = TRUE, what = "ZM")


# 結果を格納するデータフレームを作成
results_all <- data.frame(
  Image = character(),
  Precision = numeric(),
  Recall = numeric(),
  F1_Score = numeric(),
  Reference_size = numeric(),
  stringsAsFactors = FALSE
)

i = 1

# 各画像に対して処理を実行
for (predict_image_path in image_files) {
  
  # 1. 画像ファイルを読み込む
  predict_image <- raster(predict_image_path)
  print(i)
  i = i + 1 
  
  # 3. CRSの確認と修正
  # 画像のCRSを取得
  image_crs <- crs(predict_image)
  
  # ポリゴンのCRSを画像のCRSに合わせる
  polygons <- st_transform(polygons, crs = st_crs(image_crs))
  
  # 4. 無効なジオメトリを修正
  polygons <- st_make_valid(polygons)  # 無効なポリゴンを修正
  
  # 5. 画像の範囲をsfオブジェクトとして作成
  image_extent <- as(extent(predict_image), "SpatialPolygons")   # raster::extent → SpatialPolygons
  crs(image_extent) <- image_crs                                # 画像のCRSを適用
  image_extent_sf <- st_as_sf(image_extent)                     # sfオブジェクトに変換
  
  # 6. ポリゴンを画像の範囲でクリップ
  polygons_clipped <- st_intersection(polygons, image_extent_sf)
  
  # Class フィールドを数値型に変換
  polygons_clipped$Class <- as.numeric(as.character(polygons_clipped$Class))
  
  # polygons_clipped が空かどうかを確認
  if (nrow(polygons_clipped) > 0) {
    # polygons_clipped が空でない場合のみ処理を実行
    polygon_raster <- rasterize(polygons_clipped, predict_image, field = "Class", fun = "first", background = 0)
    # 以降の処理を続ける
    # 例えば、raster に基づく解析など
  } else {
    # polygons_clipped が空の場合の処理
    cat("ポリゴンが空です。polygon_rasterの作成をスキップします。\n")
    # 必要であれば、ポリゴンが空の場合に行う別の処理を追加
  }
  
  # 8. 混合行列を作成し、precision、recall、F1-scoreを計算する
  predicted <- values(predict_image)  # 画像の値
  reference <- values(polygon_raster)  # ポリゴンの値
  
  # 欠損値を除外
  valid_data <- !is.na(predicted) & !is.na(reference)
  predicted <- predicted[valid_data]
  reference <- reference[valid_data]
  
  # 混合行列を作成
  conf_matrix <- confusionMatrix(factor(predicted, levels = c(0, 1)), 
                                 factor(reference, levels = c(0, 1)))
  
  # 混合行列の値を取得
  conf_matrix_table <- conf_matrix$table
  
  # クラス 1 の真陽性、偽陽性、偽陰性を抽出
  # 修正後のコード
  TP <- conf_matrix$table["1", "1"]  # 予測 1, 実際 1
  FN <- conf_matrix$table["0", "1"]  # 予測 0, 実際 1 (間違って 0 と予測)
  FP <- conf_matrix$table["1", "0"]  # 予測 1, 実際 0 (間違って 1 と予測)
  TN <- conf_matrix$table["0", "0"]  # 予測 0, 実際 0
  reference_size <- sum(reference == 1, na.rm = TRUE)
  
  # Precision と Recall を計算
  precision_1 <- TP / (TP + FP)
  recall_1 <- TP / (TP + FN)
  
  # False Negative Rateと　False Positive Rate
  FalseNegativeRate <- FN / (TP + FN)
  FalsePositiveRate <- FP / (TN + FP)
  
  # F1-Score の計算
  f1_score_1 <- 2 * (precision_1 * recall_1) / (precision_1 + recall_1)
  
  # 結果を出力
  cat("\nPrecision (Class 1):", precision_1, "\n")
  cat("Recall (Class 1):", recall_1, "\n")
  cat("F1-Score (Class 1):", f1_score_1, "\n")
  cat("reference_size:",reference_size,"\n")
  
  # 結果をリストに格納
  results <- data.frame(
    Image = basename(predict_image_path),  # predict_image のファイル名
    Precision = precision_1,
    Recall = recall_1,
    F1_Score = f1_score_1,
    FalseNegativeRate,
    FalsePositiveRate,
    Reference_size = reference_size  # Referenceにおける1の数を追加
  )
  
  # 結果を全ての結果データフレームに追加
  results_all <- rbind(results_all, results)
}

# 出力ファイルのパスを設定
output_csv_path <- "F:/2024_hirora_Toyota5/tool/ArcGIS/fold_image6_val_134/640pix_output/640pix_0.5_common_Landsat5/results_true.csv"  # 保存先を指定

# 結果をCSVファイルに書き込む
write.csv(results_all, output_csv_path, row.names = FALSE)

# メッセージを表示
cat("All results written to", output_csv_path, "\n")
