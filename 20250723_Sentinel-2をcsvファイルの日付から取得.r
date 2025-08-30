#########################################################################
# Landsat取得
# R環境でGoogle Earth Engineを使用する
### Google Earth EngineをR環境上で起動する
library("rgee")
ee_Initialize(drive = TRUE)

# 一度リセットする
# ee_clean_user_credentials()
# ee_Authenticate()

# Google Driveパッケージ、sf、lubridateの読み込み
library("googledrive")
library("sf")
library(lubridate)

# GoogleDriveの編集権限をチェックする
drive <- drive_find(pattern="*_epsg4326_scale500m_beech.tif")

# 鉱山ポリゴンの読み込み（SHAPE形式）
polygon <- st_read(dsn="F:/2024_hirora_Toyota6/2023_hirota/Polygon/Polygon_Bare_20250715/サイト抽出結果/20240709_all_overlapping_polygons_unique.shp")

# CSVファイルの読み込み（group_idとday&year2023の情報を含む）
csv_data <- read.csv("F:/2024_hirora_Toyota6/2023_hirota/Polygon/Polygon_Bare_20240709/mining_data_20240709.csv")  # CSVファイルのパスを適切に設定してください

# CSVファイルの構造を確認
print("CSV file structure:")
print(str(csv_data))
print("First few rows:")
print(head(csv_data))
print("Column names:")
print(names(csv_data))

####################################################################
#タスクを終了させる
# すべてのタスクを取得する
tasks <- ee$data$getTaskList()
# タスクIDを取得してキャンセル
if (length(tasks) > 0) {
  for (task in tasks) {
    if (task$state %in% c("RUNNING", "READY")) {
      ee$data$cancelTask(task$id)
      print(paste("Cancelled task:", task$id))
    }
  }
} else {
  print("No tasks to cancel.")
}

####################################################################
####################################################################
#月間最良画像のダウンロード
# 既存ファイル一覧とgroup_idリストを一度だけ取得
existing_files <- drive_ls(path = "Sentinel-2_Best_Image_csv")
existing_group_ids <- unique(unlist(regmatches(existing_files$name, gregexpr("(?<=_polygon)\\d+", existing_files$name, perl=TRUE))))
for(i in seq_len(nrow(polygon))){
  polygon1 <- polygon[i, ]
  group_id <- as.character(polygon1$group_id)
  print(paste("Processing polygon:", group_id))
  # group_id単位で処理済みかチェック
  if (group_id %in% existing_group_ids) {
    print(paste("Already processed polygon group_id:", group_id))
    next
  }
  
  # CSVファイルからgroup_idに対応するday&year2023の値を取得
  group_id <- polygon1$group_id
  print(paste("Looking for group_id:", group_id))
  print(paste("CSV columns:", paste(names(csv_data), collapse = ", ")))

  csv_row <- csv_data[csv_data$group_id == group_id, ]
  
  if(nrow(csv_row) == 0){
    print(paste("No date information found for group_id:", group_id))
    next
  }
  
  # 列名を確認して適切な列を選択
  date_column_names <- names(csv_data)[grepl("day.*year|date", names(csv_data), ignore.case = TRUE)]
  print(paste("Date-related columns found:", paste(date_column_names, collapse = ", ")))
  
  if(length(date_column_names) == 0){
    print("No date column found in CSV")
    next
  }
  
  # 最初に見つかった日付列を使用
  date_column <- date_column_names[1]
  day_year_value <- csv_row[[date_column]][1]
  
  print(paste("Using column:", date_column))
  print(paste("Raw date value:", day_year_value))
  print(paste("Class of date value:", class(day_year_value)))
  
  # day&year2023の値から日付を解析（例：20170702 -> 2017-07-02）
  if(is.null(day_year_value) || length(day_year_value) == 0 || is.na(day_year_value)){
    print(paste("Empty or NA date value for group_id:", group_id))
    next
  }
  


  print(paste("day_year_value before as.integer:", day_year_value))
  int_date <- suppressWarnings(as.integer(day_year_value))
  print(paste("int_date:", int_date))
  if(is.na(int_date)){
    print(paste("Invalid integer date for group_id:", group_id, "value:", day_year_value))
    next
  }
  date_str <- sprintf("%08d", int_date)
  print(paste("date_str:", date_str))
  if(date_str == "NA"){
    print(paste("date_str is NA for group_id:", group_id, "value:", day_year_value))
    next
  }

  base_date <- as.Date(date_str, format = "%Y%m%d")
  print(paste("base_date:", base_date))
  if(is.na(base_date)){
    print(paste("Invalid date format for group_id:", group_id, "date:", date_str))
    next
  }


  print(paste("base_date:", base_date))
  print(paste("class(base_date):", class(base_date)))
  start_date <- format(lubridate::add_with_rollback(base_date, -months(6)), "%Y-%m-%d")
  end_date   <- format(lubridate::add_with_rollback(base_date,  months(6)), "%Y-%m-%d")
  print(paste("start_date:", start_date))
  print(paste("end_date:", end_date))
  print(paste("Date range for group_id", group_id, ":", start_date, "to", end_date))
  
  # 鉱山ポリゴンのXY座標（WGS84）から範囲を算出
  bbox <- st_bbox(polygon1)
  xmin <- bbox$xmin; xmax <- bbox$xmax; ymin <- bbox$ymin; ymax <- bbox$ymax
  x_mean <- (xmax + xmin) / 2; y_mean <- (ymax + ymin) / 2
  UTM_zone <- floor((x_mean + 180) / 6) + 1
  crs_UTM <- paste0("+proj=utm +zone=", UTM_zone, " +datum=WGS84 +units=m +no_defs")
  
  polygon1_UTM <- st_transform(polygon1, crs = crs_UTM)
  
  # AOIの作成（中心座標から±10000mの領域）
  centroid <- st_centroid(polygon1_UTM)
  coords <- st_coordinates(centroid)
  x_centroid <- coords[1]; y_centroid <- coords[2]
  xmin1 <- x_centroid - 10000; xmax1 <- x_centroid + 10000
  ymin1 <- y_centroid - 10000; ymax1 <- y_centroid + 10000
  AOI <- st_polygon(list(cbind(
    c(xmin1, xmax1, xmax1, xmin1, xmin1),
    c(ymin1, ymin1, ymax1, ymax1, ymin1)
  )))
  AOI <- st_sfc(AOI, crs = crs_UTM)
  AOI <- st_transform(AOI, crs = st_crs(polygon))  # AOIをWGS84に再変換
  region <- ee$Geometry$Rectangle(st_bbox(AOI))

  ### 画像検索（指定期間・領域・必要なバンド、雲被覆率10%未満）
  LC08 <- ee$ImageCollection("COPERNICUS/S2_SR")$
    filterDate(start_date, end_date)$
    filterBounds(region)$
    filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 10))$
    select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12','SCL'))
  
  n <- LC08$size()$getInfo()
  if(n == 0){
    print(paste("No images found for polygon", polygon1$group_id, "in", start_date, "to", end_date))
    next
  }
  
  ### マスク条件に基づく画像コレクションの作成
  createMaskedImage <- function(img, mask_conditions){
    scl <- img$select("SCL")
    mask <- scl$neq(1)  # データなしを除外
    for(condition in mask_conditions){
      mask <- mask$And(scl$neq(condition))
    }
    return(img$updateMask(mask))
  }
  
  # ===== 期間内の月ごとに雲量が最も少ない画像のみエクスポート =====
  months_seq <- seq.Date(
    from = as.Date(start_date),
    to   = as.Date(end_date),
    by   = "month"
  )
  months_seq <- unique(format(months_seq, "%Y-%m"))  # 年月でユニーク化

  for (month_str in months_seq) {
    start_date_month <- paste0(month_str, "-01")
    # 月末日を取得
    end_date_month <- as.character(ymd(start_date_month) + months(1) - days(1))
    # NAチェック
    if (is.na(start_date_month) || is.na(end_date_month)) {
      print(paste("Invalid date for month", month_str, "skipping..."))
      next
    }
    print(paste("Processing best image for month:", start_date_month, "to", end_date_month))

    # 1. Sentinel-2画像コレクション（雲被覆率10%未満）
    LC08_month <- ee$ImageCollection("COPERNICUS/S2_SR")$
      filterDate(start_date_month, end_date_month)$
      filterBounds(region)$
      filter(ee$Filter$lte("CLOUDY_PIXEL_PERCENTAGE", 10))$
      select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12','SCL'))

    n_month <- LC08_month$size()$getInfo()
    if (n_month == 0) {
      print(paste("No images found for polygon", polygon1$group_id, "in", start_date_month, "to", end_date_month))
      next
    }

    # 2. SCLバンドでマスク
    maskWithSCL <- function(img) {
      scl <- img$select("SCL")
      mask <- scl$neq(1)$And(scl$neq(11))$And(scl$neq(3))$And(scl$neq(8))$And(scl$neq(9))$And(scl$neq(10))
      img$updateMask(mask)
    }
    LC08_masked_month <- LC08_month$map(maskWithSCL)

    # 3. 有効画素数で最良画像を選択
    validPixelCount <- function(img) {
      validPixels <- img$reduceRegion(
        reducer = ee$Reducer$count(),
        geometry = region,
        scale = 100,
        bestEffort = TRUE
      )$get("B1")
      img$set("valid_pixel_count", validPixels)
    }
    LC08_with_valid_count <- LC08_masked_month$map(validPixelCount)
    best_masked_image <- LC08_with_valid_count$sort("valid_pixel_count", FALSE)$first()

    # 4. 選択画像のマスク前画像を取得
    best_unmasked_image <- LC08_month$filter(ee$Filter$eq("system:index", best_masked_image$get("system:index")))$first()
    # 撮影日を取得
    best_image_date_str <- "nodate"
    tryCatch({
      product_id <- best_unmasked_image$get("PRODUCT_ID")$getInfo()
      if (!is.null(product_id) && !is.na(product_id)) {
        date_match <- regexpr("\\d{8}", product_id)
        if (date_match > 0) {
          best_image_date_str <- substr(product_id, date_match, date_match + 7)
        }
      }
    }, error = function(e) {})
    if (best_image_date_str == "nodate") {
      tryCatch({
        image_date <- best_unmasked_image$get("system:time_start")$getInfo()
        if (!is.null(image_date) && !is.na(image_date)) {
          best_image_date_str <- format(as.Date(as.numeric(image_date) / 1000, origin = "1970-01-01"), "%Y%m%d")
        }
      }, error = function(e) {})
    }
    best_unmasked_image <- best_unmasked_image$toUint16()

    # ファイル名例: Sentinel2_Best_Image_polygon1234-202101-20210115
    filename_best_unmasked <- paste0("Sentinel2_Best_Image_polygon", polygon1$group_id, "-", gsub("-", "", month_str), "-", best_image_date_str)
    task_best_unmasked <- ee_image_to_drive(
      image = best_unmasked_image,
      description = filename_best_unmasked,
      fileFormat = "GEO_TIFF",
      fileNamePrefix = filename_best_unmasked,
      timePrefix = FALSE,
      folder = "Sentinel-2_Best_Image_csv",
      region = region,
      scale = 10
    )
    task_best_unmasked$start()
  }
}
