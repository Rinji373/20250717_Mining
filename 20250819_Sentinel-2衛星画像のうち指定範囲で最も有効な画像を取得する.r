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
polygon <- st_read(dsn="F:/2024_hirora_Toyota6/2023_hirota/Polygon/Polygon_Bare_20250715/20250715_画像ダウンロード/20240709_all_overlapping_polygons_unique.shp")
#polygon <- st_read(dsn="C:/Users/casakuma-lab-04-std/Downloads/test.shp")

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

#################################################################################
#最良画像の選択
for(i in 1:nrow(polygon)){
  # i番目の鉱山ポリゴンを抽出し、AOIを設定
  polygon1 <- polygon[i, ]
  print(paste("Processing polygon:", polygon1$group_id))
  
  # 鉱山ポリゴンのXY座標（WGS84）から範囲を算出
  bbox <- st_bbox(polygon1)
  xmin <- bbox$xmin; xmax <- bbox$xmax; ymin <- bbox$ymin; ymax <- bbox$ymax
  x_mean <- (xmax + xmin) / 2; y_mean <- (ymax + ymin) / 2
  UTM_zone <- floor((x_mean + 180) / 6) + 1
  crs_UTM <- paste0("+proj=utm +zone=", UTM_zone, " +datum=WGS84 +units=m +no_defs")
  
  polygon1_UTM <- st_transform(polygon1, crs = crs_UTM)
  
  # AOIの作成（中心座標から±20000mの領域）
  centroid <- st_centroid(polygon1_UTM)
  coords <- st_coordinates(centroid)
  x_centroid <- coords[1]; y_centroid <- coords[2]
  xmin1 <- x_centroid - 20000; xmax1 <- x_centroid + 20000
  ymin1 <- y_centroid - 20000; ymax1 <- y_centroid + 20000
  AOI <- st_polygon(list(cbind(
    c(xmin1, xmax1, xmax1, xmin1, xmin1),
    c(ymin1, ymin1, ymax1, ymax1, ymin1)
  )))
  AOI <- st_sfc(AOI, crs = crs_UTM)
  AOI <- st_transform(AOI, crs = st_crs(polygon))  # AOIをWGS84に再変換
  region <- ee$Geometry$Rectangle(st_bbox(AOI))
  # 年間で最も有効な画像を1枚だけエクスポート
  start_date <- "2021-01-01"
  end_date <- "2021-12-31"
  print(paste("Processing year:", start_date, "to", end_date))

  # 1. Sentinel-2衛星画像を検索（雲被覆率10%未満）
  LC08 <- ee$ImageCollection("COPERNICUS/S2_SR")$
    filterDate(start_date, end_date)$
    filterBounds(region)$
    filter(ee$Filter$lte("CLOUDY_PIXEL_PERCENTAGE", 10))$
    select(c('B2','B3','B4','SCL'))

  n <- LC08$size()$getInfo()
  if(n == 0){
    print(paste("No images found for polygon", polygon1$group_id, "in", start_date, "to", end_date))
    next
  }

  # 2. SCLバンドを用いたマスク処理
  maskWithSCL <- function(img){
    scl <- img$select("SCL")
    mask <- scl$neq(1)$And(scl$neq(11))$And(scl$neq(3))$And(scl$neq(8))$And(scl$neq(9))$And(scl$neq(10))
    return(img$updateMask(mask))
  }

  LC08_masked <- LC08$map(maskWithSCL)

  # 3. 有効な画像を取得（最も欠損が少ない画像を選択）
  validPixelCount <- function(img){
    validPixels <- img$reduceRegion(
      reducer = ee$Reducer$count(),
      geometry = region,
      scale = 100,
      bestEffort = TRUE
    )$get("B2")  # B2バンドで有効画素数を取得
    return(img$set("valid_pixel_count", validPixels))
  }

  LC08_with_valid_count <- LC08_masked$map(validPixelCount)
  best_masked_image <- LC08_with_valid_count$sort("valid_pixel_count", FALSE)$first()

  # 4. 選択した画像のマスク処理前の画像を取得
  best_unmasked_image <- LC08$filter(ee$Filter$eq("system:index", best_masked_image$get("system:index")))$first()

  # すべてのバンドを UInt16 型に変換
  best_unmasked_image <- best_unmasked_image$toUint16()

  filename_best_unmasked <- paste0("Sentinel2_Best_Image_polygon", polygon1$group_id, "-best")
  task_best_unmasked <- ee_image_to_drive(
    image = best_unmasked_image,
    description = filename_best_unmasked,
    fileFormat = "GEO_TIFF",
    fileNamePrefix = filename_best_unmasked,
    timePrefix = FALSE,
    folder = "Sentinel-2_Year_Best_Image",
    region = region,
    scale = 10
  )
  task_best_unmasked$start()
}
