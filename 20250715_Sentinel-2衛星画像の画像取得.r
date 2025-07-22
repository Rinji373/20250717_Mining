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
polygon <- st_read(dsn="C:/Users/casakuma-lab-04-std/Downloads/test/test.shp")

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
#年間最小値合成画像の作成とダウンロード
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
  start_date <- "2021-01-01"
  end_date <- "2021-12-31"
  # end_date <- "2021-12-31"

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
  
  # ===== 期間内のすべての画像（B1-B12, SCL）を取得しエクスポート =====
  all_images <- LC08$toList(LC08$size())
# ...既存のコード...
for(j in 0:(n-1)){
  single_image <- ee$Image(all_images$get(j))
  # 全てのバンドをFLOAT型に変換
  scl_float <- single_image$select("SCL")$toFloat()
  bands_float <- single_image$select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12'))$toFloat()
  export_image <- bands_float$addBands(scl_float)
  filename_single <- paste("Sentinel-2_Single_Image_polygon", polygon1$group_id,
                           gsub("-", "", start_date),
                           gsub("-", "", end_date),
                           sprintf("img%03d", j+1), sep = "_")
  task_single <- ee_image_to_drive(
    image = export_image,
    description = filename_single,
    fileFormat = "GEO_TIFF",
    fileNamePrefix = filename_single,
    timePrefix = FALSE,
    folder = "Sentinel-2_Single_Image",
    region = region,
    scale = 10
  )
  task_single$start()}
}
