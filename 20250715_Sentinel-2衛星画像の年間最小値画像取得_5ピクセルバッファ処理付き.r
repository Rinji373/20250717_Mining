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
#polygon <- st_read(dsn="F:/2024_hirora_Toyota6/2023_hirota/Polygon/Polygon_Bare_20250715/20250715_画像ダウンロード/20240709_all_overlapping_polygons_unique.shp")
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
#5PixelBuffer付_年間最小値合成画像の作成とダウンロード
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
  start_date <- "2021-01-01"
  end_date <- "2021-12-31"
  

  ### 画像検索（指定期間・領域・必要なバンド、雲被覆率20%未満）
  LC08 <- ee$ImageCollection("COPERNICUS/S2_SR")$
    filterDate(start_date, end_date)$
    filterBounds(region)$
    select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12','SCL'))
  
  n <- LC08$size()$getInfo()
  if(n == 0){
    print(paste("No images found for polygon", polygon1$group_id, "in", start_date, "to", end_date))
    next
  }
  
  #### 雲マスク前の画像から最小値合成画像
  composite_unmasked <- LC08$select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12'))$min()
 
  ### マスク条件に基づく画像コレクションの作成（5ピクセルバッファ付き）
  createMaskedImage <- function(img, mask_conditions){
    scl <- img$select("SCL")
    mask <- scl$neq(1)  # データなしを除外
    for(condition in mask_conditions){
      mask <- mask$And(scl$neq(condition))
    }
    # 5ピクセルバッファを適用してマスクを拡大
    # focal_minは各ピクセルの周囲の最小値を取るため、マスクを拡大する効果がある
    buffered_mask <- mask$focal_min(5, "circle", "pixels")
    return(img$updateMask(buffered_mask))
  }
  
  # 画像①～⑤の作成
  LC08_masked_1 <- LC08$map(function(img) createMaskedImage(img, c(11, 3, 8, 9, 10)))  # 画像①
  LC08_masked_2 <- LC08$map(function(img) createMaskedImage(img, c(3, 8, 9, 10)))      # 画像②
  LC08_masked_3 <- LC08$map(function(img) createMaskedImage(img, c(8, 9, 10)))         # 画像③
  LC08_masked_4 <- LC08$map(function(img) createMaskedImage(img, c(8, 9)))             # 画像④
  LC08_masked_5 <- LC08$map(function(img) createMaskedImage(img, c()))                 # 画像⑤
  
  # 各画像の年間最小値合成画像を作成
  composite_1 <- LC08_masked_1$select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12'))$min()
  composite_2 <- LC08_masked_2$select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12'))$min()
  composite_3 <- LC08_masked_3$select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12'))$min()
  composite_4 <- LC08_masked_4$select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12'))$min()
  composite_5 <- LC08_masked_5$select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12'))$min()
  
  # ===== 欠損値補完 =====
  # composite_1の欠損値をcomposite_2で補完
  final_composite <- composite_1$unmask(composite_2)
  # composite_2で補完されなかった欠損値をcomposite_3で補完
  final_composite <- final_composite$unmask(composite_3)
  # composite_3で補完されなかった欠損値をcomposite_4で補完
  final_composite <- final_composite$unmask(composite_4)
  # composite_4で補完されなかった欠損値をcomposite_5で補完
  final_composite <- final_composite$unmask(composite_5)
  # composite_5で補完されなかった欠損値をマスク処理をしていない画像で補完
  final_composite <- final_composite$unmask(composite_unmasked)
  
   #年間SCL Bandの作製
  ### SCLバンド画像の検索
  SCL_images <- ee$ImageCollection("COPERNICUS/S2_SR")$
    filterDate(start_date, end_date)$
    filterBounds(region)$
    select("SCL")
 
  n <- SCL_images$size()$getInfo()
  if(n == 0){
    print(paste("No SCL images found for polygon", polygon1$group_id, "in", start_date, "to", end_date))
    next
  }

  # ===== SCLバンドの処理（5ピクセルバッファ付き） =====
  createMaskedSCLImage <- function(img, mask_conditions, new_value){
    scl <- img$select("SCL")
    mask <- scl$neq(1)  # データなしを除外
    for(condition in mask_conditions){
      mask <- mask$And(scl$neq(condition))
    }
    # 5ピクセルバッファを適用してマスクを拡大
    # focal_minは各ピクセルの周囲の最小値を取るため、マスクを拡大する効果がある
    buffered_mask <- mask$focal_min(5, "circle", "pixels")
    return(scl$updateMask(buffered_mask)$multiply(0)$add(new_value))  # 値を変更
  }
  
  # SCL画像①～⑤の作成
  SCL_masked_1 <- SCL_images$map(function(img) createMaskedSCLImage(img, c(11, 3, 8, 9, 10), 7))  # SCL画像①'
  SCL_masked_2 <- SCL_images$map(function(img) createMaskedSCLImage(img, c(3, 8, 9, 10), 11))     # SCL画像②'
  SCL_masked_3 <- SCL_images$map(function(img) createMaskedSCLImage(img, c(8, 9, 10), 3))         # SCL画像③'
  SCL_masked_4 <- SCL_images$map(function(img) createMaskedSCLImage(img, c(8, 9), 10))            # SCL画像④'
  SCL_masked_5 <- SCL_images$map(function(img) createMaskedSCLImage(img, c(), 9))                 # SCL画像⑤'
  SCL_unmasked <- SCL_images$map(function(img) img$multiply(0)$add(1))                            # マスク前のSCL画像'

  # 各SCL画像の月間最小値合成画像を作成
  composite_SCL_1 <- SCL_masked_1$min()
  composite_SCL_2 <- SCL_masked_2$min()
  composite_SCL_3 <- SCL_masked_3$min()
  composite_SCL_4 <- SCL_masked_4$min()
  composite_SCL_5 <- SCL_masked_5$min()
  composite_SCL_unmasked <- SCL_unmasked$min()
  
  # ===== 欠損値補完 =====
  # composite_SCL_1の欠損値をcomposite_SCL_2で補完
  final_SCL_composite <- composite_SCL_1$unmask(composite_SCL_2)
  # composite_SCL_2で補完されなかった欠損値をcomposite_SCL_3で補完
  final_SCL_composite <- final_SCL_composite$unmask(composite_SCL_3)
  # composite_SCL_3で補完されなかった欠損値をcomposite_SCL_4で補完
  final_SCL_composite <- final_SCL_composite$unmask(composite_SCL_4)
  # composite_SCL_4で補完されなかった欠損値をcomposite_SCL_5で補完
  final_SCL_composite <- final_SCL_composite$unmask(composite_SCL_5)
  # composite_SCL_5で補完されなかった欠損値をマスク処理をしていない画像で補完
  final_SCL_composite <- final_SCL_composite$unmask(composite_SCL_unmasked)

  final_composite <- final_composite$toFloat()
  final_SCL_composite <- final_SCL_composite$toFloat()
  final_combined_composite <- final_composite$addBands(final_SCL_composite)
  
  # ===== 最終的な補完画像にSCL補完画像を追加 =====
  final_combined_composite <- final_composite$addBands(final_SCL_composite)

  # エクスポート時のファイル名に月情報を追加
  filename_combined <- paste("Sentinel-2_5Pixelbuffer_Yearly_MinComposite_polygon", polygon1$group_id,
                           gsub("-", "", start_date),
                           gsub("-", "", end_date), sep = "_")
  task_combined <- ee_image_to_drive(
    image = final_combined_composite,
    description = filename_combined,
    fileFormat = "GEO_TIFF",
    fileNamePrefix = filename_combined,
    timePrefix = FALSE,
    folder = "Sentinel-2_5Pixelbuffers_Yearly_MinComposite",
    region = region,
    scale = 10
  )
  task_combined$start()
}

################################################################################################
#10PixelBuffer付_年間最小値合成画像の作成とダウンロード
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
  start_date <- "2021-01-01"
  end_date <- "2021-12-31"
  

  ### 画像検索（指定期間・領域・必要なバンド、雲被覆率20%未満）
  LC08 <- ee$ImageCollection("COPERNICUS/S2_SR")$
    filterDate(start_date, end_date)$
    filterBounds(region)$
    select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12','SCL'))
  
  n <- LC08$size()$getInfo()
  if(n == 0){
    print(paste("No images found for polygon", polygon1$group_id, "in", start_date, "to", end_date))
    next
  }
  
  #### 雲マスク前の画像から最小値合成画像
  composite_unmasked <- LC08$select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12'))$min()
 
  ### マスク条件に基づく画像コレクションの作成（5ピクセルバッファ付き）
  createMaskedImage <- function(img, mask_conditions){
    scl <- img$select("SCL")
    mask <- scl$neq(1)  # データなしを除外
    for(condition in mask_conditions){
      mask <- mask$And(scl$neq(condition))
    }
    # 5ピクセルバッファを適用してマスクを拡大
    # focal_minは各ピクセルの周囲の最小値を取るため、マスクを拡大する効果がある
    buffered_mask <- mask$focal_min(10, "circle", "pixels")
    return(img$updateMask(buffered_mask))
  }
  
  # 画像①～⑤の作成
  LC08_masked_1 <- LC08$map(function(img) createMaskedImage(img, c(11, 3, 8, 9, 10)))  # 画像①
  LC08_masked_2 <- LC08$map(function(img) createMaskedImage(img, c(3, 8, 9, 10)))      # 画像②
  LC08_masked_3 <- LC08$map(function(img) createMaskedImage(img, c(8, 9, 10)))         # 画像③
  LC08_masked_4 <- LC08$map(function(img) createMaskedImage(img, c(8, 9)))             # 画像④
  LC08_masked_5 <- LC08$map(function(img) createMaskedImage(img, c()))                 # 画像⑤
  
  # 各画像の年間最小値合成画像を作成
  composite_1 <- LC08_masked_1$select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12'))$min()
  composite_2 <- LC08_masked_2$select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12'))$min()
  composite_3 <- LC08_masked_3$select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12'))$min()
  composite_4 <- LC08_masked_4$select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12'))$min()
  composite_5 <- LC08_masked_5$select(c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12'))$min()
  
  # ===== 欠損値補完 =====
  # composite_1の欠損値をcomposite_2で補完
  final_composite <- composite_1$unmask(composite_2)
  # composite_2で補完されなかった欠損値をcomposite_3で補完
  final_composite <- final_composite$unmask(composite_3)
  # composite_3で補完されなかった欠損値をcomposite_4で補完
  final_composite <- final_composite$unmask(composite_4)
  # composite_4で補完されなかった欠損値をcomposite_5で補完
  final_composite <- final_composite$unmask(composite_5)
  # composite_5で補完されなかった欠損値をマスク処理をしていない画像で補完
  final_composite <- final_composite$unmask(composite_unmasked)
  
   #年間SCL Bandの作製
  ### SCLバンド画像の検索
  SCL_images <- ee$ImageCollection("COPERNICUS/S2_SR")$
    filterDate(start_date, end_date)$
    filterBounds(region)$
    select("SCL")
 
  n <- SCL_images$size()$getInfo()
  if(n == 0){
    print(paste("No SCL images found for polygon", polygon1$group_id, "in", start_date, "to", end_date))
    next
  }

  # ===== SCLバンドの処理（5ピクセルバッファ付き） =====
  createMaskedSCLImage <- function(img, mask_conditions, new_value){
    scl <- img$select("SCL")
    mask <- scl$neq(1)  # データなしを除外
    for(condition in mask_conditions){
      mask <- mask$And(scl$neq(condition))
    }
    # 5ピクセルバッファを適用してマスクを拡大
    # focal_minは各ピクセルの周囲の最小値を取るため、マスクを拡大する効果がある
    buffered_mask <- mask$focal_min(10, "circle", "pixels")
    return(scl$updateMask(buffered_mask)$multiply(0)$add(new_value))  # 値を変更
  }
  
  # SCL画像①～⑤の作成
  SCL_masked_1 <- SCL_images$map(function(img) createMaskedSCLImage(img, c(11, 3, 8, 9, 10), 7))  # SCL画像①'
  SCL_masked_2 <- SCL_images$map(function(img) createMaskedSCLImage(img, c(3, 8, 9, 10), 11))     # SCL画像②'
  SCL_masked_3 <- SCL_images$map(function(img) createMaskedSCLImage(img, c(8, 9, 10), 3))         # SCL画像③'
  SCL_masked_4 <- SCL_images$map(function(img) createMaskedSCLImage(img, c(8, 9), 10))            # SCL画像④'
  SCL_masked_5 <- SCL_images$map(function(img) createMaskedSCLImage(img, c(), 9))                 # SCL画像⑤'
  SCL_unmasked <- SCL_images$map(function(img) img$multiply(0)$add(1))                            # マスク前のSCL画像'

  # 各SCL画像の月間最小値合成画像を作成
  composite_SCL_1 <- SCL_masked_1$min()
  composite_SCL_2 <- SCL_masked_2$min()
  composite_SCL_3 <- SCL_masked_3$min()
  composite_SCL_4 <- SCL_masked_4$min()
  composite_SCL_5 <- SCL_masked_5$min()
  composite_SCL_unmasked <- SCL_unmasked$min()
  
  # ===== 欠損値補完 =====
  # composite_SCL_1の欠損値をcomposite_SCL_2で補完
  final_SCL_composite <- composite_SCL_1$unmask(composite_SCL_2)
  # composite_SCL_2で補完されなかった欠損値をcomposite_SCL_3で補完
  final_SCL_composite <- final_SCL_composite$unmask(composite_SCL_3)
  # composite_SCL_3で補完されなかった欠損値をcomposite_SCL_4で補完
  final_SCL_composite <- final_SCL_composite$unmask(composite_SCL_4)
  # composite_SCL_4で補完されなかった欠損値をcomposite_SCL_5で補完
  final_SCL_composite <- final_SCL_composite$unmask(composite_SCL_5)
  # composite_SCL_5で補完されなかった欠損値をマスク処理をしていない画像で補完
  final_SCL_composite <- final_SCL_composite$unmask(composite_SCL_unmasked)

  final_composite <- final_composite$toFloat()
  final_SCL_composite <- final_SCL_composite$toFloat()
  final_combined_composite <- final_composite$addBands(final_SCL_composite)
  
  # ===== 最終的な補完画像にSCL補完画像を追加 =====
  final_combined_composite <- final_composite$addBands(final_SCL_composite)

  # エクスポート時のファイル名に月情報を追加
  filename_combined <- paste("Sentinel-2_10Pixelbuffer_Yearly_MinComposite_polygon", polygon1$group_id,
                           gsub("-", "", start_date),
                           gsub("-", "", end_date), sep = "_")
  task_combined <- ee_image_to_drive(
    image = final_combined_composite,
    description = filename_combined,
    fileFormat = "GEO_TIFF",
    fileNamePrefix = filename_combined,
    timePrefix = FALSE,
    folder = "Sentinel-2_10Pixelbuffer_Yearly_MinComposite",
    region = region,
    scale = 10
  )
  task_combined$start()
}
