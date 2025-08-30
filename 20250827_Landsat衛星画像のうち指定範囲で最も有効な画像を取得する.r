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
polygon <- st_read(dsn="F:/2024_hirora_Toyota6/2023_hirota/Polygon/Polygon_Bare_20250819_2021ver/polygon_bare20250819_2021ver.shp")
#polygon <- st_read(dsn="C:/Users/casakuma-lab-04-std/Downloads/test3380-3381.shp")

# フォルダー名
folder_name <- "Landsat-8_Monthly_Best_Image"

# フォルダーの存在確認
folder <- drive_find(type = "folder", pattern = paste0("^", folder_name, "$"))

if (nrow(folder) == 0) {
  # フォルダーが存在しなければ作成
  folder <- drive_mkdir(folder_name)
  message("フォルダーを新規作成しました: ", folder_name)
} else {
  message("既存フォルダーを使用します: ", folder_name)
}

#################################################################################
#最良画像の選択
# 対象年を指定
target_year <- 2021

for(i in 1:nrow(polygon)){
  polygon1 <- polygon[i, ]
  polygon_id <- as.character(polygon1$polygon_id)
  print(paste("Processing polygon:", polygon_id))
  # 2. 既存ファイルにpolygon_idが含まれていればスキップ
  if (polygon_id %in% existing_polygon_ids) {
    print(paste("Already processed polygon_id:", polygon_id))
    next
  }

  # AOI作成
  bbox <- st_bbox(polygon1)
  xmin <- bbox$xmin; xmax <- bbox$xmax; ymin <- bbox$ymin; ymax <- bbox$ymax
  x_mean <- (xmax + xmin) / 2; y_mean <- (ymax + ymin) / 2
  UTM_zone <- floor((x_mean + 180) / 6) + 1
  crs_UTM <- paste0("+proj=utm +zone=", UTM_zone, " +datum=WGS84 +units=m +no_defs")
  polygon1_UTM <- st_transform(polygon1, crs = crs_UTM)
  centroid <- st_centroid(st_geometry(polygon1_UTM))
  coords <- st_coordinates(centroid)
  x_centroid <- coords[1]; y_centroid <- coords[2]
  xmin1 <- x_centroid - 10000; xmax1 <- x_centroid + 10000
  ymin1 <- y_centroid - 10000; ymax1 <- y_centroid + 10000
  AOI <- st_polygon(list(cbind(
    c(xmin1, xmax1, xmax1, xmin1, xmin1),
    c(ymin1, ymin1, ymax1, ymax1, ymin1)
  )))
  AOI <- st_sfc(AOI, crs = crs_UTM)
  AOI <- st_transform(AOI, crs = st_crs(polygon))
  region <- ee$Geometry$Rectangle(st_bbox(AOI))

  for(month in 1:12){
    start_date <- sprintf("%d-%02d-01", target_year, month)
    end_date <- as.character(as.Date(start_date) + months(1) - 1)
    print(paste("Processing month:", start_date, "to", end_date))

    LC08 <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$
      filterDate(start_date, end_date)$
      filterBounds(region)$
      filter(ee$Filter$lt('CLOUD_COVER', 10))$
      select(c('SR_B1','SR_B2','SR_B3','SR_B4','SR_B5','SR_B7','QA_PIXEL'))

    n <- LC08$size()$getInfo()
    if(n == 0){
      print(paste("No images found for polygon", polygon1$polygon_id, "in", start_date, "to", end_date))
      next
    }

    maskWithQA <- function(img){
      qa <- img$select("QA_PIXEL")
      dilated_cloud <- qa$bitwiseAnd(2L)$eq(0)
      cirrus <- qa$bitwiseAnd(4L)$eq(0)
      cloud <- qa$bitwiseAnd(8L)$eq(0)
      cloud_shadow <- qa$bitwiseAnd(16L)$eq(0)
      mask <- dilated_cloud$And(cirrus)$And(cloud)$And(cloud_shadow)
      return(img$updateMask(mask))
    }
    LC08_masked <- LC08$map(maskWithQA)

    validPixelCount <- function(img){
      validPixels <- img$reduceRegion(
        reducer = ee$Reducer$count(),
        geometry = region,
        scale = 100,
        bestEffort = TRUE
      )$get("SR_B2")
      return(img$set("valid_pixel_count", validPixels))
    }
    LC08_with_valid_count <- LC08_masked$map(validPixelCount)
    best_masked_image <- LC08_with_valid_count$sort("valid_pixel_count", FALSE)$first()

    best_unmasked_image <- LC08$filter(ee$Filter$eq("system:index", best_masked_image$get("system:index")))$first()
    
    # AOI内の雲量率を計算
    qa <- best_unmasked_image$select("QA_PIXEL")
    cloud_mask <- qa$bitwiseAnd(8L)$neq(0)
    cloud_shadow_mask <- qa$bitwiseAnd(16L)$neq(0)
    cloud_or_shadow <- cloud_mask$Or(cloud_shadow_mask)
    n_cloud <- cloud_or_shadow$reduceRegion(
      reducer = ee$Reducer$sum(),
      geometry = region,
      scale = 30,
      bestEffort = TRUE
    )$getInfo()
    n_cloud <- n_cloud[[1]]
    n_total <- best_unmasked_image$select("SR_B2")$reduceRegion(
      reducer = ee$Reducer$count(),
      geometry = region,
      scale = 30,
      bestEffort = TRUE
    )$getInfo()
    n_total <- n_total[[1]]
    cloud_percent <- ifelse(is.null(n_cloud) || is.null(n_total) || n_total == 0, 100, 100 * n_cloud / n_total)
    print(paste("AOI内の雲量率:", round(cloud_percent,2), "%"))
    if(cloud_percent >= 10){
      print("AOI内の雲量が10%以上のためスキップします")
      next
    }

    best_unmasked_image <- best_unmasked_image$toUint16()
    filename_best_unmasked <- paste0("Landsat8_Best_Image_polygon", polygon1$polygon_id, "_", target_year, sprintf("%02d", month), "_best")
    task_best_unmasked <- ee_image_to_drive(
      image = best_unmasked_image,
      description = filename_best_unmasked,
      fileFormat = "GEO_TIFF",
      fileNamePrefix = filename_best_unmasked,
      timePrefix = FALSE,
      folder = folder_name,
      region = region,
      scale = 10
    )
    task_best_unmasked$start()
  }
}

#途中から実行する際に一度実行するコード##################################

#複数のフォルダを一つにまとめる
folders <- drive_find(type = "folder", pattern = folder_name)
for (j in 2:nrow(folders)) {
  folder_id <- as_id(folders[j, ])
  # 存在確認
  folder_info <- tryCatch(drive_get(folder_id), error = function(e) NULL)
  if (is.null(folder_info) || nrow(folder_info) == 0) {
    warning("フォルダが存在しないためスキップ: ", as.character(folder_id))
    next
  }
  files <- drive_ls(path = folder_id)
  if (nrow(files) > 0) {
    for (k in 1:nrow(files)) {
      file_id <- as_id(files$id[k])
      file_info <- tryCatch(drive_get(file_id), error = function(e) NULL)
      if (!is.null(file_info) && nrow(file_info) == 1) {
        drive_mv(file_id, path = main_folder_id)
      } else {
        warning("ファイルIDが一意でない/存在しないためスキップ: ", files$name[k])
      }
    }
  }
  tryCatch(drive_rm(folder_id), error = function(e) warning("削除失敗: ", as.character(folder_id)))
}

folders <- drive_find(type = "folder", pattern = folder_name)
for (j in 2:nrow(folders)) {
  tryCatch({
    folder_id <- as_id(folders[j, ])
    folder_info <- tryCatch(drive_get(folder_id), error = function(e) NULL)
    if (is.null(folder_info) || nrow(folder_info) == 0) {
      warning("フォルダが存在しないためスキップ: ", as.character(folder_id))
      next
    }
    files <- tryCatch(drive_ls(path = folder_id), error = function(e) NULL)
    if (!is.null(files) && nrow(files) > 0) {
      for (k in 1:nrow(files)) {
        file_id <- as_id(files$id[k])
        file_info <- tryCatch(drive_get(file_id), error = function(e) NULL)
        if (!is.null(file_info) && nrow(file_info) == 1) {
          tryCatch(
            drive_mv(file_id, path = main_folder_id),
            error = function(e) warning("ファイル移動失敗: ", files$name[k])
          )
        } else {
          warning("ファイルIDが一意でない/存在しないためスキップ: ", files$name[k])
        }
      }
    }
    tryCatch(drive_rm(folder_id), error = function(e) warning("削除失敗: ", as.character(folder_id)))
  }, error = function(e) {
    warning("ループ内で予期せぬエラー: ", conditionMessage(e))
    next
  })
}

# 1. 重複フォルダとそのファイルをリストアップ
folders <- drive_find(type = "folder", pattern = folder_name)
main_folder_id <- as_id(folders[1, ])
if (nrow(folders) > 1) {
  for (j in 2:nrow(folders)) {
    folder_id <- as_id(folders[j, ])
    files <- tryCatch(drive_ls(path = folder_id), error = function(e) NULL)
    # 2. ファイルをまとめて移動
    if (!is.null(files) && nrow(files) > 0) {
      for (k in 1:nrow(files)) {
        file_id <- as_id(files$id[k])
        file_info <- tryCatch(drive_get(file_id), error = function(e) NULL)
        if (!is.null(file_info) && nrow(file_info) == 1) {
          tryCatch(
            drive_mv(file_id, path = main_folder_id),
            error = function(e) warning("ファイル移動失敗: ", files$name[k])
          )
        } else {
          warning("ファイルIDが一意でない/存在しないためスキップ: ", files$name[k])
        }
      }
    }
  }
  # 3. すべての重複フォルダを再取得し、空のものだけ削除
  folders <- drive_find(type = "folder", pattern = folder_name)
  for (j in 2:nrow(folders)) {
    folder_id <- as_id(folders[j, ])
    files <- tryCatch(drive_ls(path = folder_id), error = function(e) NULL)
    if (is.null(files) || nrow(files) == 0) {
      tryCatch(drive_rm(folder_id), error = function(e) warning("削除失敗: ", as.character(folder_id)))
    } else {
      warning("フォルダが空でないため削除スキップ: ", as.character(folder_id))
    }
  }
}

# 既存のpolygon_idを取得
existing_polygon_ids <- c()
# 1. 既存ファイル名から取得済みpolygon_idを抽出
existing_files <- drive_ls(path = folder_name)
# ファイル名例: Landsat8_Best_Image_polygon1234_202101_best.tif
existing_polygon_ids <- unique(unlist(regmatches(existing_files$name, gregexpr("(?<=_polygon)\\d+", existing_files$name, perl=TRUE))))

####################################################################
#タスクを終了させるコード
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