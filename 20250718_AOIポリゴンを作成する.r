# Google Driveパッケージ、sf、lubridateの読み込み
library("googledrive")
library("sf")
library(lubridate)

# 鉱山ポリゴンの読み込み（SHAPE形式）
polygon <- st_read(dsn="F:/2024_hirora_Toyota6/2023_hirota/Polygon/Polygon_Bare_20250715/20250715_画像ダウンロード/20240709_all_overlapping_polygons_unique.shp")
#polygon <- st_read(dsn="C:/Users/casakuma-lab-04-std/Downloads/test/test.shp")

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
  AOI_list[[i]] <- AOI
}

# AOIをまとめてsfオブジェクト化
AOI_sf <- st_sf(geometry = do.call(c, AOI_list), crs = st_crs(polygon))

# shpファイルとして保存
st_write(AOI_sf, "F:/2024_hirora_Toyota6/2023_hirota/Polygon/Polygon_Bare_20250715/20250715_画像ダウンロード/Image_AOI.shp")