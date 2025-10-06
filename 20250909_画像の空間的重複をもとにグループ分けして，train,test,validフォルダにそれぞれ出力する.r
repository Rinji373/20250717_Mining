#######################################
# 必要パッケージ
library(terra)
library(sf)
library(igraph)
library(stringr)
library(fs)

# 1. 画像用入力フォルダとラベル用入力フォルダを指定
image_input_folder <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/All"   # ここを適宜変更
#image_input_folder <- "F:/2024_hirora_Toyota6/2023_hirota/Image/20250909_test"
label_input_folder <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/All_label"   # ここを適宜変更
#label_input_folder <- "F:/2024_hirora_Toyota6/2023_hirota/Image/20250909_label"

# 2. 出力ベースフォルダを指定
output_base <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024"              # ここを適宜変更

# 3. 画像ファイル名からユニークなpolygon_idリスト作成
image_files <- list.files(image_input_folder, pattern = "\\.tif$", full.names = TRUE)
get_polygon_id <- function(filename) {
  str_extract(basename(filename), "(?<=polygon)\\d+")
}
polygon_ids <- sapply(image_files, get_polygon_id)
unique_polygon_ids <- unique(polygon_ids)

# 4. polygon_idごとに一枚画像ファイルを抽出
selected_image_files <- sapply(unique_polygon_ids, function(pid) {
  files <- image_files[polygon_ids == pid]
  files[1]
})

# 5. 各画像のポリゴン取得
polys <- lapply(selected_image_files, function(f) {
  r <- rast(f)
  as.polygons(ext(r)) |> st_as_sf()
})

# 6. 重複判定行列作成
n <- length(polys)
overlap_mat <- matrix(FALSE, n, n)
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    overlap_mat[i, j] <- st_intersects(polys[[i]], polys[[j]], sparse = FALSE)[1,1]
    overlap_mat[j, i] <- overlap_mat[i, j]
  }
}
g <- graph_from_adjacency_matrix(overlap_mat, mode = "undirected")
groups <- components(g)$membership

# 7. グループを無作為にtrain40%, test30%, valid30%に分類
set.seed(123)
group_ids <- unique(groups)
n_group <- length(group_ids)
shuffled <- sample(group_ids)
n_train <- floor(n_group * 0.7)
n_test  <- floor(n_group * 0.1)
n_valid <- n_group - n_train - n_test
train_groups <- shuffled[1:n_train]
test_groups  <- shuffled[(n_train+1):(n_train+n_test)]
valid_groups <- shuffled[(n_train+n_test+1):n_group]
set_type <- rep(NA, length(groups))
set_type[groups %in% train_groups] <- "train"
set_type[groups %in% test_groups]  <- "test"
set_type[groups %in% valid_groups] <- "valid"

# 8. 画像ファイルをセットごとに分類
all_set_type <- sapply(polygon_ids, function(pid) {
  idx <- which(unique_polygon_ids == pid)
  set_type[idx]
})

# 9. ラベル画像ファイルもpolygon_idで分類
label_files <- list.files(label_input_folder, pattern = "\\.tif$", full.names = TRUE)
label_polygon_ids <- sapply(label_files, get_polygon_id)
label_set_type <- sapply(label_polygon_ids, function(pid) {
  idx <- which(unique_polygon_ids == pid)
  if (length(idx) == 0) return(NA) else set_type[idx]
})

# 10. 画像・ラベルをセットごとにコピー
for (set in c("train", "test", "valid")) {
  # 画像
  out_img_dir <- file.path(output_base, paste0(set, "_image"))
  if (!dir_exists(out_img_dir)) dir_create(out_img_dir, recurse = TRUE)
  files_to_copy_img <- image_files[all_set_type == set]
  file_copy(files_to_copy_img, out_img_dir, overwrite = TRUE)
  # ラベル
  out_lbl_dir <- file.path(output_base, paste0(set, "_label"))
  if (!dir_exists(out_lbl_dir)) dir_create(out_lbl_dir, recurse = TRUE)
  files_to_copy_lbl <- label_files[label_set_type == set]
  file_copy(files_to_copy_lbl, out_lbl_dir, overwrite = TRUE)
}

# 分類結果の確認用出力
result <- data.frame(
  image_file = basename(image_files),
  polygon_id = polygon_ids,
  set = all_set_type
)
print(result)

############################################画像とポリゴンの空間的重複を確認する
# 必要パッケージ
library(terra)
library(sf)
library(igraph)
library(stringr)
library(fs)

# 1. 画像用入力フォルダとラベル用入力フォルダを指定
image_input_folder <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat7_2000"   # ここを適宜変更
#image_input_folder <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/test_image"

# 2. 出力ベースフォルダを指定
output_base <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat7_2000_サイト統一"            # ここを適宜変更

# 3. 画像ファイル名からユニークなpolygon_idリスト作成
image_files <- list.files(image_input_folder, pattern = "\\.tif$", full.names = TRUE)
get_polygon_id <- function(filename) {
  str_extract(basename(filename), "(?<=polygon)\\d+")
}
polygon_ids <- sapply(image_files, get_polygon_id)
unique_polygon_ids <- unique(polygon_ids)

# 4. polygon_idごとに一枚画像ファイルを抽出
selected_image_files <- sapply(unique_polygon_ids, function(pid) {
  files <- image_files[polygon_ids == pid]
  files[1]
})

# 5. 各画像のポリゴン取得（CRSを付与）
polys <- lapply(selected_image_files, function(f) {
  r <- rast(f)
  poly <- as.polygons(ext(r)) |> st_as_sf()
  st_crs(poly) <- st_crs(r)  # CRSを設定
  poly
})

# 6. 鉱山ポリゴンの読み込み
mine_polygon_file <- "C:/Users/casakuma-lab-04-std/Downloads/polygon_Bare_2000-2010 3/polygon_Bare_2000-2010/polygon_Bare_2000_ダウンロード.shp" # 例
mine_poly <- st_read(mine_polygon_file)

# 画像ポリゴンのCRSに合わせて鉱山ポリゴンを変換
mine_poly <- st_transform(mine_poly, st_crs(polys[[1]]))


# 7. 鉱山ポリゴンと重複する画像ポリゴンの抽出
overlap_with_mine <- sapply(polys, function(poly) {
  any(st_intersects(poly, mine_poly, sparse = FALSE))
})

selected_mine_image_files <- selected_image_files[overlap_with_mine]

# 8. 重複画像の保存
# 代表画像のpolygon_idを取得
selected_ids <- names(selected_mine_image_files)

# そのpolygon_idを持つ全画像ファイルを抽出
all_selected_files <- image_files[polygon_ids %in% selected_ids]

# 出力フォルダにコピー
mine_output_dir <- file.path(output_base)
if (!dir_exists(mine_output_dir)) dir_create(mine_output_dir, recurse = TRUE)
file_copy(all_selected_files, mine_output_dir, overwrite = TRUE)

#########################################################検証用サイトを統一させる
#######################################
# 必要パッケージ
library(terra)
library(sf)
library(igraph)
library(stringr)
library(fs)

# 1. 入力フォルダ2つ・出力フォルダ指定
image_input_folder1 <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat7_2000_common"   # 画像フォルダ1
image_input_folder2 <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat5_2010"   # 画像フォルダ2
output_base <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat5_2010_common"          # 出力ベースフォルダ

# 2. 画像ファイル名からpolygon_id抽出（各フォルダ）
image_files1 <- list.files(image_input_folder1, pattern = "\\.tif$", full.names = TRUE)
image_files2 <- list.files(image_input_folder2, pattern = "\\.tif$", full.names = TRUE)
get_polygon_id <- function(filename) {
  str_extract(basename(filename), "(?<=polygon)\\d+")
}
polygon_ids1 <- sapply(image_files1, get_polygon_id)
polygon_ids2 <- sapply(image_files2, get_polygon_id)
unique_polygon_ids1 <- unique(polygon_ids1)
unique_polygon_ids2 <- unique(polygon_ids2)

# 3. polygon_idごとに代表画像抽出（各フォルダ）
selected_image_files1 <- sapply(unique_polygon_ids1, function(pid) {
  files <- image_files1[polygon_ids1 == pid]
  files[1]
})
selected_image_files2 <- sapply(unique_polygon_ids2, function(pid) {
  files <- image_files2[polygon_ids2 == pid]
  files[1]
})

# 4. 各代表画像のポリゴン取得
polys1 <- lapply(selected_image_files1, function(f) {
  r <- rast(f)
  poly <- as.polygons(ext(r)) |> st_as_sf()
  st_crs(poly) <- st_crs(r)
  poly
})
polys2 <- lapply(selected_image_files2, function(f) {
  r <- rast(f)
  poly <- as.polygons(ext(r)) |> st_as_sf()
  st_crs(poly) <- st_crs(r)
  poly
})

# 5. フォルダ1の画像ポリゴンとフォルダ2の画像ポリゴンの空間的重複判定
overlap_with_folder1 <- sapply(polys2, function(poly2) {
  any(sapply(polys1, function(poly1) {
    st_intersects(poly2, poly1, sparse = FALSE)[1,1]
  }))
})

# 6. 重複したpolygon_idを抽出
selected_ids2 <- names(selected_image_files2)[overlap_with_folder1]

# 7. そのpolygon_idを持つ全画像ファイル（フォルダ2）を抽出
all_selected_files2 <- image_files2[polygon_ids2 %in% selected_ids2]

# 8. 出力フォルダにコピー
if (!dir_exists(output_base)) dir_create(output_base, recurse = TRUE)
file_copy(all_selected_files2, output_base, overwrite = TRUE)

##########################################################################################################################
# 必要パッケージ
library(terra)
library(sf)
library(igraph)
library(stringr)
library(fs)

# 1. 画像の入力フォルダとポリゴンの入力フォルダを指定
image_input_folder <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat7_2000_common"    # 画像フォルダ
polygon_input_file <- "C:/Users/casakuma-lab-04-std/Downloads/polygon_Bare_2000-2010 3/polygon_Bare_2000-2010/polygon_Bare_2000_ダウンロード.shp" # ポリゴン(shp, geojson等)

# 2. 画像ファイル名からpolygon_id抽出
image_files <- list.files(image_input_folder, pattern = "\\.tif$", full.names = TRUE)
get_polygon_id <- function(filename) {
  str_extract(basename(filename), "(?<=polygon)\\d+")
}
polygon_ids <- sapply(image_files, get_polygon_id)
unique_polygon_ids <- unique(polygon_ids)

# 3. polygon_idごとに代表画像抽出
selected_image_files <- sapply(unique_polygon_ids, function(pid) {
  files <- image_files[polygon_ids == pid]
  files[1]
})

# 4. 各代表画像のポリゴン取得
polys <- lapply(selected_image_files, function(f) {
  r <- rast(f)
  poly <- as.polygons(ext(r)) |> st_as_sf()
  st_crs(poly) <- st_crs(r)
  poly
})

# 5. ポリゴンファイルの読み込み
input_poly <- st_read(polygon_input_file)

# 6. 空間的重複判定し、重複したgroup_idを記録
# group_idはポリゴンファイルの属性名（例: "group_id"）とする
# 必要に応じて属性名を変更してください
if (!"group_id" %in% colnames(input_poly)) {
  stop("ポリゴンファイルにgroup_id属性がありません。属性名を確認してください。")
}
overlap_group_ids <- c()
for (i in seq_along(polys)) {
  # polys[[i]]とinput_polyの重複判定
  overlap_idx <- which(st_intersects(polys[[i]], input_poly, sparse = FALSE)[1,])
  if (length(overlap_idx) > 0) {
    overlap_group_ids <- c(overlap_group_ids, input_poly$group_id[overlap_idx])
  }
}

# 7. ユニーク値と個数を出力
unique_group_ids <- unique(overlap_group_ids)
cat("重複group_idのユニーク値:\n")
print(unique_group_ids)
cat("重複group_idの個数:\n")
print(length(unique_group_ids))

#######################################################################################
# 必要パッケージ
library(terra)
library(sf)
library(stringr)
library(fs)

# 1. 入力・出力フォルダの指定
image_input_folder <- "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/L8_2024/val_Landsat7_2000_サイト統一"
polygon_input_file <- "F:/2024_hirora_Toyota6/2023_hirota/Polygon/Polygon_Bare_20250819_2021ver/merged_group_polygon.shp"     # 例: "F:/path/to/polygon.shp"
output_folder <- "F:/2024_hirora_Toyota6/2023_hirota/Polygon/Polygon_Bare_20250819_2021ver"

# 2. 画像ファイルからpolygon_id抽出
image_files <- list.files(image_input_folder, pattern = "\\.tif$", full.names = TRUE)
get_polygon_id <- function(filename) {
  str_extract(basename(filename), "(?<=polygon)\\d+")
}
polygon_ids <- sapply(image_files, get_polygon_id)
unique_polygon_ids <- unique(polygon_ids)

# 3. polygon_idごとに代表画像抽出
selected_image_files <- sapply(unique_polygon_ids, function(pid) {
  files <- image_files[polygon_ids == pid]
  files[1]
})

# 4. 各代表画像のポリゴン取得
polys <- lapply(selected_image_files, function(f) {
  r <- rast(f)
  poly <- as.polygons(ext(r)) |> st_as_sf()
  st_crs(poly) <- st_crs(r)
  poly
})

# 5. ポリゴンファイルの読み込み
input_poly <- st_read(polygon_input_file)

# 画像ポリゴンと重複するポリゴンのインデックス抽出
overlap_indices <- c()
for (poly in polys) {
  idx <- which(st_intersects(poly, input_poly, sparse = FALSE)[1,])
  overlap_indices <- c(overlap_indices, idx)
}
overlap_indices <- unique(overlap_indices)

# 重複ポリゴンのみ抽出
overlap_polygons <- input_poly[overlap_indices, ]

# 出力フォルダに保存（shp形式例）
output_polygon_file <- file.path(output_folder, "valid_polygons.shp")
st_write(overlap_polygons, output_polygon_file, delete_dsn = TRUE)