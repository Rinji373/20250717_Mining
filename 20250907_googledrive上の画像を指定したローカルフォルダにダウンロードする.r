# パッケージのインストール（初回のみ）
install.packages("googledrive")

# パッケージのロード
library(googledrive)

library(googledrive)
drive_deauth()      # 認証解除
drive_auth(cache = FALSE)  # 再認証（ブラウザが開きます）
# ダウンロードしたいフォルダのIDを指定
folder_id <- "1vZR7zX9kiC5P9ll_zGDlMjZsdhZ_5TYX"

# フォルダ内のファイル一覧を取得
files <- drive_ls(as_id(folder_id))

# 保存先のローカルフォルダを指定
local_dir <- "F:/2024_hirora_Toyota6/2023_hirota/Image/2021_BareImage_1001_2000/"  # ここを任意のパスに変更

# フォルダがなければ作成
if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

for (i in seq_len(nrow(files))) {
  save_path <- file.path(local_dir, files$name[i])
  if (file.exists(save_path)) {
    next  # 既にファイルがあればスキップ
  }
  drive_download(files$id[i], path = save_path, overwrite = TRUE)
}