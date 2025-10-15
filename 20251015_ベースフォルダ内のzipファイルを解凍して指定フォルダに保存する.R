# move_archives_contents.R
# ベースフォルダ内のアーカイブを解凍し、解凍後の中身を出力フォルダへ移動するユーティリティ

move_archives_contents <- function(base_dir,
                                    out_dir,
                                    recursive = TRUE,
                                    keep_archive_name = TRUE,
                                    flatten = FALSE,
                                    remove_extracted_dir = TRUE,
                                    patterns = c("zip", "tar", "tar.gz", "tgz"),
                                    overwrite = FALSE,
                                    verbose = TRUE) {
  stopifnot(is.character(base_dir), length(base_dir) == 1)
  stopifnot(is.character(out_dir), length(out_dir) == 1)
  base_dir <- normalizePath(base_dir, mustWork = TRUE)
  out_dir <- normalizePath(out_dir, mustWork = FALSE)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # ログ関数
  log_msg <- function(fmt, ...) {
    if (verbose) cat(sprintf(fmt, ...), "\n")
  }

  # 検索用正規表現（拡張子）
  ext_pat <- paste0("\\.(", paste0(gsub("\\.", "", patterns), collapse = "|"), ")$", collapse = "")
  archives <- list.files(base_dir, pattern = ext_pat, recursive = recursive, full.names = TRUE, ignore.case = TRUE)

  if (length(archives) == 0) {
    log_msg("ベースフォルダ '%s' に対象アーカイブが見つかりませんでした。patterns: %s", base_dir, paste(patterns, collapse = ", "))
    return(invisible(list(success = TRUE, processed = 0)))
  }

  # 移動 (file.rename の fallback を実装)
  move_path <- function(src, dest) {
    dest_dir <- dirname(dest)
    if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
    ok <- file.rename(src, dest)
    if (!ok) {
      # fallback: copy then remove
      copied <- file.copy(src, dest, recursive = TRUE, copy.mode = TRUE)
      if (!copied) return(FALSE)
      # 削除（ファイルまたはディレクトリ）
      unlink(src, recursive = TRUE, force = TRUE)
      return(TRUE)
    }
    return(TRUE)
  }

  make_unique <- function(path) {
    if (!file.exists(path) || overwrite) return(path)
    base <- tools::file_path_sans_ext(basename(path))
    ext <- paste0(".", tools::file_ext(path))
    if (ext == ".") ext <- ""
    dirn <- dirname(path)
    i <- 1L
    repeat {
      candidate <- file.path(dirn, paste0(base, "_", i, ext))
      if (!file.exists(candidate)) return(candidate)
      i <- i + 1L
    }
  }

  processed <- 0L
  for (arch in archives) {
    log_msg("処理中: %s", arch)
    base_name <- tools::file_path_sans_ext(basename(arch))
    # .tar.gz のような二重拡張に対応してベース名を整える
    # もし .tar.gz を patterns に含めているなら tools::file_path_sans_ext で .gz が取れるので追加処理:
    if (grepl("\\.tar\\.gz$|\\.tgz$", tolower(basename(arch)))) {
      base_name <- sub("\\.(tar\\.gz|tgz)$", "", tolower(basename(arch)), perl = TRUE)
      # 元の大文字小文字を保つため再定義
      base_name <- sub("\\.(tar\\.gz|tgz)$", "", basename(arch), perl = TRUE)
    }

    # 解凍先（テンポラリフォルダ）
    extract_dir <- file.path(tempdir(), paste0("ex_", base_name, "_", as.integer(Sys.time())))
    dir.create(extract_dir, recursive = TRUE)
    on.exit(NULL) # 個別に後片付けする

    # 解凍（拡張子により切替）
    ext <- tolower(tools::file_ext(arch))
    tryCatch({
      if (ext == "zip") {
        utils::unzip(arch, exdir = extract_dir)
      } else if (ext %in% c("tar", "gz", "tgz")) {
        utils::untar(arch, exdir = extract_dir)
      } else {
        log_msg("未対応拡張子をスキップ: %s", arch)
        unlink(extract_dir, recursive = TRUE, force = TRUE)
        next
      }
    }, error = function(e) {
      log_msg("解凍失敗: %s -- %s", arch, conditionMessage(e))
      unlink(extract_dir, recursive = TRUE, force = TRUE)
      next
    })

    # 解凍後のファイル一覧
    contents <- list.files(extract_dir, all.files = TRUE, full.names = TRUE, no.. = TRUE, recursive = FALSE)
    if (length(contents) == 0) {
      # まれにアーカイブの中身がルート直下に空／隠ししかない場合 recursive=TRUE で再チェック
      contents <- list.files(extract_dir, all.files = TRUE, full.names = TRUE, no.. = TRUE, recursive = TRUE)
    }

    if (length(contents) == 0) {
      log_msg("解凍は成功しましたが中身が見つかりません: %s", arch)
      unlink(extract_dir, recursive = TRUE, force = TRUE)
      next
    }

    # 移動先パスの決定
    dest_root <- out_dir
    if (keep_archive_name && !flatten) {
      dest_root <- file.path(out_dir, base_name)
      if (!dir.exists(dest_root)) dir.create(dest_root, recursive = TRUE)
    }

    # contents を dest_root に移動する (単一ファイル/ディレクトリのリスト)
    for (item in list.files(extract_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)) {
      item_basename <- basename(item)
      dest_path <- file.path(dest_root, item_basename)
      if (file.exists(dest_path) && !overwrite) {
        dest_path <- make_unique(dest_path)
        log_msg("既存ファイルと衝突したためリネーム: %s", dest_path)
      }
      success <- FALSE
      if (dir.exists(item)) {
        # ディレクトリは再帰コピー（move）
        # まず try rename
        success <- move_path(item, dest_path)
      } else {
        success <- move_path(item, dest_path)
      }
      if (!success) {
        log_msg("移動失敗: %s -> %s", item, dest_path)
      } else {
        processed <- processed + 1L
      }
    }

    if (remove_extracted_dir) {
      # 残ったテンポラリフォルダを削除
      if (dir.exists(extract_dir)) unlink(extract_dir, recursive = TRUE, force = TRUE)
    } else {
      log_msg("テンポラリ解凍フォルダを保持します: %s", extract_dir)
    }

    log_msg("完了: %s", arch)
  }

  log_msg("全処理完了。移動済みアイテム数: %d", processed)
  invisible(list(success = TRUE, processed = processed, archives = archives))
}

# 使い方例（コメントアウト）
 result <- move_archives_contents(
   base_dir = "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/20250929_L8_2021ver/polygon_1_250",
   out_dir = "F:/2024_hirora_Toyota6/2023_hirota/tool/CNNvsTransformer-main/CNNvsTransformer-main/data/20250929_L8_2021ver/polygon_All",
   recursive = TRUE,
   keep_archive_name = TRUE,
   flatten = FALSE,
   remove_extracted_dir = TRUE,
   overwrite = FALSE,
   verbose = TRUE
)