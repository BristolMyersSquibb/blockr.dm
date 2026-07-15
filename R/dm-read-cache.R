#' Read data files into a list of tables
#'
#' Reads each file according to its extension. When a cache backend is
#' configured, slow statistical formats (SAS, SPSS, Stata) are transparently
#' mirrored as parquet: the first read converts the source file, every later
#' read loads the parquet copy instead. Cache entries are keyed on file name,
#' size and modification time, so an updated source file invalidates its
#' entry automatically.
#'
#' Two backends are supported, in order of precedence:
#'
#' * A pins board (option `blockr.dm_read_cache_board`), e.g.
#'   `pins::board_connect()` on Posit Connect, where the board authenticates
#'   through the `CONNECT_SERVER`/`CONNECT_API_KEY` environment variables the
#'   server injects into running content. One pin per source table; the
#'   invalidation key lives in the pin metadata and a changed source
#'   publishes a new pin version. Reads go through the board's local pin
#'   cache, so a warm read is a local file read plus a version check.
#' * A cache directory (option `blockr.dm_read_cache_dir` or environment
#'   variable `BLOCKR_DM_READ_CACHE_DIR`), holding one parquet file per
#'   source file and key. Stale entries are never evicted; delete the
#'   directory to reclaim space.
#'
#' Parquet caching requires the arrow package (nanoparquet is not a
#' substitute here: it strips column attributes, and ADaM labels must
#' survive the round-trip). Without arrow, or with no backend configured,
#' files are read directly and behavior is unchanged. Cache failures (an
#' unreachable board, an unwritable directory) never fail the read; they
#' only cost the speedup.
#'
#' @param files Character vector of file paths.
#' @param cache_dir Directory for the parquet cache. `""` disables the
#'   directory backend. Defaults to the `dm_read_cache_dir` blockr option.
#' @param cache_board A pins board for the parquet cache, `NULL` to disable
#'   the board backend. Defaults to the `dm_read_cache_board` blockr option.
#'
#' @return An unnamed list of tables, one per file.
#'
#' @export
dm_read_tables <- function(files, cache_dir = dm_read_cache_dir(),
                           cache_board = dm_read_cache_board()) {
  lapply(files, dm_read_table, cache_dir = cache_dir,
         cache_board = cache_board)
}

#' @noRd
dm_read_cache_dir <- function() {
  blockr.core::blockr_option("dm_read_cache_dir", "")
}

#' The board option only makes sense as an R object, so anything that is not
#' a pins board (e.g. a string that leaked in via an env var) counts as unset.
#' @noRd
dm_read_cache_board <- function() {
  board <- blockr.core::blockr_option("dm_read_cache_board", NULL)
  if (inherits(board, "pins_board")) board else NULL
}

#' Extensions worth caching: formats read via haven, which are slow to parse
#' (and, on network shares, slow to pull). Native-fast formats (parquet,
#' feather, rds) and text formats gain little and are read directly.
#' @noRd
dm_read_cacheable_exts <- c("sas7bdat", "xpt", "sav", "zsav", "dta", "por")

#' @noRd
dm_read_table <- function(f, cache_dir, cache_board) {
  ext <- tolower(tools::file_ext(f))

  cacheable <- ext %in% dm_read_cacheable_exts &&
    requireNamespace("arrow", quietly = TRUE)

  if (cacheable && !is.null(cache_board) &&
        requireNamespace("pins", quietly = TRUE)) {
    return(dm_read_table_board(f, ext, cache_board))
  }

  if (!cacheable || !nzchar(cache_dir)) {
    return(dm_read_file(f, ext))
  }

  cached <- dm_read_cache_path(f, cache_dir)

  if (file.exists(cached)) {
    return(arrow::read_parquet(cached))
  }

  res <- dm_read_file(f, ext)

  if (is.data.frame(res)) {
    dm_read_cache_write(res, cached, cache_dir)
  }

  res
}

#' Invalidation key for a source file: hashed from the normalized path, size
#' and mtime, so a touched or rewritten source misses and gets re-converted.
#' @noRd
dm_read_cache_key <- function(f) {
  info <- file.info(f)
  rlang::hash(
    list(
      path = normalizePath(f, winslash = "/", mustWork = FALSE),
      size = info$size,
      mtime = as.numeric(info$mtime)
    )
  )
}

#' Cache entry path for a source file: readable stem plus the key.
#' @noRd
dm_read_cache_path <- function(f, cache_dir) {
  stem <- tools::file_path_sans_ext(basename(f))
  file.path(cache_dir, paste0(stem, "-", dm_read_cache_key(f), ".parquet"))
}

#' Write a cache entry atomically (temp file + rename) so concurrent
#' sessions converting the same file cannot see a half-written parquet.
#' A failed cache write is silently ignored: it only costs the speedup.
#' @noRd
dm_read_cache_write <- function(df, cached, cache_dir) {
  tryCatch(
    {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      tmp <- tempfile("dm_read_", tmpdir = cache_dir, fileext = ".parquet")
      on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
      arrow::write_parquet(df, tmp)
      file.rename(tmp, cached)
    },
    error = function(e) FALSE
  )
  invisible(NULL)
}

#' Pin name for a source file: sanitized stem plus a short hash of the
#' containing directory, so same-named tables from different studies get
#' distinct pins while data-version churn stays within one pin (as versions,
#' which the board can prune) instead of minting ever-new names.
#' @noRd
dm_read_pin_name <- function(f) {
  stem <- tolower(gsub("[^A-Za-z0-9]+", "-",
                       tools::file_path_sans_ext(basename(f))))
  dir_key <- rlang::hash(
    normalizePath(dirname(f), winslash = "/", mustWork = FALSE)
  )
  paste0("dm-read-", substr(dir_key, 1, 8), "-", stem)
}

#' Board-backed cache: hit when the pin exists and its stored key matches
#' the source file. On miss, read the source and publish the arrow-written
#' parquet via pin_upload() -- never pin_write(type = "parquet"), which goes
#' through nanoparquet and strips labels. Any board failure falls back to a
#' direct read.
#' @noRd
dm_read_table_board <- function(f, ext, board) {
  name <- dm_read_pin_name(f)
  key <- dm_read_cache_key(f)

  hit <- tryCatch(
    pins::pin_exists(board, name) &&
      identical(pins::pin_meta(board, name)$user$key, key),
    error = function(e) FALSE
  )

  if (hit) {
    res <- tryCatch(
      arrow::read_parquet(pins::pin_download(board, name)),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      return(res)
    }
  }

  res <- dm_read_file(f, ext)

  if (is.data.frame(res)) {
    tryCatch(
      {
        tmp_dir <- tempfile("dm_read_pin_")
        dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        tmp <- file.path(
          tmp_dir,
          paste0(tools::file_path_sans_ext(basename(f)), ".parquet")
        )
        arrow::write_parquet(res, tmp)
        suppressMessages(
          pins::pin_upload(board, tmp, name = name,
                           metadata = list(key = key))
        )
      },
      error = function(e) NULL
    )
  }

  res
}

#' Reader expression for a vector of files bound to `files`
#'
#' Block expressions stay generic R: with no cache backend configured (the
#' default) the emitted code is a plain per-extension reading loop with no
#' blockr.dm dependency. Only when caching is active (and wanted, see
#' `cache`) does the expression become a call to [dm_read_tables()]. A
#' directory backend is baked in as a literal so the code remains
#' copy-runnable; a board cannot be represented as a literal, so the board
#' case emits a bare call whose default resolves the option at eval time.
#'
#' @noRd
dm_read_tables_expr <- function(cache = TRUE) {
  if (cache && !is.null(dm_read_cache_board())) {
    return(quote(blockr.dm::dm_read_tables(files)))
  }

  cache_dir <- dm_read_cache_dir()

  if (cache && nzchar(cache_dir)) {
    return(
      bquote(blockr.dm::dm_read_tables(files, cache_dir = .(cache_dir)))
    )
  }

  quote(
    lapply(files, function(f) {
      ext <- tolower(tools::file_ext(f))
      if (ext %in% c("csv", "tsv")) {
        readr::read_csv(f, show_col_types = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        readxl::read_excel(f)
      } else if (ext == "parquet") {
        if (requireNamespace("arrow", quietly = TRUE)) {
          arrow::read_parquet(f)
        } else {
          nanoparquet::read_parquet(f)
        }
      } else if (ext == "feather") {
        arrow::read_feather(f)
      } else if (ext %in% c("rds")) {
        readRDS(f)
      } else if (ext == "rda") {
        e <- new.env()
        load(f, envir = e)
        as.list(e)[[1]]
      } else {
        rio::import(f)
      }
    })
  )
}

#' Read one data file by extension
#' @noRd
dm_read_file <- function(f, ext = tolower(tools::file_ext(f))) {
  if (ext %in% c("csv", "tsv")) {
    readr::read_csv(f, show_col_types = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(f)
  } else if (ext == "parquet") {
    if (requireNamespace("arrow", quietly = TRUE)) {
      arrow::read_parquet(f)
    } else {
      nanoparquet::read_parquet(f)
    }
  } else if (ext == "feather") {
    arrow::read_feather(f)
  } else if (ext == "rds") {
    readRDS(f)
  } else if (ext == "rda") {
    e <- new.env()
    load(f, envir = e)
    as.list(e)[[1]]
  } else if (ext == "sas7bdat" && requireNamespace("haven", quietly = TRUE)) {
    haven::read_sas(f)
  } else if (ext == "xpt" && requireNamespace("haven", quietly = TRUE)) {
    haven::read_xpt(f)
  } else if (ext %in% c("sav", "zsav", "por") &&
               requireNamespace("haven", quietly = TRUE)) {
    haven::read_spss(f)
  } else if (ext == "dta" && requireNamespace("haven", quietly = TRUE)) {
    haven::read_dta(f)
  } else {
    rio::import(f)
  }
}
