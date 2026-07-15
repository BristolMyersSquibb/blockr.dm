#' Read data files into a list of tables
#'
#' Reads each file according to its extension. When a cache directory is
#' configured (option `blockr.dm_read_cache_dir` or environment variable
#' `BLOCKR_DM_READ_CACHE_DIR`), slow statistical formats (SAS, SPSS, Stata)
#' are transparently mirrored as parquet: the first read converts the source
#' file, every later read loads the parquet copy instead. Cache entries are
#' keyed on file name, size and modification time, so an updated source file
#' invalidates its entry automatically. Stale entries are never evicted;
#' the cache directory can simply be deleted to reclaim space.
#'
#' Parquet caching requires the arrow package (nanoparquet is not a
#' substitute here: it strips column attributes, and ADaM labels must
#' survive the round-trip). Without arrow, or with no cache directory set,
#' files are read directly and behavior is unchanged.
#'
#' @param files Character vector of file paths.
#' @param cache_dir Directory for the parquet cache. `""` disables caching.
#'   Defaults to the `dm_read_cache_dir` blockr option.
#'
#' @return An unnamed list of tables, one per file.
#'
#' @export
dm_read_tables <- function(files, cache_dir = dm_read_cache_dir()) {
  lapply(files, dm_read_table, cache_dir = cache_dir)
}

#' @noRd
dm_read_cache_dir <- function() {
  blockr.core::blockr_option("dm_read_cache_dir", "")
}

#' Extensions worth caching: formats read via haven, which are slow to parse
#' (and, on network shares, slow to pull). Native-fast formats (parquet,
#' feather, rds) and text formats gain little and are read directly.
#' @noRd
dm_read_cacheable_exts <- c("sas7bdat", "xpt", "sav", "zsav", "dta", "por")

#' @noRd
dm_read_table <- function(f, cache_dir) {
  ext <- tolower(tools::file_ext(f))

  use_cache <- nzchar(cache_dir) &&
    ext %in% dm_read_cacheable_exts &&
    requireNamespace("arrow", quietly = TRUE)

  if (!use_cache) {
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

#' Cache entry path for a source file: readable stem plus a key hashed from
#' the normalized path, size and mtime, so a touched or rewritten source
#' misses and gets re-converted.
#' @noRd
dm_read_cache_path <- function(f, cache_dir) {
  info <- file.info(f)
  key <- rlang::hash(
    list(
      path = normalizePath(f, winslash = "/", mustWork = FALSE),
      size = info$size,
      mtime = as.numeric(info$mtime)
    )
  )
  stem <- tools::file_path_sans_ext(basename(f))
  file.path(cache_dir, paste0(stem, "-", key, ".parquet"))
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

#' Reader expression for a vector of files bound to `files`
#'
#' Block expressions stay generic R: with no cache dir configured (the
#' default) the emitted code is a plain per-extension reading loop with no
#' blockr.dm dependency. Only when caching is active (and wanted, see
#' `cache`) does the expression become a call to [dm_read_tables()], with
#' the cache dir baked in as a literal so the code remains copy-runnable.
#'
#' @noRd
dm_read_tables_expr <- function(cache = TRUE) {
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
