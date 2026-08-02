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

  dm_read_state$backend_error <- NULL

  reads <- lapply(files, function(f) {
    started <- Sys.time()
    out <- dm_read_table(f, cache_dir = cache_dir, cache_board = cache_board)
    out$path <- f
    out$seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    out
  })

  dm_read_stats_record(reads)

  lapply(reads, `[[`, "table")
}

#' Where the last read of each table came from
#'
#' A read is the one thing about this block that is worth minutes rather than
#' milliseconds, and from the outside a warm read and a cold one look the
#' same: the app just sits there. `dm_read_tables()` records how each table
#' was obtained and how long it took, and the block reports it, so "this is
#' slow" can be answered with "because two SAS files were converted", not
#' guessed at.
#'
#' Kept as the most recent batch only. Reads are serialized by Shiny's single
#' R process, and the block reads this immediately after its own evaluation,
#' so the last batch is that block's batch; the stems let a caller confirm
#' that rather than assume it.
#'
#' @noRd
dm_read_state <- new.env(parent = emptyenv())

#' @noRd
dm_read_stats_record <- function(reads) {

  rows <- lapply(reads, function(r) {
    size <- file.size(r$path)
    data.frame(
      path = r$path,
      stem = make.names(tools::file_path_sans_ext(basename(r$path))),
      source = r$source,
      seconds = r$seconds,
      bytes = if (is.na(size)) 0 else size
    )
  })

  dm_read_state$last <- do.call(rbind, rows)

  invisible(NULL)
}

#' A backend that was configured but could not be reached is the failure the
#' block has to be loudest about: everything still works, just slowly and
#' forever, and nothing on screen says why.
#' @noRd
dm_read_backend_failed <- function(e) {
  dm_read_state$backend_error <- conditionMessage(e)
  invisible(NULL)
}

#' @param stems Table names to report on; rows for other tables are dropped.
#' @noRd
dm_read_stats <- function(stems = NULL) {
  res <- dm_read_state$last

  if (is.null(res) || !nrow(res)) {
    return(NULL)
  }

  if (!is.null(stems)) {
    res <- res[res$stem %in% stems, , drop = FALSE]
  }

  if (!nrow(res)) NULL else res
}

#' Human-readable duration, at the precision the number deserves
#' @noRd
dm_read_fmt_secs <- function(s) {
  if (!length(s) || is.na(s)) {
    return("")
  }
  if (s >= 90) {
    return(sprintf("%.1f min", s / 60))
  }
  if (s >= 1) {
    return(sprintf("%.1f s", s))
  }
  sprintf("%.0f ms", s * 1000)
}

#' One line saying where the data came from and what it cost
#'
#' Written for the question that actually gets asked in front of the app:
#' "is this using the cache, or is it reading the SAS files again?" -- and
#' its follow-up, "why is blockr so slow" when the answer is that 400 MB of
#' .sas7bdat is being parsed.
#'
#' @noRd
dm_read_status <- function(stems, elapsed = NULL) {

  stats <- dm_read_stats(stems)
  err <- dm_read_state$backend_error
  configured <- nzchar(dm_read_cache_dir()) || !is.null(dm_read_cache_board())

  total <- if (!is.null(elapsed)) dm_read_fmt_secs(elapsed) else ""
  head_txt <- if (nzchar(total)) {
    sprintf("%d tables in %s", length(stems), total)
  } else {
    sprintf("%d tables", length(stems))
  }

  if (is.null(stats)) {
    # No read went through the cache-aware reader: an Excel / ZIP / RDS
    # source, or no backend configured at all. Say which -- silence here is
    # what makes people guess.
    detail <- if (configured) {
      "parquet cache not used for this source"
    } else {
      "no parquet cache configured"
    }
    return(list(text = paste(head_txt, detail, sep = " · "),
                state = "plain"))
  }

  hit <- stats$source %in% c("cache-dir", "cache-board")
  conv <- stats$source == "converted"

  parts <- character()

  if (any(hit)) {
    parts <- c(parts, sprintf(
      "%d from parquet cache (%s)", sum(hit),
      dm_read_fmt_secs(sum(stats$seconds[hit]))
    ))
  }

  if (any(conv)) {
    parts <- c(parts, sprintf(
      "%d converted from source (%s)", sum(conv),
      dm_read_fmt_secs(sum(stats$seconds[conv]))
    ))
  }

  if (any(!hit & !conv)) {
    parts <- c(parts, sprintf(
      "%d read directly (%s)", sum(!hit & !conv),
      dm_read_fmt_secs(sum(stats$seconds[!hit & !conv]))
    ))
  }

  if (!is.null(err) && nzchar(err)) {
    parts <- c(parts, sprintf("cache unavailable: %s", err))
  }

  list(
    text = paste(c(head_txt, parts), collapse = " · "),
    state = if (!is.null(err) && nzchar(err)) {
      "error"
    } else if (any(conv)) {
      "converted"
    } else if (any(hit)) {
      "cached"
    } else {
      "plain"
    }
  )
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

#' Returns the table alongside where it came from, for the read report:
#' `"cache-dir"` / `"cache-board"` (a hit), `"converted"` (read from source
#' and mirrored into the cache), or `"source"` (read directly, because the
#' format is not worth caching or no backend is configured).
#' @noRd
dm_read_table <- function(f, cache_dir, cache_board) {
  ext <- tolower(tools::file_ext(f))

  cacheable <- ext %in% dm_read_cacheable_exts &&
    requireNamespace("arrow", quietly = TRUE)

  if (cacheable && !is.null(cache_board) &&
        requireNamespace("pins", quietly = TRUE)) {
    return(dm_read_table_board(f, cache_board))
  }

  if (!cacheable || !nzchar(cache_dir)) {
    return(list(table = dm_read_file(f), source = "source"))
  }

  cached <- dm_read_cache_path(f, cache_dir)

  if (file.exists(cached)) {
    # mmap = FALSE: a memory-mapped read keeps the file open, and on Windows a
    # later invalidation (rewriting the same path) then fails with a locked-file
    # error. Read it fully into memory so the cache entry stays replaceable.
    return(
      list(
        table = arrow::read_parquet(cached, mmap = FALSE),
        source = "cache-dir"
      )
    )
  }

  res <- dm_read_file(f)

  if (is.data.frame(res)) {
    dm_read_cache_write(res, cached, cache_dir)
  }

  list(table = res, source = "converted")
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
dm_read_table_board <- function(f, board) {
  name <- dm_read_pin_name(f)
  key <- dm_read_cache_key(f)

  hit <- tryCatch(
    pins::pin_exists(board, name) &&
      identical(pins::pin_meta(board, name)$user$key, key),
    error = function(e) {
      dm_read_backend_failed(e)
      FALSE
    }
  )

  if (hit) {
    res <- tryCatch(
      arrow::read_parquet(pins::pin_download(board, name), mmap = FALSE),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      return(list(table = res, source = "cache-board"))
    }
  }

  res <- dm_read_file(f)

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
      error = dm_read_backend_failed
    )
  }

  list(table = res, source = "converted")
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

  bquote(lapply(files, .(dm_read_file_expr())))
}

#' The one reader, as a language object
#'
#' Both routes into a file go through this body: the emitted uncached loop
#' inlines it, and [dm_read_file()] below IS it. Keeping one definition is
#' not tidiness -- when the two drifted, a `.sas7bdat` came back as a
#' `data.frame` with the cache off and a `tbl_df` with it on, which made
#' turning on a cache a semantic change to the data rather than a speedup.
#'
#' `haven` is called directly rather than behind `requireNamespace()`: the
#' fallback it used to guard (`rio::import()`) reads these formats through
#' haven anyway, so the guard bought a worse error message, not a working
#' read -- and a guard in emitted code is noise for whoever copies it.
#'
#' @noRd
dm_read_file_expr <- function() {
  quote(
    function(f) {
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
      } else if (ext == "rds") {
        readRDS(f)
      } else if (ext == "rda") {
        e <- new.env()
        load(f, envir = e)
        as.list(e)[[1]]
      } else if (ext == "sas7bdat") {
        haven::read_sas(f)
      } else if (ext == "xpt") {
        haven::read_xpt(f)
      } else if (ext %in% c("sav", "zsav", "por")) {
        haven::read_spss(f)
      } else if (ext == "dta") {
        haven::read_dta(f)
      } else {
        rio::import(f)
      }
    }
  )
}

#' Read one data file by extension
#' @noRd
dm_read_file <- eval(dm_read_file_expr())
