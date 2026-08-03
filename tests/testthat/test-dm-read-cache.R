write_sas_fixture <- function(dir, name = "adxx.sas7bdat") {
  df <- data.frame(
    USUBJID = c("01-001", "01-002", "01-003"),
    AGE = c(34, 61, 47)
  )
  attr(df$AGE, "label") <- "Age"
  f <- file.path(dir, name)
  suppressWarnings(haven::write_sas(df, f))
  f
}

test_that("dm_read_tables caches sas7bdat as parquet and hits the cache", {
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")

  src_dir <- withr::local_tempdir()
  cache_dir <- withr::local_tempdir()
  f <- write_sas_fixture(src_dir)

  cold <- dm_read_tables(f, cache_dir = cache_dir)[[1]]

  entries <- list.files(cache_dir, pattern = "\\.parquet$")
  expect_length(entries, 1L)

  warm <- dm_read_tables(f, cache_dir = cache_dir)[[1]]
  expect_identical(as.data.frame(warm), as.data.frame(cold))
  expect_identical(lapply(warm, attributes), lapply(cold, attributes))
  expect_identical(attr(warm$AGE, "label"), "Age")

  # still a single entry: the second read did not re-convert
  expect_length(list.files(cache_dir, pattern = "\\.parquet$"), 1L)

  # prove the hit reads the parquet copy, not the source
  sentinel <- data.frame(x = 99)
  arrow::write_parquet(sentinel, file.path(cache_dir, entries))
  hit <- dm_read_tables(f, cache_dir = cache_dir)[[1]]
  expect_identical(as.data.frame(hit), sentinel)
})

test_that("a changed source file invalidates its cache entry", {
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")

  src_dir <- withr::local_tempdir()
  cache_dir <- withr::local_tempdir()
  f <- write_sas_fixture(src_dir)

  dm_read_tables(f, cache_dir = cache_dir)
  expect_length(list.files(cache_dir, pattern = "\\.parquet$"), 1L)

  Sys.setFileTime(f, Sys.time() + 10)
  dm_read_tables(f, cache_dir = cache_dir)
  expect_length(list.files(cache_dir, pattern = "\\.parquet$"), 2L)
})

test_that("caching is off by default and skips non-haven formats", {
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")

  src_dir <- withr::local_tempdir()
  cache_dir <- withr::local_tempdir()

  f <- write_sas_fixture(src_dir)
  res <- dm_read_tables(f, cache_dir = "")[[1]]
  expect_identical(res$USUBJID, c("01-001", "01-002", "01-003"))

  csv <- file.path(src_dir, "adyy.csv")
  write.csv(data.frame(a = 1:2), csv, row.names = FALSE)
  dm_read_tables(csv, cache_dir = cache_dir)

  expect_length(list.files(cache_dir), 0L)
})

test_that("cache writes are atomic: no temp files left behind", {
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")

  src_dir <- withr::local_tempdir()
  cache_dir <- withr::local_tempdir()
  f <- write_sas_fixture(src_dir)

  dm_read_tables(f, cache_dir = cache_dir)
  expect_length(list.files(cache_dir), 1L)
  expect_false(any(startsWith(list.files(cache_dir), "dm_read_")))
})

test_that("a failing cache write does not fail the read", {
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")

  src_dir <- withr::local_tempdir()
  f <- write_sas_fixture(src_dir)

  bogus <- file.path(src_dir, "adxx.sas7bdat", "not-a-dir")
  expect_no_error(res <- dm_read_tables(f, cache_dir = bogus))
  expect_identical(res[[1]]$USUBJID, c("01-001", "01-002", "01-003"))
})

test_that("a pins board caches sas7bdat as one pin and hits it", {
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")
  skip_if_not_installed("pins")

  src_dir <- withr::local_tempdir()
  f <- write_sas_fixture(src_dir)
  board <- pins::board_temp()

  cold <- dm_read_tables(f, cache_dir = "", cache_board = board)[[1]]

  name <- blockr.dm:::dm_read_pin_name(f)
  expect_true(pins::pin_exists(board, name))
  expect_identical(
    pins::pin_meta(board, name)$user$key,
    blockr.dm:::dm_read_cache_key(f)
  )

  warm <- dm_read_tables(f, cache_dir = "", cache_board = board)[[1]]
  expect_identical(as.data.frame(warm), as.data.frame(cold))
  expect_identical(lapply(warm, attributes), lapply(cold, attributes))
  expect_identical(attr(warm$AGE, "label"), "Age")

  # prove the hit reads the pin, not the source: republish a sentinel
  # under the same name and key
  sentinel <- data.frame(x = 99)
  tmp <- file.path(withr::local_tempdir(), "adxx.parquet")
  arrow::write_parquet(sentinel, tmp)
  pins::pin_upload(board, tmp, name = name,
                   metadata = list(key = blockr.dm:::dm_read_cache_key(f)))
  hit <- dm_read_tables(f, cache_dir = "", cache_board = board)[[1]]
  expect_identical(as.data.frame(hit), sentinel)
})

test_that("a changed source republishes the pin under the same name", {
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")
  skip_if_not_installed("pins")

  src_dir <- withr::local_tempdir()
  f <- write_sas_fixture(src_dir)
  board <- pins::board_temp(versioned = TRUE)

  dm_read_tables(f, cache_dir = "", cache_board = board)
  name <- blockr.dm:::dm_read_pin_name(f)
  key1 <- pins::pin_meta(board, name)$user$key

  # a new data cut: content (and size) change, key changes with it
  df2 <- data.frame(USUBJID = c("01-001", "01-002"), AGE = c(34, 61))
  suppressWarnings(haven::write_sas(df2, f))
  res <- dm_read_tables(f, cache_dir = "", cache_board = board)[[1]]

  expect_identical(res$USUBJID, c("01-001", "01-002"))
  expect_identical(pins::pin_list(board), name)
  key2 <- pins::pin_meta(board, name)$user$key
  expect_false(identical(key1, key2))
  expect_identical(key2, blockr.dm:::dm_read_cache_key(f))
})

test_that("the board takes precedence over the directory backend", {
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")
  skip_if_not_installed("pins")

  src_dir <- withr::local_tempdir()
  cache_dir <- withr::local_tempdir()
  f <- write_sas_fixture(src_dir)
  board <- pins::board_temp()

  dm_read_tables(f, cache_dir = cache_dir, cache_board = board)

  expect_true(pins::pin_exists(board, blockr.dm:::dm_read_pin_name(f)))
  expect_length(list.files(cache_dir), 0L)
})

test_that("a failing board falls back to a direct read", {
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")
  skip_if_not_installed("pins")

  src_dir <- withr::local_tempdir()
  f <- write_sas_fixture(src_dir)
  broken <- structure(list(), class = c("pins_board_folder", "pins_board"))

  expect_no_error(
    res <- dm_read_tables(f, cache_dir = "", cache_board = broken)
  )
  expect_identical(res[[1]]$USUBJID, c("01-001", "01-002", "01-003"))
})

test_that("same-named tables in different directories get distinct pins", {
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")
  skip_if_not_installed("pins")

  dir_a <- withr::local_tempdir()
  dir_b <- withr::local_tempdir()
  fa <- write_sas_fixture(dir_a)
  fb <- write_sas_fixture(dir_b)

  expect_false(identical(
    blockr.dm:::dm_read_pin_name(fa),
    blockr.dm:::dm_read_pin_name(fb)
  ))
})

test_that("a non-board value in the option counts as unset", {
  withr::local_options(blockr.dm_read_cache_board = "not-a-board")
  expect_null(blockr.dm:::dm_read_cache_board())
})

test_that("every route into a file goes through the one reader", {

  # The emitted code names `dm_read_tables()` whether or not a cache is
  # configured. It used to inline an anonymous reader when there was no
  # backend, which meant those reads were never measured -- so the block had
  # nothing honest to say about how long they took -- and put a fifteen-line
  # lambda in the exported script.
  withr::local_options(
    blockr.dm_read_cache_dir = NULL,
    blockr.dm_read_cache_board = NULL
  )
  plain <- dm_read_tables_expr()
  expect_identical(plain[[1]], quote(blockr.dm::dm_read_tables))
  expect_null(plain$cache_dir)

  withr::local_options(blockr.dm_read_cache_dir = "/some/cache")
  cached <- dm_read_tables_expr()
  expect_identical(cached[[1]], quote(blockr.dm::dm_read_tables))
  expect_identical(cached$cache_dir, "/some/cache")

  # The zip path opts out of caching even when the option is set -- its
  # files live in a temp dir -- but it is still the same reader.
  uncached <- dm_read_tables_expr(cache = FALSE)
  expect_identical(uncached[[1]], quote(blockr.dm::dm_read_tables))
  expect_identical(uncached$cache_dir, "")
  expect_null(uncached$cache_board)
})

test_that("the block routes a directory of SAS files through the cache", {
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")

  # The layer every other test in this file skips: the helpers were always
  # right, but nothing checked that the BLOCK reaches them.
  src_dir <- withr::local_tempdir()
  cache_dir <- withr::local_tempdir()
  write_sas_fixture(src_dir, "adsl.sas7bdat")
  write_sas_fixture(src_dir, "adae.sas7bdat")

  withr::local_options(blockr.dm_read_cache_dir = cache_dir)

  read_once <- function() {
    block <- new_dm_read_block(
      path = src_dir, selected_tables = c("adsl", "adae")
    )
    out <- NULL
    shiny::testServer(
      blockr.core:::get_s3_method("block_server", block),
      {
        session$flushReact()
        out <<- list(
          expr = rlang::expr_text(session$returned$expr()),
          result = session$returned$result(),
          stats = blockr.dm:::dm_read_stats()
        )
      },
      args = list(x = block, data = list())
    )
    out
  }

  cold <- read_once()

  expect_match(cold$expr, "dm_read_tables", fixed = TRUE)
  # expr_text() renders the call as R SOURCE, so a path's backslashes come back
  # doubled and the raw value never appears in it. encodeString() is the same
  # rendering, and a no-op on a separator that needs no escaping -- so this
  # compares like with like on Windows without changing what it asserts
  # anywhere else.
  expect_match(cold$expr, encodeString(cache_dir), fixed = TRUE)
  expect_s3_class(cold$result, "dm")
  expect_setequal(cold$stats$source, "converted")
  expect_length(list.files(cache_dir, pattern = "\\.parquet$"), 2L)

  warm <- read_once()

  expect_setequal(warm$stats$source, "cache-dir")
  expect_setequal(
    names(dm::dm_get_tables(warm$result)), c("adsl", "adae")
  )
})

test_that("cached and uncached reads return the same object", {
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")

  # A cache is a speedup, never a change of type: when these two drifted, the
  # same .sas7bdat came back a data.frame with caching off and a tbl_df with
  # it on, so switching a cache on silently changed the data downstream.
  src_dir <- withr::local_tempdir()
  cache_dir <- withr::local_tempdir()
  f <- write_sas_fixture(src_dir)

  files <- f
  plain <- eval(dm_read_tables_expr(cache = FALSE))[[1]]

  cached_cold <- dm_read_tables(f, cache_dir = cache_dir)[[1]]
  cached_warm <- dm_read_tables(f, cache_dir = cache_dir)[[1]]

  expect_identical(class(plain), class(cached_cold))
  expect_identical(class(plain), class(cached_warm))
  expect_identical(plain, cached_cold)
  expect_identical(
    lapply(plain, attributes), lapply(cached_warm, attributes)
  )
})

test_that("the read report says which route the data took", {
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")

  src_dir <- withr::local_tempdir()
  cache_dir <- withr::local_tempdir()
  f <- write_sas_fixture(src_dir)

  withr::local_options(blockr.dm_read_cache_dir = cache_dir)

  dm_read_tables(f, cache_dir = cache_dir)
  cold <- blockr.dm:::dm_read_status("adxx")
  expect_identical(cold$state, "converted")
  expect_match(cold$text, "^1 table in ")
  expect_match(cold$text, "converted from source")

  dm_read_tables(f, cache_dir = cache_dir)
  warm <- blockr.dm:::dm_read_status("adxx")
  expect_identical(warm$state, "cached")
  expect_match(warm$text, "from parquet cache")
})

test_that("the read report names the missing cache rather than staying quiet", {

  # No backend configured: the read still works, which is exactly why nobody
  # notices it is uncached until someone asks why the app takes a minute.
  withr::local_options(
    blockr.dm_read_cache_dir = NULL,
    blockr.dm_read_cache_board = NULL
  )
  blockr.dm:::dm_read_stats_record(list())

  # Nothing measured, so no duration is quoted: an Excel workbook or an RDS
  # file is one call, not a per-file loop, and inventing a number for it is
  # how the report ends up reporting how long a dock tab stayed shut.
  status <- blockr.dm:::dm_read_status(c("adsl", "adae"))
  expect_identical(status$state, "plain")
  expect_match(status$text, "no parquet cache configured")
  expect_match(status$text, "^2 tables ")
  expect_no_match(status$text, " in ")
})

test_that("an unreachable board is reported, not swallowed", {
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")
  skip_if_not_installed("pins")

  src_dir <- withr::local_tempdir()
  f <- write_sas_fixture(src_dir)
  broken <- structure(list(), class = c("pins_board_folder", "pins_board"))

  res <- dm_read_tables(f, cache_dir = "", cache_board = broken)

  expect_identical(res[[1]]$USUBJID, c("01-001", "01-002", "01-003"))

  status <- blockr.dm:::dm_read_status("adxx")
  expect_identical(status$state, "error")
  expect_match(status$text, "cache unavailable")
})

test_that("dm_read_tables_expr emits a bare call for a board backend", {
  skip_if_not_installed("pins")

  withr::local_options(
    blockr.dm_read_cache_dir = NULL,
    blockr.dm_read_cache_board = pins::board_temp()
  )
  ex <- dm_read_tables_expr()
  expect_identical(ex, quote(blockr.dm::dm_read_tables(files)))

  # zip path still opts out
  expect_identical(
    dm_read_tables_expr(cache = FALSE),
    quote(blockr.dm::dm_read_tables(files, cache_dir = "", cache_board = NULL))
  )
})
