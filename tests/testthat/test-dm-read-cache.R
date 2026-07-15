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

test_that("dm_read_tables_expr stays generic R unless caching is active", {
  withr::local_options(
    blockr.dm_read_cache_dir = NULL,
    blockr.dm_read_cache_board = NULL
  )
  plain <- dm_read_tables_expr()
  expect_identical(plain[[1]], as.name("lapply"))

  withr::local_options(blockr.dm_read_cache_dir = "/some/cache")
  cached <- dm_read_tables_expr()
  expect_identical(
    cached[[1]],
    quote(blockr.dm::dm_read_tables)
  )
  expect_identical(cached$cache_dir, "/some/cache")

  # zip path opts out even when the option is set
  expect_identical(dm_read_tables_expr(cache = FALSE)[[1]], as.name("lapply"))
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
  expect_identical(dm_read_tables_expr(cache = FALSE)[[1]], as.name("lapply"))
})
