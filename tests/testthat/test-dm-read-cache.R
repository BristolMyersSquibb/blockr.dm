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

test_that("dm_read_tables_expr stays generic R unless caching is active", {
  withr::local_options(blockr.dm_read_cache_dir = NULL)
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
