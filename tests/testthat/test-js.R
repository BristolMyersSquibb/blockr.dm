# The block JavaScript has its own tests (tests/js/, node --test). Running
# them from testthat too means devtools::test() sees them fail.
test_that("the block JavaScript passes its tests (node --test)", {
  skip_on_cran()
  node <- Sys.which("node")
  skip_if(!nzchar(node), "node is not installed")
  js_dir <- testthat::test_path("..", "js")
  skip_if(!dir.exists(js_dir), "tests/js is not present")
  files <- list.files(js_dir, pattern = "[.]test[.]js$", full.names = TRUE)
  out <- suppressWarnings(
    system2(node, c("--test", shQuote(files)), stdout = TRUE, stderr = TRUE)
  )
  status <- attr(out, "status")
  expect_true(
    is.null(status) || identical(status, 0L),
    info = paste(out, collapse = "\n")
  )
})
