# --- Test helper ---
make_test_dm <- function() {
  adsl <- data.frame(
    USUBJID = paste0("SUBJ-", 1:5),
    SEX = c("M", "F", "M", "F", "M"),
    AGE = c(45, 52, 38, 61, 29),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-2", "SUBJ-3", "SUBJ-4"),
    AESEV = c("MILD", "SEVERE", "MODERATE", "MILD", "SEVERE"),
    stringsAsFactors = FALSE
  )
  dm::dm(adsl = adsl, adae = adae) |>
    dm::dm_add_pk(adsl, USUBJID) |>
    dm::dm_add_fk(adae, USUBJID, adsl)
}

# ==============================================================================
# Constructor
# ==============================================================================

test_that("dm_crossfilter_block constructor", {
  block <- new_dm_crossfilter_block()
  expect_s3_class(
    block,
    c("dm_crossfilter_block", "transform_block", "block")
  )

  # With all three state params
  block2 <- new_dm_crossfilter_block(
    active_dims = list(adsl = c("SEX", "AGE")),
    filters = list(adsl = list(SEX = "F")),
    range_filters = list(adsl = list(AGE = c(40, 60)))
  )
  expect_s3_class(block2, "dm_crossfilter_block")
})

# ==============================================================================
# State Restoration (testServer)
# ==============================================================================

test_that("dm_crossfilter_block state contains filters field", {
  block <- new_dm_crossfilter_block(
    filters = list(adsl = list(SEX = "F"))
  )
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state
      expect_true("filters" %in% names(state))
      expect_true("range_filters" %in% names(state))
      expect_true("active_dims" %in% names(state))
      expect_true(is.function(state$filters))
      expect_true(is.function(state$range_filters))
      expect_true(is.function(state$active_dims))
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})

test_that("dm_crossfilter_block filters initialized from constructor", {
  block <- new_dm_crossfilter_block(
    filters = list(adsl = list(SEX = "F"))
  )
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state
      filters <- state$filters()
      expect_type(filters, "list")
      expect_equal(filters$adsl$SEX, "F")
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})

test_that("dm_crossfilter_block range_filters initialized from constructor", {
  block <- new_dm_crossfilter_block(
    range_filters = list(adsl = list(AGE = c(40, 60)))
  )
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state
      rng <- state$range_filters()
      expect_type(rng, "list")
      expect_equal(rng$adsl$AGE, c(40, 60))
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})

test_that("dm_crossfilter_block active_dims initialized from constructor", {
  block <- new_dm_crossfilter_block(
    active_dims = list(adsl = c("SEX", "AGE"), adae = "AESEV")
  )
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state
      ad <- state$active_dims()
      expect_equal(ad$adsl, c("SEX", "AGE"))
      expect_equal(ad$adae, "AESEV")
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})

# ==============================================================================
# Expression Generation
# ==============================================================================

test_that("dm_crossfilter_block no filters produces identity expr", {
  block <- new_dm_crossfilter_block()
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      expr <- session$returned$expr()
      expr_str <- deparse(expr)
      expect_true(any(grepl("identity", expr_str)))
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})

test_that("dm_crossfilter_block categorical filter generates dm_filter expr", {
  block <- new_dm_crossfilter_block(
    filters = list(adsl = list(SEX = "F"))
  )
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      expr <- session$returned$expr()
      expr_str <- deparse(expr)
      expect_true(any(grepl("dm_filter", expr_str)))
      expect_true(any(grepl("SEX", expr_str)))
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})

test_that("dm_crossfilter_block range filter generates dm_filter expr", {
  block <- new_dm_crossfilter_block(
    range_filters = list(adsl = list(AGE = c(40, 60)))
  )
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      expr <- session$returned$expr()
      expr_str <- deparse(expr)
      expect_true(any(grepl("dm_filter", expr_str)))
      expect_true(any(grepl("AGE", expr_str)))
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})

# ==============================================================================
# Result Correctness
# ==============================================================================

test_that("dm_crossfilter_block no filters returns unchanged dm", {
  block <- new_dm_crossfilter_block()
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_s3_class(result, "dm")

      adsl <- dm::pull_tbl(result, adsl)
      adae <- dm::pull_tbl(result, adae)
      expect_equal(nrow(adsl), 5)
      expect_equal(nrow(adae), 5)
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})

test_that("dm_crossfilter_block categorical filter on adsl filters correctly", {
  # Filter SEX = "F" in adsl: SUBJ-2 and SUBJ-4
  # adae cascades: SUBJ-2 (MODERATE), SUBJ-4 (SEVERE) => 2 rows
  block <- new_dm_crossfilter_block(
    filters = list(adsl = list(SEX = "F"))
  )
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_s3_class(result, "dm")

      adsl <- dm::pull_tbl(result, adsl)
      expect_true(all(adsl$SEX == "F"))
      expect_equal(nrow(adsl), 2)

      adae <- dm::pull_tbl(result, adae)
      expect_true(all(adae$USUBJID %in% c("SUBJ-2", "SUBJ-4")))
      expect_equal(nrow(adae), 2)
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})

test_that("dm_crossfilter_block categorical filter on adae filters correctly", {
  # Filter AESEV = "SEVERE" in adae: SUBJ-1 (row 2), SUBJ-4 (row 5) => 2 rows
  # adsl cascades: SUBJ-1 and SUBJ-4
  block <- new_dm_crossfilter_block(
    filters = list(adae = list(AESEV = "SEVERE"))
  )
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_s3_class(result, "dm")

      adae <- dm::pull_tbl(result, adae)
      expect_true(all(adae$AESEV == "SEVERE"))
      expect_equal(nrow(adae), 2)

      adsl <- dm::pull_tbl(result, adsl)
      expect_true(all(adsl$USUBJID %in% c("SUBJ-1", "SUBJ-4")))
      expect_equal(nrow(adsl), 2)
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})

test_that("dm_crossfilter_block range filter on adsl filters correctly", {
  # Filter AGE in [40, 60] in adsl: SUBJ-1 (45), SUBJ-2 (52) => 2 rows
  # adae cascades: SUBJ-1 (2 rows), SUBJ-2 (1 row) => 3 rows
  block <- new_dm_crossfilter_block(
    range_filters = list(adsl = list(AGE = c(40, 60)))
  )
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_s3_class(result, "dm")

      adsl <- dm::pull_tbl(result, adsl)
      expect_true(all(adsl$AGE >= 40 & adsl$AGE <= 60))
      expect_equal(nrow(adsl), 2)

      adae <- dm::pull_tbl(result, adae)
      expect_true(all(adae$USUBJID %in% c("SUBJ-1", "SUBJ-2")))
      expect_equal(nrow(adae), 3)
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})

test_that("dm_crossfilter_block multi-value categorical filter", {
  # Filter AESEV %in% c("MILD", "SEVERE") => 4 rows in adae
  # Subjects with those AEs: SUBJ-1, SUBJ-3, SUBJ-4
  block <- new_dm_crossfilter_block(
    filters = list(adae = list(AESEV = c("MILD", "SEVERE")))
  )
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()

      adae <- dm::pull_tbl(result, adae)
      expect_true(all(adae$AESEV %in% c("MILD", "SEVERE")))
      expect_equal(nrow(adae), 4)

      adsl <- dm::pull_tbl(result, adsl)
      expect_true(all(adsl$USUBJID %in% c("SUBJ-1", "SUBJ-3", "SUBJ-4")))
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})

# ==============================================================================
# Clear Filters
# ==============================================================================

test_that("dm_crossfilter_block clear_filters does not crash", {
  # Note: clear_filters is an actionButton rendered inside renderUI,
  # so testServer cannot fully simulate it. We verify it doesn't crash.
  block <- new_dm_crossfilter_block(
    active_dims = list(adsl = "SEX"),
    filters = list(adsl = list(SEX = "F")),
    range_filters = list(adsl = list(AGE = c(40, 60)))
  )
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Before clearing: filters are active
      expr_before <- session$returned$expr()
      expect_true(any(grepl("dm_filter", deparse(expr_before))))

      # Attempt clear_filters — may not trigger since button is inside renderUI
      session$setInputs(clear_filters = 1)
      session$flushReact()

      # Should not crash
      expect_type(session$returned, "list")
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})

# ==============================================================================
# Full State Round-Trip
# ==============================================================================

test_that("dm_crossfilter_block full state round-trip", {
  block <- new_dm_crossfilter_block(
    active_dims = list(adsl = c("SEX", "AGE"), adae = "AESEV"),
    filters = list(adsl = list(SEX = "F")),
    range_filters = list(adsl = list(AGE = c(40, 60)))
  )
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # All three state params accessible
      state <- session$returned$state
      expect_equal(state$active_dims()$adsl, c("SEX", "AGE"))
      expect_equal(state$active_dims()$adae, "AESEV")
      expect_equal(state$filters()$adsl$SEX, "F")
      expect_equal(state$range_filters()$adsl$AGE, c(40, 60))

      # Expr is dm_filter
      expr <- session$returned$expr()
      expr_str <- deparse(expr)
      expect_true(any(grepl("dm_filter", expr_str)))

      # Result is correctly filtered: SEX = "F" AND AGE in [40, 60]
      # SUBJ-2 (F, 52) matches both; SUBJ-4 (F, 61) fails AGE range
      result <- session$returned$result()
      adsl <- dm::pull_tbl(result, adsl)
      expect_equal(nrow(adsl), 1)
      expect_equal(adsl$USUBJID, "SUBJ-2")

      # adae cascades: only SUBJ-2's AE (MODERATE)
      adae <- dm::pull_tbl(result, adae)
      expect_equal(nrow(adae), 1)
      expect_equal(adae$USUBJID, "SUBJ-2")
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})

# ==============================================================================
# external_ctrl: state is writable reactiveVals
# ==============================================================================

test_that("dm_crossfilter_block state vars are reactiveVals (external_ctrl)", {
  block <- new_dm_crossfilter_block()
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state
      # With external_ctrl = TRUE, state vars should be reactiveVals
      expect_true(inherits(state$filters, "reactiveVal"))
      expect_true(inherits(state$range_filters, "reactiveVal"))
      expect_true(inherits(state$active_dims, "reactiveVal"))
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})

test_that("dm_crossfilter_block writing filters reactiveVal updates result", {
  block <- new_dm_crossfilter_block()
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Initially no filters: all 5 subjects
      result <- session$returned$result()
      adsl <- dm::pull_tbl(result, adsl)
      expect_equal(nrow(adsl), 5)

      # Simulate AI setting filters (write to reactiveVal)
      state <- session$returned$state
      state$filters(list(adsl = list(SEX = "F")))
      session$flushReact()

      result2 <- session$returned$result()
      adsl2 <- dm::pull_tbl(result2, adsl)
      expect_equal(nrow(adsl2), 2)
      expect_true(all(adsl2$SEX == "F"))
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})

test_that("dm_crossfilter_block writing range_filters reactiveVal updates result", {
  block <- new_dm_crossfilter_block()
  test_dm <- make_test_dm()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Simulate AI setting range filter
      state <- session$returned$state
      state$range_filters(list(adsl = list(AGE = c(40, 55))))
      session$flushReact()

      result <- session$returned$result()
      adsl <- dm::pull_tbl(result, adsl)
      expect_true(all(adsl$AGE >= 40 & adsl$AGE <= 55))
      # SUBJ-1 (45) and SUBJ-2 (52) match
      expect_equal(nrow(adsl), 2)
    },
    args = list(x = block, data = list(data = function() test_dm))
  )
})
