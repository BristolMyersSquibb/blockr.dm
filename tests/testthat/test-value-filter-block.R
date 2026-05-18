# Tests for Filter Block

# Construction ----------------------------------------------------------------

test_that("value_filter_block constructor creates correct class", {
  blk <- new_value_filter_block()
  expect_s3_class(blk, c("value_filter_block", "transform_block", "block"))
})

test_that("constructor accepts default empty state", {
  blk <- new_value_filter_block()
  expect_s3_class(blk, "value_filter_block")
})

# Migration -------------------------------------------------------------------

test_that("migrate_value_filter_state handles NULL", {
  expect_equal(migrate_value_filter_state(NULL), list(columns = list()))
})

test_that("migrate_value_filter_state passes through new-shape state", {
  new_state <- list(columns = list(
    list(name = "Species", mode = "single", values = "setosa")
  ))
  expect_identical(migrate_value_filter_state(new_state), new_state)
})

test_that("migrate_value_filter_state converts old parallel-list shape", {
  old <- list(
    columns = "Species",
    modes   = list(Species = "single"),
    values  = list(Species = "setosa")
  )
  out <- migrate_value_filter_state(old)
  expect_equal(out, list(columns = list(
    list(name = "Species", mode = "single", values = "setosa")
  )))
})

test_that("migrate_value_filter_state handles multi-column old shape", {
  old <- list(
    columns = c("Species", "Sepal.Length"),
    modes   = list(Species = "single", Sepal.Length = "multi"),
    values  = list(Species = "setosa", Sepal.Length = c("5.0", "5.1"))
  )
  out <- migrate_value_filter_state(old)
  expect_length(out$columns, 2L)
  expect_equal(out$columns[[1]]$name, "Species")
  expect_equal(out$columns[[2]]$values, c("5.0", "5.1"))
})

test_that("migrate_value_filter_state defaults missing mode to single", {
  old <- list(columns = "Species", modes = list(), values = list())
  out <- migrate_value_filter_state(old)
  expect_equal(out$columns[[1]]$mode, "single")
  expect_equal(out$columns[[1]]$values, character())
})

# make_filter_block_expr: data frame path -------------------------------------

test_that("empty state produces pass-through expression on data frame", {
  expr <- make_filter_block_expr(list(), iris)
  expect_identical(expr, bquote(dplyr::filter(data, TRUE)))
})

test_that("multi-select empty values skip the column", {
  cols <- list(list(name = "Species", mode = "multi", values = character()))
  expr <- make_filter_block_expr(cols, iris)
  expect_identical(expr, bquote(dplyr::filter(data, TRUE)))
})

test_that("single-value filter uses %in%", {
  cols <- list(list(name = "Species", mode = "single", values = "setosa"))
  expr <- make_filter_block_expr(cols, iris)
  expected <- as.call(list(
    quote(dplyr::filter),
    quote(data),
    bquote(Species %in% "setosa")
  ))
  expect_identical(expr, expected)
})

test_that("multi-value filter uses %in% with vector", {
  cols <- list(list(name = "Species", mode = "multi",
                    values = c("setosa", "versicolor")))
  expr <- make_filter_block_expr(cols, iris)
  vec <- c("setosa", "versicolor")
  expected <- as.call(list(
    quote(dplyr::filter),
    quote(data),
    bquote(Species %in% .(vec))
  ))
  expect_identical(expr, expected)
})

test_that("numeric columns coerce string values to numeric", {
  df <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"))
  cols <- list(list(name = "x", mode = "single", values = "2"))
  expr <- make_filter_block_expr(cols, df)
  expected <- as.call(list(
    quote(dplyr::filter),
    quote(data),
    bquote(x %in% 2)
  ))
  expect_identical(expr, expected)
})

test_that("multiple columns combine with &", {
  cols <- list(
    list(name = "Species",      mode = "single", values = "setosa"),
    list(name = "Sepal.Length", mode = "multi",  values = c("5.0", "5.1"))
  )
  expr <- make_filter_block_expr(cols, iris)
  nums <- c(5, 5.1)
  inner <- bquote(Species %in% "setosa" & Sepal.Length %in% .(nums))
  expected <- as.call(list(quote(dplyr::filter), quote(data), inner))
  expect_equal(expr, expected)
})

test_that("integer-column multi filter coerces to integer", {
  df <- data.frame(x = c(1L, 2L, 3L))
  cols <- list(list(name = "x", mode = "multi", values = c("1", "3")))
  expr <- make_filter_block_expr(cols, df)
  ints <- c(1L, 3L)
  expected <- as.call(list(
    quote(dplyr::filter),
    quote(data),
    bquote(x %in% .(ints))
  ))
  expect_identical(expr, expected)
})

# make_filter_block_expr: dm path ---------------------------------------------

skip_if_no_dm <- function() {
  testthat::skip_if_not_installed("dm")
}

mk_demo_dm <- function() {
  dm::as_dm(list(
    policies = data.frame(
      policy_id = c("P001", "P002", "P003"),
      status    = c("active", "active", "lapsed"),
      stringsAsFactors = FALSE
    ),
    claims = data.frame(
      claim_id   = c("C1", "C2", "C3", "C4"),
      policy_id  = c("P001", "P001", "P002", "P003"),
      claim_year = c(2024L, 2025L, 2024L, 2023L),
      stringsAsFactors = FALSE
    )
  ))
}

test_that("empty state on dm input returns dm::dm_filter(data)", {
  skip_if_no_dm()
  expr <- make_filter_block_expr(list(), mk_demo_dm())
  expect_identical(expr, bquote(dm::dm_filter(data)))
})

test_that("single dm filter wraps one dm_filter call with named table arg", {
  skip_if_no_dm()
  cols <- list(list(name = "policy_id", table = "policies",
                    mode = "single", values = "P001"))
  expr <- make_filter_block_expr(cols, mk_demo_dm())
  expected <- call("dm_filter", quote(data))
  expected[["policies"]] <- bquote(policy_id %in% "P001")
  expected[[1L]] <- quote(dm::dm_filter)
  expect_identical(expr, expected)
})

test_that("multi-table dm filter chains dm_filter calls", {
  skip_if_no_dm()
  cols <- list(
    list(name = "policy_id",  table = "policies", mode = "single",
         values = "P001"),
    list(name = "claim_year", table = "claims",   mode = "multi",
         values = c("2024", "2025"))
  )
  expr <- make_filter_block_expr(cols, mk_demo_dm())
  # Verify the outer call targets the claims table.
  expect_true(is.call(expr))
  expect_identical(expr[[1L]], quote(dm::dm_filter))
  # Outer named-arg should be `claims`.
  expect_true("claims" %in% names(expr))
  # Inner (1st arg of outer) is itself a dm_filter call on policies.
  inner <- expr[[2L]]
  expect_identical(inner[[1L]], quote(dm::dm_filter))
  expect_true("policies" %in% names(inner))
})

test_that("dm filter executes end-to-end against a real dm", {
  skip_if_no_dm()
  d <- mk_demo_dm()
  cols <- list(list(name = "policy_id", table = "policies",
                    mode = "single", values = "P001"))
  expr <- make_filter_block_expr(cols, d)
  out <- eval(expr, list(data = d))
  # Both tables restrict via FK or named-arg semantics. policies → 1 row.
  pol <- as.data.frame(dm::dm_get_tables(out)$policies)
  expect_equal(nrow(pol), 1L)
  expect_equal(pol$policy_id, "P001")
})

test_that("integer-column dm filter coerces values to integer", {
  skip_if_no_dm()
  d <- mk_demo_dm()
  cols <- list(list(name = "claim_year", table = "claims",
                    mode = "multi", values = c("2024", "2025")))
  expr <- make_filter_block_expr(cols, d)
  # The claims condition should carry integers, not strings.
  ints <- c(2024L, 2025L)
  expected_cond <- bquote(claim_year %in% .(ints))
  expect_identical(expr[["claims"]], expected_cond)
})

# enforce_single_rule ---------------------------------------------------------

test_that("enforce_single_rule fills empty single-select with first value", {
  s <- list(columns = list(
    list(name = "Species", mode = "single", values = character())
  ))
  out <- enforce_single_rule(s, iris)
  expect_equal(out$columns[[1]]$values, "setosa")
})

test_that("enforce_single_rule leaves non-empty selections alone", {
  s <- list(columns = list(
    list(name = "Species", mode = "single", values = "virginica")
  ))
  out <- enforce_single_rule(s, iris)
  expect_equal(out$columns[[1]]$values, "virginica")
})

test_that("enforce_single_rule does not touch multi-select columns", {
  s <- list(columns = list(
    list(name = "Species", mode = "multi", values = character())
  ))
  out <- enforce_single_rule(s, iris)
  expect_equal(out$columns[[1]]$values, character())
})

test_that("enforce_single_rule drops entries missing from upstream df", {
  s <- list(columns = list(
    list(name = "NotAColumn", mode = "single", values = "x")
  ))
  out <- enforce_single_rule(s, iris)
  expect_length(out$columns, 0L)
})

test_that("enforce_single_rule drops dm entries whose table has gone away", {
  skip_if_no_dm()
  s <- list(columns = list(
    list(name = "policy_id", table = "missing_table",
         mode = "single", values = "P001")
  ))
  out <- enforce_single_rule(s, mk_demo_dm())
  expect_length(out$columns, 0L)
})

test_that("enforce_single_rule on dm fills empty single-select", {
  skip_if_no_dm()
  s <- list(columns = list(
    list(name = "policy_id", table = "policies", mode = "single",
         values = character())
  ))
  out <- enforce_single_rule(s, mk_demo_dm())
  expect_equal(out$columns[[1]]$values, "P001")
})

# build_column_options --------------------------------------------------------

test_that("build_column_options on df returns flat metadata", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  attr(df$x, "label") <- "X axis"
  meta <- build_column_options(df)
  expect_false(meta$is_dm)
  expect_equal(meta$columns[[1]], list(value = "x", label = "X axis"))
  expect_equal(meta$columns[[2]], list(value = "y", label = ""))
})

test_that("build_column_options on df returns plain values for unlabelled columns", {
  df <- data.frame(g = c("b", "a", "b", "c"), stringsAsFactors = FALSE)
  meta <- build_column_options(df)
  expect_equal(meta$values$g, list("a", "b", "c"))
})

test_that("build_column_options on df honors haven-style value labels", {
  df <- data.frame(sex = c(1L, 2L, 1L))
  attr(df$sex, "labels") <- c(Male = 1L, Female = 2L)
  meta <- build_column_options(df)
  expect_equal(meta$values$sex[[1]], list(value = "1", label = "Male"))
  expect_equal(meta$values$sex[[2]], list(value = "2", label = "Female"))
})

test_that("build_column_options on dm carries table/column fields", {
  skip_if_no_dm()
  meta <- build_column_options(mk_demo_dm())
  expect_true(meta$is_dm)
  # Each entry has table + column fields and a qualified value key.
  expect_true(all(vapply(meta$columns, function(c) !is.null(c$table),
                         logical(1))))
  pol_entry <- Filter(
    function(c) c$table == "policies" && c$column == "policy_id",
    meta$columns
  )[[1]]
  expect_equal(pol_entry$value, "policies.policy_id")
  expect_true("policies.policy_id" %in% names(meta$values))
  expect_equal(meta$values$`policies.policy_id`,
               list("P001", "P002", "P003"))
})

# normalize_state_for_json ----------------------------------------------------

test_that("normalize_state_for_json wraps length-1 values for JSON", {
  s <- list(columns = list(
    list(name = "x", mode = "single", values = "a")
  ))
  out <- normalize_state_for_json(s)
  expect_type(out$columns[[1]]$values, "list")
  expect_equal(out$columns[[1]]$values, list("a"))
})

test_that("normalize_state_for_json strips empty table fields", {
  s <- list(columns = list(
    list(name = "x", table = "", mode = "single", values = "a")
  ))
  out <- normalize_state_for_json(s)
  expect_null(out$columns[[1]]$table)
})

test_that("normalize_state_for_json keeps non-empty table fields", {
  s <- list(columns = list(
    list(name = "policy_id", table = "policies", mode = "single",
         values = "P001")
  ))
  out <- normalize_state_for_json(s)
  expect_equal(out$columns[[1]]$table, "policies")
})

# Block server (df path, end-to-end) ------------------------------------------

test_that("block server result filters a data frame by single column", {
  blk <- new_value_filter_block(state = list(columns = list(
    list(name = "Species", mode = "single", values = "setosa")
  )))
  testthat::skip_if_not_installed("blockr.core")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      # The block's expr should resolve to a filter on Species %in% "setosa".
      e <- session$returned$expr()
      out <- eval(e, list(data = iris))
      expect_true(all(out$Species == "setosa"))
      expect_equal(nrow(out), 50L)
    },
    args = list(x = blk, data = list(data = function() iris))
  )
})

test_that("block server result filters a dm by FK cascade", {
  skip_if_no_dm()
  testthat::skip_if_not_installed("blockr.core")
  d <- mk_demo_dm()
  blk <- new_value_filter_block(state = list(columns = list(
    list(name = "policy_id", table = "policies", mode = "single",
         values = "P001")
  )))
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      e <- session$returned$expr()
      out <- eval(e, list(data = d))
      pol <- as.data.frame(dm::dm_get_tables(out)$policies)
      expect_equal(nrow(pol), 1L)
      expect_equal(pol$policy_id, "P001")
    },
    args = list(x = blk, data = list(data = function() d))
  )
})
