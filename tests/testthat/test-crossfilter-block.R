test_that("new_crossfilter_block constructs a transform block", {
  blk <- new_crossfilter_block()
  expect_s3_class(blk, "crossfilter_block")
  expect_s3_class(blk, "transform_block")
})

test_that("new_crossfilter_block stores initial state", {
  blk <- new_crossfilter_block(
    active_dims = list(adsl = c("SEX", "AGE")),
    filters = list(adsl = list(SEX = list("F"))),
    range_filters = list(adsl = list(AGE = c(40, 60))),
    measure = "adsl.AGE",
    agg_func = "mean"
  )
  state <- blockr.core:::initial_block_state(blk)
  expect_equal(state$active_dims, list(adsl = c("SEX", "AGE")))
  expect_equal(state$filters, list(adsl = list(SEX = list("F"))))
  expect_equal(state$range_filters, list(adsl = list(AGE = c(40, 60))))
  expect_equal(state$measure, "adsl.AGE")
  expect_equal(state$agg_func, "mean")
})

test_that("new_js_crossfilter_block is an alias for new_crossfilter_block", {
  blk <- new_js_crossfilter_block()
  expect_s3_class(blk, "crossfilter_block")
  expect_identical(new_js_crossfilter_block, new_crossfilter_block)
})

test_that("lookup builders include measure when table name starts with dot", {
  # data.frame inputs are wrapped as `dm(.tbl = df)` — the table name is
  # `.tbl` (leading dot). The measure spec is then `.tbl.<column>`, and a
  # regex like `^[^.]+\\.` failed to strip the prefix, leaving the measure
  # column out of the lookup and producing all-zero counts on the JS side.
  df <- data.frame(
    age_band    = c("18-29", "30-44", "45-59", "18-29", "30-44"),
    sex         = c("M", "F", "M", "F", "M"),
    Sum_at_Risk = c(100, 200, 300, 400, 500),
    stringsAsFactors = FALSE
  )

  ind <- build_lookups_independent(
    tables      = list(.tbl = df),
    active_dims = list(.tbl = c("age_band", "sex")),
    measure_col = ".tbl.Sum_at_Risk"
  )
  expect_true("Sum_at_Risk" %in% names(ind$lookups[[".tbl"]]))

  # build_lookups_flat needs at least one FK — exercise it on a two-table dm
  # where the parent name also starts with a dot.
  parent <- data.frame(
    pid = 1:3,
    .extra = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  child <- data.frame(
    pid         = c(1, 1, 2, 3),
    age_band    = c("18-29", "30-44", "45-59", "18-29"),
    Sum_at_Risk = c(100, 200, 300, 400),
    stringsAsFactors = FALSE
  )
  d <- dm::dm(.parent = parent, .child = child) |>
    dm::dm_add_pk(.parent, pid) |>
    dm::dm_add_fk(.child, pid, .parent)

  flat <- build_lookups_flat(
    dm_obj      = d,
    active_dims = list(.child = "age_band"),
    measure_col = ".child.Sum_at_Risk"
  )
  expect_true("Sum_at_Risk" %in% names(flat$lookups[[".child"]]))
})
