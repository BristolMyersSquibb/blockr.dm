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
