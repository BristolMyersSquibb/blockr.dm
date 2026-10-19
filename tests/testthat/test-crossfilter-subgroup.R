sub_df <- function() {
  data.frame(
    ID = 1:4,
    TRT = c("Placebo", "Active", "Active", "Placebo"),
    SEX = c("F", "M", "F", "M"),
    AGE = c(60, 70, 80, 90),
    stringsAsFactors = FALSE
  )
}


test_that("subgroup round-trips through block state", {
  state <- blockr.core:::initial_block_state(
    new_crossfilter_block(pinned = "TRT", subgroup = "SEX")
  )
  expect_equal(state$subgroup, "SEX")
  expect_null(
    blockr.core:::initial_block_state(new_crossfilter_block())$subgroup
  )
})

test_that("the payload carries the subgroup, never the pinned column", {
  pay <- function(pinned, subgroup) {
    cf_payload(new_crossfilter_block(
      featured = c("TRT", "SEX"), pinned = pinned, subgroup = subgroup
    ), sub_df())
  }
  expect_equal(pay("TRT", "SEX")$subgroup, "SEX")
  expect_null(pay("SEX", "SEX")$subgroup)
  expect_null(pay("TRT", NULL)$subgroup)
})

test_that("a subgroup that is not an eligible column is not reported", {
  blk <- new_crossfilter_block(
    featured = c("TRT", "SEX"), pinned = "TRT", subgroup = "AGE"
  )
  expect_null(cf_payload(blk, sub_df())$subgroup)
})

test_that("the payload carries the subgroup's levels", {
  blk <- new_crossfilter_block(
    featured = c("TRT", "SEX"), pinned = "TRT", subgroup = "SEX"
  )
  msg <- cf_payload(blk, sub_df())
  expect_equal(
    vapply(msg$subgroup_levels, `[[`, character(1), "value"), c("F", "M")
  )
  swapped <- cf_payload(new_crossfilter_block(
    featured = c("TRT", "SEX"), pinned = "SEX", subgroup = "TRT"
  ), sub_df())
  expect_equal(swapped$pinned, "SEX")
  expect_equal(
    vapply(swapped$subgroup_levels, `[[`, character(1), "value"),
    c("Active", "Placebo")
  )
})



