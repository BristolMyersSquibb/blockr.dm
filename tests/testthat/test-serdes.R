# Helper: serialize -> JSON -> deserialize roundtrip
ser_deser <- function(block) {
  ser <- blockr_ser(block)
  json <- jsonlite::toJSON(ser, null = "null", auto_unbox = TRUE)
  parsed <- jsonlite::fromJSON(
    as.character(json),
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
  blockr_deser(parsed)
}

# Helper: compare state values (the meaningful part for restore)
expect_state_equal <- function(original, restored) {
  orig_state <- blockr.core:::initial_block_state(original)
  rest_state <- blockr.core:::initial_block_state(restored)
  expect_equal(names(orig_state), names(rest_state))
  for (nm in names(orig_state)) {
    expect_equal(
      orig_state[[nm]], rest_state[[nm]],
      label = paste("state$", nm)
    )
  }
}

test_that("ser/deser dm_block", {
  for (infer in c(TRUE, FALSE)) {
    blk <- new_dm_block(infer_keys = infer)
    restored <- ser_deser(blk)
    expect_s3_class(restored, class(blk))
    expect_state_equal(blk, restored)
  }
})

test_that("ser/deser dm_select_block", {
  blk <- new_dm_select_block()
  restored <- ser_deser(blk)
  expect_s3_class(restored, class(blk))

  blk2 <- new_dm_select_block(tables = c("adsl", "adae"))
  restored2 <- ser_deser(blk2)
  state <- blockr.core:::initial_block_state(restored2)
  expect_equal(state$tables, c("adsl", "adae"))
})

test_that("ser/deser dm_add_keys_block", {
  blk <- new_dm_add_keys_block()
  restored <- ser_deser(blk)
  expect_s3_class(restored, class(blk))

  blk2 <- new_dm_add_keys_block(
    pk_table = "adsl", pk_column = "USUBJID",
    fk_tables = c("adae", "adlb"), fk_column = "USUBJID"
  )
  restored2 <- ser_deser(blk2)
  state <- blockr.core:::initial_block_state(restored2)
  expect_equal(state$pk_table, "adsl")
  expect_equal(state$pk_column, "USUBJID")
  expect_equal(state$fk_tables, c("adae", "adlb"))
  expect_equal(state$fk_column, "USUBJID")
})

test_that("ser/deser dm_filter_block", {
  blk <- new_dm_filter_block()
  restored <- ser_deser(blk)
  expect_s3_class(restored, class(blk))
  expect_state_equal(blk, restored)

  state_in <- list(
    conditions = list(list(
      type = "values", column = "AEDECOD",
      values = list("Headache"), mode = "include"
    )),
    operator = "&"
  )
  blk2 <- new_dm_filter_block(table = "adae", state = state_in)
  restored2 <- ser_deser(blk2)
  state <- blockr.core:::initial_block_state(restored2)
  expect_equal(state$table, "adae")
  expect_equal(state$state$operator, "&")
  expect_equal(state$state$conditions[[1L]]$column, "AEDECOD")
})

test_that("ser/deser dm_pull_block", {
  blk <- new_dm_pull_block()
  restored <- ser_deser(blk)
  expect_s3_class(restored, class(blk))
  expect_state_equal(blk, restored)

  blk2 <- new_dm_pull_block(table = "adsl")
  restored2 <- ser_deser(blk2)
  state <- blockr.core:::initial_block_state(restored2)
  expect_equal(state$table, "adsl")
})

test_that("ser/deser dm_flatten_block", {
  blk <- new_dm_flatten_block()
  restored <- ser_deser(blk)
  expect_s3_class(restored, class(blk))

  blk2 <- new_dm_flatten_block(
    start_table = "adsl",
    include_tables = c("adae", "adlb"),
    join_type = "inner",
    recursive = FALSE
  )
  restored2 <- ser_deser(blk2)
  state <- blockr.core:::initial_block_state(restored2)
  expect_equal(state$start_table, "adsl")
  expect_equal(state$include_tables, c("adae", "adlb"))
  expect_equal(state$join_type, "inner")
  expect_false(state$recursive)
})

test_that("ser/deser crossfilter_block", {
  blk <- new_crossfilter_block()
  restored <- ser_deser(blk)
  expect_s3_class(restored, class(blk))
})

