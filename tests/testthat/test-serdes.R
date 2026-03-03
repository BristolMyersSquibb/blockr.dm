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

test_that("ser/deser cdisc_dm_block", {
  for (keys in c(TRUE, FALSE)) {
    for (dedup in c(TRUE, FALSE)) {
      blk <- new_cdisc_dm_block(set_keys = keys, dedup_cols = dedup)
      restored <- ser_deser(blk)
      expect_s3_class(restored, class(blk))
      expect_state_equal(blk, restored)
    }
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

  blk2 <- new_dm_filter_block(table = "adae", expr = "AEDECOD == 'Headache'")
  restored2 <- ser_deser(blk2)
  state <- blockr.core:::initial_block_state(restored2)
  expect_equal(state$table, "adae")
  expect_equal(state$expr, "AEDECOD == 'Headache'")
})

test_that("ser/deser dm_filter_value_block", {
  blk <- new_dm_filter_value_block()
  restored <- ser_deser(blk)
  expect_s3_class(restored, class(blk))
  expect_state_equal(blk, restored)

  blk2 <- new_dm_filter_value_block(table = "adsl", column = "SEX", value = "M")
  restored2 <- ser_deser(blk2)
  state <- blockr.core:::initial_block_state(restored2)
  expect_equal(state$table, "adsl")
  expect_equal(state$column, "SEX")
  expect_equal(state$value, "M")
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

test_that("ser/deser dm_nested_view_block", {
  blk <- new_dm_nested_view_block()
  restored <- ser_deser(blk)
  expect_s3_class(restored, class(blk))

  blk2 <- new_dm_nested_view_block(root_table = "adsl")
  restored2 <- ser_deser(blk2)
  state <- blockr.core:::initial_block_state(restored2)
  expect_equal(state$root_table, "adsl")
})

test_that("ser/deser dm_read_block", {
  blk <- new_dm_read_block()
  restored <- ser_deser(blk)
  expect_s3_class(restored, class(blk))

  blk2 <- new_dm_read_block(path = "/tmp/test.xlsx")
  restored2 <- ser_deser(blk2)
  state <- blockr.core:::initial_block_state(restored2)
  expect_equal(state$path, "/tmp/test.xlsx")
})

test_that("ser/deser dm_write_block", {
  blk <- new_dm_write_block()
  restored <- ser_deser(blk)
  expect_s3_class(restored, class(blk))
  expect_state_equal(blk, restored)

  blk2 <- new_dm_write_block(
    directory = "/tmp",
    filename = "output",
    format = "csv",
    auto_write = TRUE
  )
  restored2 <- ser_deser(blk2)
  state <- blockr.core:::initial_block_state(restored2)
  expect_equal(state$directory, "/tmp")
  expect_equal(state$filename, "output")
  expect_equal(state$format, "csv")
  expect_true(state$auto_write)
})

test_that("ser/deser temporal_join_block", {
  blk <- new_temporal_join_block()
  restored <- ser_deser(blk)
  expect_s3_class(restored, class(blk))

  blk2 <- new_temporal_join_block(
    by = "USUBJID",
    left_date = "ASTDT",
    right_date = "AESTDT",
    window_days = 14,
    direction = "before"
  )
  restored2 <- ser_deser(blk2)
  state <- blockr.core:::initial_block_state(restored2)
  expect_equal(state$by, "USUBJID")
  expect_equal(state$left_date, "ASTDT")
  expect_equal(state$right_date, "AESTDT")
  expect_equal(state$window_days, 14)
  expect_equal(state$direction, "before")
})

test_that("ser/deser dm_temporal_join_block", {
  blk <- new_dm_temporal_join_block()
  restored <- ser_deser(blk)
  expect_s3_class(restored, class(blk))
  expect_state_equal(blk, restored)

  blk2 <- new_dm_temporal_join_block(
    left_table = "adsl",
    left_date = "TRTSDT",
    right_table = "adae",
    right_date = "AESTDT",
    window_days = 30,
    direction = "after"
  )
  restored2 <- ser_deser(blk2)
  state <- blockr.core:::initial_block_state(restored2)
  expect_equal(state$left_table, "adsl")
  expect_equal(state$left_date, "TRTSDT")
  expect_equal(state$right_table, "adae")
  expect_equal(state$right_date, "AESTDT")
  expect_equal(state$window_days, 30)
  expect_equal(state$direction, "after")
})

test_that("ser/deser crossfilter_block", {
  blk <- new_crossfilter_block()
  restored <- ser_deser(blk)
  expect_s3_class(restored, class(blk))
})

test_that("ser/deser dm_crossfilter_block", {
  blk <- new_dm_crossfilter_block()
  restored <- ser_deser(blk)
  expect_s3_class(restored, class(blk))
})
