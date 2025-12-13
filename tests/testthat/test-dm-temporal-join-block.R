test_that("dm_temporal_join block constructor", {
  block <- new_dm_temporal_join_block()
  expect_s3_class(
    block,
    c("dm_temporal_join_block", "transform_block", "block")
  )

  # Test with initial values
  block2 <- new_dm_temporal_join_block(
    left_table = "adae",
    left_date = "ASTDT",
    right_table = "adlb",
    right_date = "ADT",
    window_days = 7,
    direction = "after"
  )
  expect_s3_class(block2, "dm_temporal_join_block")
})

test_that("dm_temporal_join block returns data frame", {
  block <- new_dm_temporal_join_block(
    left_table = "adae",
    left_date = "ASTDT",
    right_table = "adlb",
    right_date = "ADT",
    window_days = 7,
    direction = "after"
  )

  adlb <- data.frame(
    USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-2"),
    ADT = as.Date(c("2024-01-08", "2024-01-20", "2024-01-15")),
    AVAL = c(45, 30, 28)
  )

  adae <- data.frame(
    USUBJID = c("SUBJ-1", "SUBJ-2"),
    AETERM = c("DIARRHOEA", "NAUSEA"),
    ASTDT = as.Date(c("2024-01-05", "2024-01-10"))
  )

  test_dm <- dm::dm(adae = adae, adlb = adlb)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Should return a data frame, not a dm
      expect_s3_class(result, "data.frame")
      expect_false(inherits(result, "dm"))

      # Should have days_diff column
      expect_true("days_diff" %in% names(result))

      # Should have 2 rows (SUBJ-1 lab on Jan 8, SUBJ-2 lab on Jan 15)
      expect_equal(nrow(result), 2)

      # Check days_diff values
      expect_equal(result$days_diff[result$USUBJID == "SUBJ-1"], 3)  # Jan 8 - Jan 5
      expect_equal(result$days_diff[result$USUBJID == "SUBJ-2"], 5)  # Jan 15 - Jan 10
    },
    args = list(
      x = block,
      data = list(data = function() test_dm)
    )
  )
})

test_that("dm_temporal_join block respects window_days", {
  # Test with smaller window that excludes some matches
  block <- new_dm_temporal_join_block(
    left_table = "adae",
    left_date = "ASTDT",
    right_table = "adlb",
    right_date = "ADT",
    window_days = 2,  # Only 2 days
    direction = "after"
  )

  adlb <- data.frame(
    USUBJID = c("SUBJ-1", "SUBJ-1"),
    ADT = as.Date(c("2024-01-06", "2024-01-10")),  # 1 day and 5 days after
    AVAL = c(45, 30)
  )

  adae <- data.frame(
    USUBJID = c("SUBJ-1"),
    AETERM = c("DIARRHOEA"),
    ASTDT = as.Date(c("2024-01-05"))
  )

  test_dm <- dm::dm(adae = adae, adlb = adlb)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Should only have 1 row (Jan 6 is within 2 days, Jan 10 is not)
      expect_equal(nrow(result), 1)
      expect_equal(result$days_diff, 1)
    },
    args = list(
      x = block,
      data = list(data = function() test_dm)
    )
  )
})

test_that("dm_temporal_join block direction 'before'", {
  block <- new_dm_temporal_join_block(
    left_table = "adae",
    left_date = "ASTDT",
    right_table = "adlb",
    right_date = "ADT",
    window_days = 5,
    direction = "before"
  )

  adlb <- data.frame(
    USUBJID = c("SUBJ-1", "SUBJ-1"),
    ADT = as.Date(c("2024-01-01", "2024-01-10")),  # 4 days before and 5 days after
    AVAL = c(25, 45)
  )

  adae <- data.frame(
    USUBJID = c("SUBJ-1"),
    AETERM = c("DIARRHOEA"),
    ASTDT = as.Date(c("2024-01-05"))
  )

  test_dm <- dm::dm(adae = adae, adlb = adlb)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Should only have 1 row (Jan 1 is before, Jan 10 is after)
      expect_equal(nrow(result), 1)
      expect_equal(result$days_diff, -4)  # Jan 1 - Jan 5 = -4
      expect_equal(result$AVAL, 25)
    },
    args = list(
      x = block,
      data = list(data = function() test_dm)
    )
  )
})

test_that("dm_temporal_join block direction 'around'", {
  block <- new_dm_temporal_join_block(
    left_table = "adae",
    left_date = "ASTDT",
    right_table = "adlb",
    right_date = "ADT",
    window_days = 5,
    direction = "around"
  )

  adlb <- data.frame(
    USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-1"),
    ADT = as.Date(c("2024-01-01", "2024-01-08", "2024-01-20")),  # -4, +3, +15 days
    AVAL = c(25, 45, 30)
  )

  adae <- data.frame(
    USUBJID = c("SUBJ-1"),
    AETERM = c("DIARRHOEA"),
    ASTDT = as.Date(c("2024-01-05"))
  )

  test_dm <- dm::dm(adae = adae, adlb = adlb)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Should have 2 rows (Jan 1 and Jan 8 are within 5 days, Jan 20 is not)
      expect_equal(nrow(result), 2)
      expect_true(all(abs(result$days_diff) <= 5))
    },
    args = list(
      x = block,
      data = list(data = function() test_dm)
    )
  )
})

test_that("dm_temporal_join block state includes all parameters", {
  block <- new_dm_temporal_join_block(
    left_table = "adae",
    left_date = "ASTDT",
    right_table = "adlb",
    right_date = "ADT",
    window_days = 7,
    direction = "after"
  )

  adlb <- data.frame(
    USUBJID = c("SUBJ-1"),
    ADT = as.Date(c("2024-01-08")),
    AVAL = c(45)
  )

  adae <- data.frame(
    USUBJID = c("SUBJ-1"),
    AETERM = c("DIARRHOEA"),
    ASTDT = as.Date(c("2024-01-05"))
  )

  test_dm <- dm::dm(adae = adae, adlb = adlb)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state
      expect_true("left_table" %in% names(state))
      expect_true("left_date" %in% names(state))
      expect_true("right_table" %in% names(state))
      expect_true("right_date" %in% names(state))
      expect_true("window_days" %in% names(state))
      expect_true("direction" %in% names(state))

      expect_equal(state$left_table(), "adae")
      expect_equal(state$right_table(), "adlb")
      expect_equal(state$window_days(), 7)
      expect_equal(state$direction(), "after")
    },
    args = list(
      x = block,
      data = list(data = function() test_dm)
    )
  )
})
