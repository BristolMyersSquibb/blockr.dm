test_that("dm_filter_block constructor", {
  block <- new_dm_filter_block()
  expect_s3_class(block, c("dm_filter_block", "transform_block", "block"))

  block2 <- new_dm_filter_block(
    table = "adlb",
    state = list(
      conditions = list(list(
        type = "numeric", column = "AVAL", op = ">", value = 5
      )),
      operator = "&"
    )
  )
  expect_s3_class(block2, "dm_filter_block")
})

test_that("dm_filter_block filters a table with a values condition", {
  block <- new_dm_filter_block(
    table = "events",
    state = list(
      conditions = list(list(
        type = "values", column = "type",
        values = list("A"), mode = "include"
      )),
      operator = "&"
    )
  )

  subjects <- data.frame(id = 1:3, name = c("a", "b", "c"))
  events <- data.frame(
    subject_id = c(1, 1, 2, 3),
    type = c("A", "B", "A", "B")
  )
  test_dm <- dm::dm(subjects = subjects, events = events) |>
    dm::dm_add_pk(subjects, id) |>
    dm::dm_add_fk(events, subject_id, subjects)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      events_filtered <- dm::pull_tbl(result, events)
      expect_true(all(events_filtered$type == "A"))
      expect_equal(nrow(events_filtered), 2)
    },
    args = list(
      x = block,
      data = list(data = function() test_dm)
    )
  )
})

test_that("dm_filter_block cascade reduces related tables via FKs", {
  block <- new_dm_filter_block(
    table = "events",
    state = list(
      conditions = list(list(
        type = "numeric", column = "value", op = ">", value = 10
      )),
      operator = "&"
    )
  )

  subjects <- data.frame(id = 1:5, name = c("a", "b", "c", "d", "e"))
  events <- data.frame(
    subject_id = c(1, 1, 2, 3, 4, 5),
    value = c(5, 15, 20, 8, 12, 3)
  )
  test_dm <- dm::dm(subjects = subjects, events = events) |>
    dm::dm_add_pk(subjects, id) |>
    dm::dm_add_fk(events, subject_id, subjects)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      events_filtered <- dm::pull_tbl(result, events)
      expect_true(all(events_filtered$value > 10))

      subjects_filtered <- dm::pull_tbl(result, subjects)
      expect_true(all(subjects_filtered$id %in% c(1, 2, 4)))
    },
    args = list(
      x = block,
      data = list(data = function() test_dm)
    )
  )
})

test_that("dm_filter_block returns dm unchanged when no conditions set", {
  block <- new_dm_filter_block(table = "events")

  subjects <- data.frame(id = 1:3)
  events <- data.frame(subject_id = c(1, 1, 2), value = c(5, 15, 20))
  test_dm <- dm::dm(subjects = subjects, events = events)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "dm")
      expect_equal(nrow(dm::pull_tbl(result, events)), 3)
    },
    args = list(
      x = block,
      data = list(data = function() test_dm)
    )
  )
})

test_that("dm_filter_block clears conditions when the table switches", {
  block <- new_dm_filter_block(
    table = "events",
    state = list(
      conditions = list(list(
        type = "values", column = "type",
        values = list("A"), mode = "include"
      )),
      operator = "&"
    )
  )

  subjects <- data.frame(id = 1:3)
  events <- data.frame(subject_id = c(1, 2, 3), type = c("A", "B", "A"))
  test_dm <- dm::dm(subjects = subjects, events = events)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      state <- session$returned$state
      expect_equal(length(state$state()$conditions), 1L)

      # The block wraps its server in `moduleServer(id = "expr", ...)` inside
      # block_expr_server, so the input is namespaced as "expr-table_input".
      session$setInputs(`expr-table_input` = "subjects")
      session$flushReact()
      expect_equal(state$table(), "subjects")
      expect_equal(state$state()$conditions, list())
    },
    args = list(
      x = block,
      data = list(data = function() test_dm)
    )
  )
})

test_that("dm_filter_block state exposes table and state reactives", {
  state_in <- list(
    conditions = list(list(
      type = "numeric", column = "AVAL", op = ">", value = 5
    )),
    operator = "&"
  )
  block <- new_dm_filter_block(table = "adlb", state = state_in)

  subjects <- data.frame(USUBJID = paste0("SUBJ-", 1:3))
  labs <- data.frame(
    USUBJID = rep(paste0("SUBJ-", 1:3), each = 2),
    AVAL = c(4.5, 8.2, 6.1, 7.5, 3.2, 6.8)
  )
  test_dm <- dm::dm(adsl = subjects, adlb = labs)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      st <- session$returned$state
      expect_true(all(c("table", "state") %in% names(st)))
      expect_equal(st$table(), "adlb")
      expect_equal(st$state()$operator, "&")
      expect_equal(st$state()$conditions[[1L]]$column, "AVAL")
    },
    args = list(
      x = block,
      data = list(data = function() test_dm)
    )
  )
})
