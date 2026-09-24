test_that("crossfilter_level_order follows factor levels, else sorts", {
  f <- factor(c("b", "a", "b"), levels = c("c", "b", "a"))
  # Only the levels that occur, in levels() order.
  expect_equal(crossfilter_level_order(f), c("b", "a"))
  expect_equal(crossfilter_level_order(c("b", NA, "a", "b")), c("a", "b"))
  expect_equal(crossfilter_level_order(c(3, 1, 2, 1)), c("1", "2", "3"))
  expect_equal(crossfilter_level_order(character()), character())
})

test_that("a pool's default name follows its members", {
  lv <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")
  expect_equal(crossfilter_pool_default_name(character(), lv), "New pool")
  expect_equal(crossfilter_pool_default_name(rev(lv), lv), "All patients")
  expect_equal(
    crossfilter_pool_default_name(
      c("Xanomeline Low Dose", "Xanomeline High Dose"), lv
    ),
    "All Xanomeline"
  )
  expect_equal(
    crossfilter_pool_default_name(c("Xanomeline Low Dose", "Placebo"), lv),
    "Placebo + Xanomeline Low Dose"
  )
  expect_equal(crossfilter_pool_default_name("Placebo", lv), "Placebo")
})

test_that("groups round-trip through block state", {
  groups <- list(TRT = list(
    show = c("A", "B"),
    pools = list(list(name = "All", members = c("A", "B"), custom = TRUE))
  ))
  state <- blockr.core:::initial_block_state(
    new_crossfilter_block(groups = groups)
  )
  expect_equal(state$groups, groups)
  # Empty is the default and has to survive as such.
  expect_equal(
    blockr.core:::initial_block_state(new_crossfilter_block())$groups,
    list()
  )
})

cf_groups_df <- function() {
  data.frame(
    ID = 1:6,
    TRT = factor(
      c("Placebo", "High", "Low", "High", "Placebo", "Placebo"),
      levels = c("Placebo", "Low", "High", "Unused")
    ),
    SEX = c("F", "M", "F", "M", "F", "M"),
    stringsAsFactors = FALSE
  )
}

test_that("set_groups is normalized and a default definition is dropped", {
  df <- cf_groups_df()
  blk <- new_crossfilter_block(featured = c("TRT", "SEX"), pinned = "TRT")

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() df)),
    {
      session$flushReact()
      expect_equal(session$returned$state$groups(), list())

      # What Shiny delivers from the client: JS arrays arrive as lists.
      session$setInputs(`expr-set_groups` = list(
        column = "TRT",
        show = list("High", "Placebo"),
        pools = list(
          list(name = "Active", members = list("Low", "High"), custom = TRUE),
          list(name = "", members = list("Low"), custom = TRUE),
          list(name = "New pool", members = list(), custom = FALSE)
        )
      ))
      session$flushReact()

      expect_equal(
        session$returned$state$groups(),
        list(TRT = list(
          show = c("High", "Placebo"),
          pools = list(
            list(name = "Active", members = c("Low", "High"), custom = TRUE),
            # No name: the default, and the name follows the members again.
            list(name = "Low", members = "Low", custom = FALSE),
            # No members: kept, it is a pool being built.
            list(name = "New pool", members = character(), custom = FALSE)
          )
        ))
      )

      # Every level shown, in level order, no pools: the entry goes.
      session$setInputs(`expr-set_groups` = list(
        column = "TRT",
        show = list("Placebo", "Low", "High"),
        pools = list()
      ))
      session$flushReact()
      expect_equal(session$returned$state$groups(), list())
    }
  )
})

test_that("the data payload carries pinned_levels and groups", {
  df <- cf_groups_df()
  groups <- list(TRT = list(show = "Placebo", pools = list()))
  blk <- new_crossfilter_block(
    featured = c("TRT", "SEX"), pinned = "TRT", groups = groups,
    active_dims = list(.tbl = "SEX"),
    filters = list(.tbl = list(SEX = "F"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() df)),
    {
      sent <- list()
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        sent[[length(sent) + 1L]] <<- list(type = type, message = message)
        invisible()
      }

      session$flushReact()
      data_msgs <- Filter(function(m) m$type == "js-crossfilter-data", sent)
      expect_gte(length(data_msgs), 1L)
      msg <- data_msgs[[length(data_msgs)]]$message

      # Level order of the factor, unused level left out, rows of the
      # unfiltered input (the SEX filter does not change them).
      expect_equal(
        msg$pinned_levels,
        list(
          list(value = "Placebo", n = 3L),
          list(value = "Low", n = 1L),
          list(value = "High", n = 2L)
        )
      )
      # A one-level show stays an array.
      expect_equal(
        msg$groups,
        list(TRT = list(show = list("Placebo"), pools = list()))
      )

      # A groups edit does not re-ship the data: the client already has it.
      n_before <- length(sent)
      session$setInputs(`expr-set_groups` = list(
        column = "TRT", show = list("High"), pools = list()
      ))
      session$flushReact()
      expect_equal(
        session$returned$state$groups()$TRT$show, "High"
      )
      expect_equal(length(sent), n_before)

      # A recreated client gets the current groups with the cached payload.
      session$setInputs(`expr-crossfilter_input_ready` = 1)
      session$flushReact()
      re_ship <- sent[[length(sent)]]
      expect_identical(re_ship$type, "js-crossfilter-data")
      expect_equal(re_ship$message$groups$TRT$show, list("High"))
    }
  )
})

test_that("with no active dims the payload still carries pinned_levels", {
  df <- cf_groups_df()
  blk <- new_crossfilter_block(featured = "TRT", pinned = "TRT")

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() df)),
    {
      sent <- list()
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        sent[[length(sent) + 1L]] <<- list(type = type, message = message)
        invisible()
      }
      session$flushReact()
      data_msgs <- Filter(function(m) m$type == "js-crossfilter-data", sent)
      msg <- data_msgs[[length(data_msgs)]]$message
      expect_length(msg$lookups, 0L)
      expect_equal(
        vapply(msg$pinned_levels, `[[`, character(1), "value"),
        c("Placebo", "Low", "High")
      )
      expect_equal(msg$groups, list())
    }
  )
})

test_that("groups survive board serialization", {
  groups <- list(TRT = list(
    show = "Placebo",
    pools = list(list(name = "Active", members = c("Low", "High"),
                      custom = FALSE))
  ))
  blk <- new_crossfilter_block(featured = "TRT", pinned = "TRT",
                               groups = groups)
  ser <- blockr_ser(blk)
  json <- jsonlite::toJSON(ser, null = "null", auto_unbox = TRUE)
  parsed <- jsonlite::fromJSON(
    as.character(json), simplifyDataFrame = FALSE, simplifyMatrix = FALSE
  )
  restored <- blockr_deser(parsed)

  blk2 <- restored
  testServer(
    blockr.core:::get_s3_method("block_server", blk2),
    args = list(x = blk2, data = list(data = function() cf_groups_df())),
    {
      session$flushReact()
      expect_equal(session$returned$state$groups(), groups)
    }
  )
})
