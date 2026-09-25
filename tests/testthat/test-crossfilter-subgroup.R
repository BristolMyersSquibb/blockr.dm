sub_df <- function() {
  data.frame(
    ID = 1:4,
    TRT = c("Placebo", "Active", "Active", "Placebo"),
    SEX = c("F", "M", "F", "M"),
    AGE = c(60, 70, 80, 90),
    stringsAsFactors = FALSE
  )
}

last_data_msg <- function(sent) {
  msgs <- Filter(function(m) m$type == "js-crossfilter-data", sent)
  msgs[[length(msgs)]]$message
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
  blk <- new_crossfilter_block(
    featured = c("TRT", "SEX"), pinned = "TRT", subgroup = "SEX"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() sub_df())),
    {
      sent <- list()
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        sent[[length(sent) + 1L]] <<- list(type = type, message = message)
        invisible()
      }

      session$flushReact()
      expect_equal(last_data_msg(sent)$subgroup, "SEX")

      # The group moving onto the subgroup's column takes the subgroup away.
      session$setInputs(`expr-set_pinned` = "SEX")
      session$flushReact()
      expect_null(last_data_msg(sent)$subgroup)

      session$setInputs(`expr-set_pinned` = "TRT")
      session$flushReact()
      expect_equal(last_data_msg(sent)$subgroup, "SEX")

      # An empty string clears it, and the state says so.
      session$setInputs(`expr-set_subgroup` = "")
      session$flushReact()
      expect_equal(session$returned$state$subgroup(), character())
      expect_null(last_data_msg(sent)$subgroup)
    }
  )
})

test_that("a subgroup that is not an eligible column is not reported", {
  blk <- new_crossfilter_block(
    featured = c("TRT", "SEX"), pinned = "TRT", subgroup = "AGE"
  )
  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() sub_df())),
    {
      sent <- list()
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        sent[[length(sent) + 1L]] <<- list(type = type, message = message)
        invisible()
      }
      session$flushReact()
      expect_null(last_data_msg(sent)$subgroup)
    }
  )
})

test_that("the payload carries the subgroup's levels, and swap trades the two", {
  blk <- new_crossfilter_block(
    featured = c("TRT", "SEX"), pinned = "TRT", subgroup = "SEX",
    groups = list(SEX = list(show = list("F"), pools = list()))
  )
  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() sub_df())),
    {
      sent <- list()
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        sent[[length(sent) + 1L]] <<- list(type = type, message = message)
        invisible()
      }
      session$flushReact()
      msg <- last_data_msg(sent)
      expect_equal(
        vapply(msg$subgroup_levels, `[[`, character(1), "value"), c("F", "M")
      )
      expect_equal(names(msg$groups), "SEX")

      session$setInputs(`expr-swap_split` = 1)
      session$flushReact()
      msg <- last_data_msg(sent)
      expect_equal(msg$pinned, "SEX")
      expect_equal(msg$subgroup, "TRT")
      expect_equal(
        vapply(msg$subgroup_levels, `[[`, character(1), "value"),
        c("Active", "Placebo")
      )
      expect_equal(session$returned$state$pinned(), "SEX")
      expect_equal(session$returned$state$subgroup(), "TRT")
      # The definition stays with its column.
      expect_equal(session$returned$state$groups()$SEX$show, "F")

      # A subgroup's pools are set like the group's, keyed by its column.
      session$setInputs(`expr-set_groups` = list(
        column = "TRT", show = list("Placebo"),
        pools = list(list(name = "", members = list("Active", "Placebo"),
                          custom = FALSE))
      ))
      session$flushReact()
      expect_equal(session$returned$state$groups()$TRT$pools[[1]]$name,
                   "All patients")
    }
  )
})

test_that("swap does nothing without a subgroup", {
  blk <- new_crossfilter_block(featured = c("TRT", "SEX"), pinned = "TRT")
  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() sub_df())),
    {
      session$flushReact()
      session$setInputs(`expr-swap_split` = 1)
      session$flushReact()
      expect_equal(session$returned$state$pinned(), "TRT")
      expect_equal(session$returned$state$subgroup(), character())
    }
  )
})
