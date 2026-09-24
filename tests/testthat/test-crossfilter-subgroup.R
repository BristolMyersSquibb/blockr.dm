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
