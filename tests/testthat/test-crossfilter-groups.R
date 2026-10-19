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
  groups <- list(TRT = list(columns = list(
    list(name = "", members = "A", custom = FALSE),
    list(name = "All", members = c("A", "B"), custom = TRUE),
    list(name = "", members = "B", custom = FALSE)
  )))
  state <- blockr.core:::initial_block_state(
    new_crossfilter_block(groups = groups)
  )
  expect_equal(state$groups, groups)
  # Empty is the default and has to survive as such.
  expect_length(
    blockr.core:::initial_block_state(new_crossfilter_block())$groups,
    0L
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

test_that("a group definition is normalized and a default one is dropped", {
  # The client keeps the definitions it edits (crossfilter.js, groupDefs);
  # these clean what a constructor or a saved board brings.
  levels <- c("Placebo", "Low", "High")
  def <- crossfilter_clean_group_def(list(columns = list(
    list(name = "", members = list("High"), custom = FALSE),
    list(name = "Active", members = list("Low", "High"), custom = TRUE),
    list(name = "", members = list("Low", "Placebo"), custom = TRUE),
    list(name = "New pool", members = list(), custom = FALSE)
  )), levels)
  expect_equal(def, list(columns = list(
    list(name = "", members = "High", custom = FALSE),
    list(name = "Active", members = c("Low", "High"), custom = TRUE),
    # Several members and no name: the default, following the members.
    list(name = "Placebo + Low", members = c("Low", "Placebo"),
         custom = FALSE),
    # No members: kept, it is a pool being built.
    list(name = "New pool", members = character(), custom = FALSE)
  )))

  every_level <- crossfilter_clean_group_def(list(columns = lapply(
    levels, function(l) list(name = "", members = l, custom = FALSE)
  )), levels)
  expect_true(crossfilter_group_def_is_default(every_level, levels))

  # The older show/pools shape: its levels, then its pools.
  expect_equal(
    crossfilter_clean_group_def(list(
      show = list("High", "Placebo"),
      pools = list(list(name = "", members = list("Low"), custom = TRUE))
    ), levels),
    list(columns = list(
      list(name = "", members = "High", custom = FALSE),
      list(name = "", members = "Placebo", custom = FALSE),
      list(name = "Low", members = "Low", custom = FALSE)
    ))
  )
})

test_that("the payload carries pinned_levels, the state the groups", {
  df <- cf_groups_df()
  blk <- new_crossfilter_block(
    featured = c("TRT", "SEX"), pinned = "TRT",
    groups = list(TRT = list(show = "Placebo", pools = list())),
    active_dims = list(.tbl = "SEX"),
    filters = list(.tbl = list(SEX = "F"))
  )
  msg <- cf_payload(blk, df)

  # Level order of the factor, unused level left out, rows of the unfiltered
  # input (the SEX filter does not change them).
  expect_equal(
    msg$pinned_levels,
    list(
      list(value = "Placebo", n = 3L),
      list(value = "Low", n = 1L),
      list(value = "High", n = 2L)
    )
  )
  # The groups are state, not payload: an edit costs no data. The older
  # shape arrives converted.
  expect_null(msg$groups)
  state <- blockr.core:::initial_block_state(blk)
  expect_equal(
    state$groups,
    list(TRT = list(columns = list(
      list(name = "", members = "Placebo", custom = FALSE)
    )))
  )
})

test_that("with no active dims the payload still carries pinned_levels", {
  blk <- new_crossfilter_block(featured = "TRT", pinned = "TRT")
  msg <- cf_payload(blk, cf_groups_df())
  expect_length(msg$lookups, 0L)
  expect_equal(
    vapply(msg$pinned_levels, `[[`, character(1), "value"),
    c("Placebo", "Low", "High")
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
      # Read back in the columns shape: the level, then the pool.
      expect_equal(
        session$returned$state$groups(),
        list(TRT = list(columns = list(
          list(name = "", members = "Placebo", custom = FALSE),
          list(name = "Active", members = c("Low", "High"), custom = FALSE)
        )))
      )
    }
  )
})
