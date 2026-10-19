test_that("crossfilter_parent_table picks the table children point at", {
  # One table: it is its own parent, so every featured column is pinnable.
  expect_equal(
    crossfilter_parent_table(list(table_names = ".tbl", fks = NULL)),
    ".tbl"
  )
  # No keys to reason from: no parent, and the pinnable rule falls back to
  # "every featured category".
  expect_null(
    crossfilter_parent_table(
      list(
        table_names = c("a", "b"),
        fks = data.frame(child_table = character(), parent_table = character())
      )
    )
  )
  # Two children pointing at adsl, one at ae: adsl is the parent.
  expect_equal(
    crossfilter_parent_table(
      list(
        table_names = c("adsl", "ae", "lb", "aesup"),
        fks = data.frame(
          child_table = c("ae", "lb", "aesup"),
          parent_table = c("adsl", "adsl", "ae")
        )
      )
    ),
    "adsl"
  )
})

test_that("featured columns resolve to a table, parent first", {
  col_info <- list(
    adsl = list(
      dimensions = list("SEX", "TRT"),
      range_dimensions = list("AGE"),
      date_dimensions = list(),
      labels = list(SEX = "Sex", TRT = "Treatment", AGE = "Age")
    ),
    ae = list(
      dimensions = list("AESEV", "SEX"),
      range_dimensions = list(),
      date_dimensions = list(),
      labels = list(AESEV = "Severity")
    )
  )

  feat <- crossfilter_featured_dims(
    c("TRT", "SEX", "AGE", "AESEV", "NOSUCHCOL"), col_info, "adsl"
  )

  expect_length(feat, 4L)
  expect_equal(vapply(feat, `[[`, character(1), "dim"),
               c("TRT", "SEX", "AGE", "AESEV"))
  # SEX exists on both tables; the parent wins, so the chip filters subjects
  # rather than events.
  expect_equal(feat[[2]]$table, "adsl")
  expect_equal(feat[[3]]$type, "range")
  expect_equal(feat[[4]]$table, "ae")
  expect_equal(feat[[1]]$label, "Treatment")
})

test_that("only parent-table categories are pinnable", {
  feat <- list(
    list(table = "adsl", dim = "TRT", type = "categorical", label = ""),
    list(table = "adsl", dim = "AGE", type = "range", label = ""),
    list(table = "ae", dim = "AESEV", type = "categorical", label = "")
  )

  # A range column cannot be a pinned card, and an event-level column cannot
  # split a subject-level table.
  expect_equal(crossfilter_pinnable(feat, "adsl"), "TRT")
  # No parent resolved: every featured category qualifies.
  expect_equal(crossfilter_pinnable(feat, NULL), c("TRT", "AESEV"))
  expect_equal(crossfilter_pinnable(list(), "adsl"), character())
})

test_that("featured and pinned round-trip through block state", {
  blk <- new_crossfilter_block(
    featured = c("Species", "Sepal.Length"),
    pinned = "Species"
  )
  state <- blockr.core:::initial_block_state(blk)
  expect_equal(state$featured, c("Species", "Sepal.Length"))
  expect_equal(state$pinned, "Species")

  # Empty is the default and has to survive serialization as such: the state
  # is what a saved board writes down.
  state <- blockr.core:::initial_block_state(new_crossfilter_block())
  expect_equal(state$featured, character())
  expect_null(state$pinned)
})

test_that("grouping by a column does not open its filter card", {
  # The two are separate facts: a column can split every exhibit on the board
  # without any of its levels being filtered out.
  blk <- new_crossfilter_block(featured = "Species", pinned = "Species")
  state <- blockr.core:::initial_block_state(blk)
  expect_equal(state$pinned, "Species")
  expect_length(state$active_dims, 0L)
  expect_equal(cf_payload(blk, iris)$pinned, "Species")
})

test_that("a pin that is not pinnable is dropped, not drawn", {
  # Sepal.Length is numeric (a range dim), so it can never hold the pinned
  # card. A board naming it opens without a pinned card.
  blk <- new_crossfilter_block(
    featured = c("Species", "Sepal.Length"),
    pinned = "Sepal.Length"
  )
  msg <- cf_payload(blk, iris)
  expect_null(msg$pinned)
  expect_equal(unlist(msg$pinnable), "Species")
})

test_that("the pinned column's filter is an ordinary filter", {
  # Switching the split (crossfilter.js, set_pinned) leaves the cards and
  # filters as they are; the filter on the old group still filters.
  df <- data.frame(
    Species = iris$Species,
    Site = rep(c("A", "B"), length.out = nrow(iris)),
    stringsAsFactors = FALSE
  )
  blk <- new_crossfilter_block(
    featured = c("Species", "Site"),
    pinned = "Site",
    active_dims = list(.tbl = "Species"),
    filters = list(.tbl = list(Species = "setosa"))
  )
  expr <- crossfilter_expr(
    blockr.core:::initial_block_state(blk), list(data = df)
  )
  result <- eval(expr, list(data = df))
  expect_true(all(as.character(result$Species) == "setosa"))
})







