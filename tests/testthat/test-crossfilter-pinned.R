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
  # without any of its levels being filtered out. Cards are opened from the
  # pill row, the group's included.
  blk <- new_crossfilter_block(featured = "Species", pinned = "Species")

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() iris)),
    {
      session$flushReact()
      expect_equal(session$returned$state$pinned(), "Species")
      expect_equal(session$returned$state$active_dims(), list())
    }
  )
})

test_that("a pin that is not pinnable is dropped, not drawn", {
  # Sepal.Length is numeric (a range dim), so it can never hold the pinned
  # card. A board naming it should open without a pinned card rather than with
  # one that cannot render.
  blk <- new_crossfilter_block(
    featured = c("Species", "Sepal.Length"),
    pinned = "Sepal.Length"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() iris)),
    {
      session$flushReact()
      expect_equal(session$returned$state$active_dims(), list())
    }
  )
})

test_that("moving the pin keeps the previous column and its filter", {
  # Switching the split must not widen the population under the reader: the
  # old column stays active, filter and all, as an ordinary card.
  df <- data.frame(
    Species = iris$Species,
    Site = rep(c("A", "B"), length.out = nrow(iris)),
    stringsAsFactors = FALSE
  )
  blk <- new_crossfilter_block(
    featured = c("Species", "Site"),
    pinned = "Species",
    active_dims = list(.tbl = "Species"),
    filters = list(.tbl = list(Species = "setosa"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() df)),
    {
      session$flushReact()
      # The block's own module is nested one level down ("expr"), so its
      # inputs carry that namespace prefix from here.
      session$setInputs(`expr-set_pinned` = "Site")
      session$flushReact()

      expect_equal(session$returned$state$pinned(), "Site")
      # The column that lost the pin keeps its card and its filter. Site gains
      # the pin without gaining a card.
      expect_equal(session$returned$state$active_dims()$.tbl, "Species")
      expect_equal(session$returned$state$filters(),
                   list(.tbl = list(Species = "setosa")))

      result <- eval(session$returned$expr(), list(data = df))
      expect_true(all(as.character(result$Species) == "setosa"))
    }
  )
})

test_that("clearing all dimensions leaves the group standing", {
  # Clear all takes the cards, including the group's own card if it has one.
  # What it must not take is the split: the board keeps grouping.
  blk <- new_crossfilter_block(
    featured = "Species", pinned = "Species",
    active_dims = list(.tbl = "Species")
  )

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() iris)),
    {
      session$flushReact()
      session$setInputs(`expr-clear_filters` = 1)
      session$flushReact()
      expect_equal(session$returned$state$pinned(), "Species")
    }
  )
})

test_that("the featured vocabulary is editable from the client", {
  # A board built without `featured =` has to be able to grow one, or the
  # feature is only reachable by editing board code.
  blk <- new_crossfilter_block()

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() iris)),
    {
      session$flushReact()
      expect_equal(session$returned$state$featured(), character())

      session$setInputs(`expr-set_featured` = list("Species", "Sepal.Length"))
      session$flushReact()
      expect_equal(session$returned$state$featured(),
                   c("Species", "Sepal.Length"))

      # ... and then pinned, which is the only route to a group on such a board
      session$setInputs(`expr-set_pinned` = "Species")
      session$flushReact()
      expect_equal(session$returned$state$pinned(), "Species")
      expect_equal(session$returned$state$active_dims(), list())

      # Un-featuring the pinned column drops the pin with it: the pin is a mark
      # on a member of the vocabulary, not a value of its own.
      session$setInputs(`expr-set_featured` = list("Sepal.Length"))
      session$flushReact()
      expect_equal(session$returned$state$featured(), "Sepal.Length")
    }
  )
})

test_that("an empty pick unpins without clearing the cards", {
  blk <- new_crossfilter_block(
    featured = "Species", pinned = "Species",
    active_dims = list(.tbl = "Species")
  )

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() iris)),
    {
      session$flushReact()
      session$setInputs(`expr-set_pinned` = "")
      session$flushReact()
      expect_equal(session$returned$state$pinned(), character())
      # The card stays: unpinning stops the split, it does not narrow or widen
      # the population.
      expect_equal(session$returned$state$active_dims(), list(.tbl = "Species"))
    }
  )
})
