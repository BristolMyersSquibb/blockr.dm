test_that("new_crossfilter_block constructs a transform block", {
  blk <- new_crossfilter_block()
  expect_s3_class(blk, "crossfilter_block")
  expect_s3_class(blk, "transform_block")
})

test_that("new_crossfilter_block stores initial state", {
  blk <- new_crossfilter_block(
    active_dims = list(adsl = c("SEX", "AGE")),
    filters = list(adsl = list(SEX = list("F"))),
    range_filters = list(adsl = list(AGE = c(40, 60))),
    measure = "adsl.AGE",
    agg_func = "mean"
  )
  state <- blockr.core:::initial_block_state(blk)
  expect_equal(state$active_dims, list(adsl = c("SEX", "AGE")))
  expect_equal(state$filters, list(adsl = list(SEX = list("F"))))
  expect_equal(state$range_filters, list(adsl = list(AGE = c(40, 60))))
  expect_equal(state$measure, "adsl.AGE")
  expect_equal(state$agg_func, "mean")
})

test_that("new_js_crossfilter_block is a deprecated alias for new_crossfilter_block", {
  expect_warning(
    blk <- new_js_crossfilter_block(),
    "deprecated"
  )
  # The deprecated alias still produces a crossfilter_block, just tagged with a
  # legacy subclass so old boards keep dispatching.
  expect_s3_class(blk, "crossfilter_block")
  expect_s3_class(blk, "js_crossfilter_block")
})

test_that("external write to r_filters updates expr and pushes to JS", {
  # External control contract (per blockr.docs/patterns/js-driven-blocks.md):
  # when something outside the JS UI mutates a state reactiveVal
  # (AI assistant, board restore, programmatic), the R-side observer
  # has to ship the new state to JS so the bars / sliders repaint.
  # Before the R->JS push observer existed, expr() recomputed and
  # downstream filtered correctly, but the UI sat stale.
  blk <- new_crossfilter_block(
    active_dims = list(.tbl = c("Species"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() iris)),
    {
      session$flushReact()
      # baseline: no filter, identity expr
      expr_initial <- session$returned$expr()
      expect_match(deparse(expr_initial), "identity", fixed = TRUE)

      # mimic the ai_ctrl_server path: write directly to the
      # externally-controlled reactiveVal.
      vars <- session$returned$state
      vars$filters(list(.tbl = list(Species = "setosa")))
      session$flushReact()

      # expression now filters — downstream gets the right rows
      expr_after <- session$returned$expr()
      txt <- paste(deparse(expr_after), collapse = " ")
      expect_match(txt, "dplyr::filter")
      expect_match(txt, "setosa")
      result <- eval(expr_after, list(data = iris, . = identity))
      expect_true(all(as.character(result$Species) == "setosa"))
    }
  )
})

test_that("JS-shaped filter values become atomic literals, not list()", {
  # Shiny decodes client messages with `simplifyVector = FALSE`, so a
  # one-level selection lands in r_filters as `list("setosa")`. Dropping that
  # list into the call produced `Species == list("setosa")` -- base R coerces
  # it, so a plain data.frame filtered fine and the bug hid behind a working
  # result, but the code shown to the user is wrong and no database backend
  # can translate it.
  blk <- new_crossfilter_block(active_dims = list(.tbl = c("Species")))

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() iris)),
    {
      session$flushReact()
      vars <- session$returned$state

      vars$filters(list(.tbl = list(Species = list("setosa"))))
      session$flushReact()
      txt <- paste(deparse(session$returned$expr()), collapse = " ")
      expect_match(txt, 'Species == "setosa"', fixed = TRUE)
      expect_no_match(txt, "list(", fixed = TRUE)

      vars$filters(list(.tbl = list(Species = list("setosa", "virginica"))))
      session$flushReact()
      txt <- paste(deparse(session$returned$expr()), collapse = " ")
      expect_match(txt, 'Species %in% c("setosa", "virginica")', fixed = TRUE)

      result <- eval(session$returned$expr(), list(data = iris))
      expect_setequal(as.character(result$Species), c("setosa", "virginica"))
    }
  )
})

test_that("the NA sentinel filters missings, not a literal __NA__", {
  # `apply_crossfilter_filters()` reads CROSSFILTER_NA as is.na() when it
  # counts the bars; the expression has to agree or the block reports a row
  # count it does not deliver.
  df <- data.frame(
    GRP = c("A", "B", NA, NA, "A"),
    stringsAsFactors = FALSE
  )
  blk <- new_crossfilter_block(active_dims = list(.tbl = c("GRP")))

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() df)),
    {
      session$flushReact()
      vars <- session$returned$state

      vars$filters(list(.tbl = list(GRP = list("__NA__"))))
      session$flushReact()
      expect_equal(nrow(eval(session$returned$expr(), list(data = df))), 2L)

      vars$filters(list(.tbl = list(GRP = list("A", "__NA__"))))
      session$flushReact()
      expect_equal(nrow(eval(session$returned$expr(), list(data = df))), 4L)
    }
  )
})

test_that("a client announcing itself gets the cached payload re-shipped", {
  # Deferred dock panels ship crossfilter-block.js WITH the panel, so the boot
  # push reaches a client with no registered handler and Shiny drops it
  # silently -- the block then sat blank for the whole session, even after the
  # user switched to its view. The binding announces itself on bind and R
  # answers with its cached payload (filters refreshed from the live
  # reactives, no lookup rebuild).
  blk <- new_crossfilter_block(
    active_dims = list(.tbl = "Species"),
    filters = list(.tbl = list(Species = "setosa"))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() iris)),
    {
      # Proxy sessions forbid assignment; the root MockShinySession allows it.
      sent <- list()
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        sent[[length(sent) + 1L]] <<- list(type = type, message = message)
        invisible()
      }

      session$flushReact()
      n_boot <- length(sent)
      expect_gte(n_boot, 1L)

      # The block's own inputs live one module level down (`expr_server()`).
      session$setInputs(`expr-crossfilter_input_ready` = 1)
      session$flushReact()

      expect_gt(length(sent), n_boot)
      re_ship <- sent[[length(sent)]]
      expect_identical(re_ship$type, "js-crossfilter-data")
      expect_true(length(re_ship$message$lookups) > 0)
      # The re-ship carries the CURRENT filter state, not the boot-time one.
      expect_identical(
        re_ship$message$cat_filters,
        list(.tbl = list(Species = list("setosa")))
      )
      expect_identical(re_ship$message$active_dims, list(.tbl = list("Species")))
    }
  )
})

test_that("lookup builders include measure when table name starts with dot", {
  # data.frame inputs are wrapped as `dm(.tbl = df)` — the table name is
  # `.tbl` (leading dot). The measure spec is then `.tbl.<column>`, and a
  # regex like `^[^.]+\\.` failed to strip the prefix, leaving the measure
  # column out of the lookup and producing all-zero counts on the JS side.
  df <- data.frame(
    age_band    = c("18-29", "30-44", "45-59", "18-29", "30-44"),
    sex         = c("M", "F", "M", "F", "M"),
    Sum_at_Risk = c(100, 200, 300, 400, 500),
    stringsAsFactors = FALSE
  )

  ind <- build_lookups_independent(
    tables      = list(.tbl = df),
    active_dims = list(.tbl = c("age_band", "sex")),
    measure_col = ".tbl.Sum_at_Risk"
  )
  expect_true("Sum_at_Risk" %in% names(ind$lookups[[".tbl"]]))

  # build_lookups_flat needs at least one FK — exercise it on a two-table dm
  # where the parent name also starts with a dot.
  parent <- data.frame(
    pid = 1:3,
    .extra = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  child <- data.frame(
    pid         = c(1, 1, 2, 3),
    age_band    = c("18-29", "30-44", "45-59", "18-29"),
    Sum_at_Risk = c(100, 200, 300, 400),
    stringsAsFactors = FALSE
  )
  d <- dm::dm(.parent = parent, .child = child) |>
    dm::dm_add_pk(.parent, pid) |>
    dm::dm_add_fk(.child, pid, .parent)

  flat <- build_lookups_flat(
    dm_obj      = d,
    active_dims = list(.child = "age_band"),
    measure_col = ".child.Sum_at_Risk"
  )
  expect_true("Sum_at_Risk" %in% names(flat$lookups[[".child"]]))
})
