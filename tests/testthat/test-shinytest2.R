# End-to-end tests for the crossfilter block's JS UI.
# Skipped on CRAN — slow (browser launch + Shiny boot ~5s).
# Run locally with `Sys.setenv(NOT_CRAN = "true")` then `devtools::test()`.

skip_on_cran()
skip_if_not_installed("shinytest2")

library(shinytest2)

# Read the JS-side block state for a given block id (the actual UI
# state, not r_filters on the R side).
get_block_js_state <- function(app, block_id) {
  input_id <- paste0("board-block_", block_id, "-expr-crossfilter_input")
  raw <- app$get_js(sprintf(
    "JSON.stringify((function () { var el = document.getElementById(\"%s\"); if (!el || !el._block) return null; var b = el._block; return {activeDims: b.activeDims, filters: b.filters, panels: Object.keys(b.panels || {}), ready: !!b._ready}; })())",
    input_id
  ))
  if (is.null(raw) || identical(raw, "null")) return(NULL)
  jsonlite::fromJSON(raw, simplifyVector = FALSE)
}

# Downstream block result from exportTestValues.
get_block_result <- function(app, block_id) {
  vals <- app$get_values()
  vals$export$result[[block_id]]
}

# Click one of the test-ctrl buttons that mimic an AI-assistant write.
# These buttons live in the custom ctrl_block_ui scoped to each block.
click_test_ctrl <- function(app, block_id, action) {
  selector <- sprintf("#board-block_%s-ctrl_block-%s", block_id, action)
  app$click(selector = selector)
  app$wait_for_idle()
}

app <- AppDriver$new(
  test_path("apps", "crossfilter-e2e"),
  name = "crossfilter-e2e",
  seed = 42,
  load_timeout = 60 * 1000,
  timeout = 15 * 1000
)
app$wait_for_idle(timeout = 10 * 1000)

withr::defer(app$stop(), testthat::teardown_env())

# ---------------------------------------------------------------------------
# Path 1: constructor state -> initial UI render
# ---------------------------------------------------------------------------

test_that("empty constructor renders no panels and passes data through", {
  st <- get_block_js_state(app, "cf_default")
  expect_true(st$ready)
  expect_equal(length(st$panels), 0)
  expect_equal(length(st$filters), 0)

  res <- get_block_result(app, "cf_default")
  expect_equal(nrow(res), nrow(iris))
})

test_that("active_dims renders panels without applying filters", {
  st <- get_block_js_state(app, "cf_active")
  expect_true(st$ready)
  expect_setequal(unlist(st$panels), c("Species", "Sepal.Length"))
  expect_setequal(unlist(st$activeDims[[".tbl"]]), c("Species", "Sepal.Length"))
  expect_equal(length(st$filters), 0)

  res <- get_block_result(app, "cf_active")
  expect_equal(nrow(res), nrow(iris))
})

test_that("constructor filters paint the UI and filter downstream", {
  st <- get_block_js_state(app, "cf_filtered")
  expect_true(st$ready)
  expect_setequal(unlist(st$panels), c("Species", "Sepal.Length"))
  expect_equal(unlist(st$filters$Species), "setosa")
  expect_equal(st$filters$Sepal.Length$min, 5)
  expect_equal(st$filters$Sepal.Length$max, 6)

  res <- get_block_result(app, "cf_filtered")
  expect_true(all(res$Species == "setosa"))
  expect_true(all(res$Sepal.Length >= 5 & res$Sepal.Length <= 6))
})

# ---------------------------------------------------------------------------
# Path 2: external write -> JS repaint
# The test-ctrl plugin's button mimics the AI assistant calling
# vars$filters(...) at runtime. Without the R->JS push observer, the
# downstream data updated but the UI sat stale; this catches that.
# ---------------------------------------------------------------------------

test_that("external write to vars$filters updates the JS UI", {
  # baseline: cf_active has no filter
  st0 <- get_block_js_state(app, "cf_active")
  expect_equal(length(st0$filters), 0)

  click_test_ctrl(app, "cf_active", "set_setosa")

  st1 <- get_block_js_state(app, "cf_active")
  expect_equal(unlist(st1$filters$Species), "setosa")

  res <- get_block_result(app, "cf_active")
  expect_true(all(res$Species == "setosa"))
})

test_that("external write to vars$range_filters updates the JS UI", {
  click_test_ctrl(app, "cf_active", "set_range")

  st <- get_block_js_state(app, "cf_active")
  expect_equal(st$filters$Sepal.Length$min, 5)
  expect_equal(st$filters$Sepal.Length$max, 6)

  res <- get_block_result(app, "cf_active")
  expect_true(all(res$Sepal.Length >= 5 & res$Sepal.Length <= 6))
})

test_that("external clear releases filters in both UI and downstream", {
  click_test_ctrl(app, "cf_active", "clear_all")

  st <- get_block_js_state(app, "cf_active")
  expect_equal(length(st$filters), 0)

  res <- get_block_result(app, "cf_active")
  expect_equal(nrow(res), nrow(iris))
})

# ---------------------------------------------------------------------------
# Slider snap-back regression: a user-initiated slider drag must NOT
# trigger an R->JS echo (no setData / setExternalFilters), otherwise
# the slider visually resets when R updates r_range_filters. The fix
# is shiny::isolate() around the filter reads in the data observe so
# it doesn't gain a dependency on r_filters / r_range_filters.
# ---------------------------------------------------------------------------

test_that("dragging the range slider does not cause an R->JS echo", {
  # Reset cf_active to a clean state for this test.
  click_test_ctrl(app, "cf_active", "clear_all")

  block_id <- "cf_active"
  input_id <- paste0("board-block_", block_id, "-expr-crossfilter_input")

  # Install JS-side counters on setData / setExternalFilters so we can
  # detect any R->JS echo that follows the simulated drag.
  app$run_js(sprintf(
    "(function () { var el = document.getElementById(\"%s\");
       if (!el || !el._block) return;
       var b = el._block;
       window.__snap_setData = 0;
       window.__snap_setExt  = 0;
       var rd = b.setData.bind(b);
       var re = b.setExternalFilters.bind(b);
       b.setData          = function (m) { window.__snap_setData++; return rd(m); };
       b.setExternalFilters = function (c, r) { window.__snap_setExt++; return re(c, r); };
     })();",
    input_id
  ))

  # Simulate a user drag: dispatch several `input` events on the hi
  # range input, mirroring what the browser does as the thumb moves.
  app$run_js(sprintf(
    "(function () { var el = document.getElementById(\"%s\");
       var card = el.querySelector('.dm-cf-filter-card[data-dim=\"Sepal.Length\"]');
       var hi = card._inputHi;
       [7.0, 6.5, 6.0, 5.5].forEach(function (v) {
         hi.value = String(v);
         hi.dispatchEvent(new Event('input', { bubbles: true }));
       });
     })();",
    input_id
  ))
  app$wait_for_idle(timeout = 5 * 1000)

  # The user's drag should have landed on the JS side with the
  # browser-stepped value the slider snapped to, NOT been overwritten
  # by an R-side echo.
  raw <- app$get_js(sprintf(
    "JSON.stringify((function () { var el = document.getElementById(\"%s\");
       var b = el._block;
       var card = el.querySelector('.dm-cf-filter-card[data-dim=\"Sepal.Length\"]');
       return {
         filters: b.filters,
         hi: card._inputHi.value,
         lo: card._inputLo.value,
         setDataCount: window.__snap_setData,
         setExtCount:  window.__snap_setExt
       };
     })())",
    input_id
  ))
  state <- jsonlite::fromJSON(raw, simplifyVector = FALSE)

  # Slider stays at the value the user dragged to (the last dispatched
  # value rounds to ~5.506 with the iris-derived step).
  expect_true(state$filters$Sepal.Length$max < 5.7)
  expect_true(as.numeric(state$hi) < 5.7)

  # And R did NOT push a setData / setExternalFilters back as a
  # response to the user's own write. This is what kept the slider
  # snapping to its bounds before the isolate() fix.
  expect_equal(state$setDataCount, 0)
  expect_equal(state$setExtCount,  0)
})
