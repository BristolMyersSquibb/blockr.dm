# The categorical card as the multi-select's list in place: a tick per row,
# the ticked values on top under "Selected", and above eight values the
# filter box with the count, All and None. A column of numbers keeps its
# value order. The card has no reset of its own; a range card keeps one, and
# Reset all is the count with an x.
#
# Drives the live JS with shinytest2 (chromote). Local-only, same opt-in as
# the other browser regressions here.

test_that("categorical card: ticks, groups, the list head, clearing", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  skip_if_not_installed("shinytest2")
  testthat::skip_if_not(
    identical(Sys.getenv("BLOCKR_BROWSER_TESTS"), "true"),
    "set BLOCKR_BROWSER_TESTS=true to run browser regression tests"
  )

  build_dm <- function() {
    dm::dm(pt = data.frame(
      id = 1:40,
      # Ten sites, largest first, so count order is S01 ... S10.
      SITE = rep(sprintf("S%02d", 1:10), times = c(10, 8, 6, 5, 4, 3, 1, 1, 1, 1)),
      SEX = rep(c("F", "M"), 20),
      # Three numeric values: a categorical card, 54 the most frequent.
      DOSE = rep(c(0, 54, 81, 54), 10),
      AGE = as.numeric(30:69)
    ))
  }

  ui <- shiny::fluidPage(
    blockr.dm:::crossfilter_ui("xf"),
    shiny::tags$script(shiny::HTML("
      const tick = setInterval(() => {
        const el = document.querySelector('.js-crossfilter-container');
        if (el && el._block) { window.__cfDebug = el._block; clearInterval(tick); }
      }, 100);
      window.__card = (d) => document.querySelector(
        '.dm-cf-filter-card[data-dim=\"' + d + '\"]');
      window.__rows = (d) => Array.from(
        window.__card(d).querySelectorAll('.dm-cf-tw-row'));
      window.__values = (d) => window.__rows(d).map(r => r.dataset.value);
      window.__groups = (d) => Array.from(
        window.__card(d).querySelectorAll('.blockr-pick__group'))
        .map(g => g.textContent);
      window.__click = (d, v) => window.__rows(d)
        .find(r => r.dataset.value === v).click();
      window.__bulk = (d, t) => Array.from(
        window.__card(d).querySelectorAll('.blockr-pick__bulk-btn'))
        .find(b => b.textContent === t);
      window.__type = (d, q) => {
        const inp = window.__card(d).querySelector('.dm-cf-tw-filter');
        inp.value = q;
        inp.dispatchEvent(new Event('input'));
      };
    "))
  )
  server <- function(input, output, session) {
    data_r <- shiny::reactive(build_dm())
    blockr.dm:::crossfilter_server(
      active_dims = list(pt = c("SITE", "SEX", "DOSE", "AGE")),
      filters = list(), range_filters = list(), measure = NULL, agg_func = NULL
    )("xf", data_r)
  }

  # wait = FALSE: the block keeps the session busy, so idle never comes.
  app <- suppressWarnings(shinytest2::AppDriver$new(
    shiny::shinyApp(ui, server),
    name = "crossfilter-card-rows", timeout = 30000, wait = FALSE
  ))
  on.exit(app$stop(), add = TRUE)

  deadline <- Sys.time() + 30
  repeat {
    ready <- isTRUE(tryCatch(
      app$get_js("window.__cfDebug != null && window.__card('SITE') != null &&
        window.__rows('SITE').length === 10"),
      error = function(e) FALSE
    ))
    if (ready || Sys.time() > deadline) break
    Sys.sleep(0.25)
  }
  expect_true(ready)

  count <- function(dim) {
    app$get_js(sprintf(
      "window.__card('%s').querySelector('.blockr-pick__count').textContent", dim
    ))
  }
  site_filter <- function() unlist(app$get_js("window.__cfDebug.filters.SITE || []"))

  # -- The list head shows above eight values only -------------------------
  head_shown <- function(dim) {
    app$get_js(sprintf(
      "window.__card('%s').querySelector('.dm-cf-tw-head').style.display !== 'none'",
      dim
    ))
  }
  expect_true(head_shown("SITE"))
  expect_false(head_shown("SEX"))
  expect_equal(count("SITE"), "10 values")

  # -- No reset on a categorical card; the range card keeps its own --------
  has_reset <- function(dim) {
    app$get_js(sprintf(
      "window.__card('%s').querySelector('.dm-cf-reset-btn') != null", dim
    ))
  }
  expect_false(has_reset("SITE"))
  expect_false(has_reset("SEX"))
  expect_true(has_reset("AGE"))

  # -- By count at rest; a tick moves the row under "Selected" -------------
  expect_equal(unlist(app$get_js("window.__values('SITE')"))[1:3], c("S01", "S02", "S03"))
  expect_length(app$get_js("window.__groups('SITE')"), 0)
  app$run_js("window.__click('SITE', 'S03')")
  expect_equal(unlist(app$get_js("window.__values('SITE')"))[1:3], c("S03", "S01", "S02"))
  expect_equal(unlist(app$get_js("window.__groups('SITE')")), c("Selected", "Other"))
  expect_equal(count("SITE"), "1 of 10 selected")
  expect_equal(site_filter(), "S03")
  expect_equal(
    app$get_js("window.__rows('SITE').filter(r => r.classList.contains('dimmed')).length"),
    9
  )
  expect_equal(
    app$get_js("window.__rows('SITE')[0].getAttribute('aria-selected')"), "true"
  )
  expect_true(
    app$get_js("window.__rows('SITE')[0].querySelector('.dm-cf-tw-tick svg') != null")
  )

  # -- Reset all is the count, then an x ------------------------------------
  expect_equal(
    app$get_js("document.querySelector('.jscf-reset-label').textContent"),
    "6 of 40 rows"
  )
  expect_true(app$get_js("document.querySelector('.jscf-reset-btn .jscf-reset-x svg') != null"))

  # -- All and None act on what the filter box matches ----------------------
  app$run_js("window.__type('SITE', '1')")
  expect_setequal(unlist(app$get_js("window.__values('SITE')")), c("S01", "S10"))
  app$run_js("window.__bulk('SITE', 'All').click()")
  expect_setequal(site_filter(), c("S03", "S01", "S10"))
  expect_true(app$get_js("window.__bulk('SITE', 'All').disabled"))
  app$run_js("window.__bulk('SITE', 'None').click()")
  expect_equal(site_filter(), "S03")

  # With the box empty, None clears the card.
  app$run_js("window.__type('SITE', '')")
  app$run_js("window.__bulk('SITE', 'None').click()")
  expect_length(site_filter(), 0)
  expect_equal(count("SITE"), "10 values")
  expect_true(app$get_js("window.__bulk('SITE', 'None').disabled"))

  # -- A column of numbers keeps its value order, ticked or not -------------
  expect_equal(unlist(app$get_js("window.__values('DOSE')")), c("0", "54", "81"))
  app$run_js("window.__click('DOSE', '81')")
  expect_equal(unlist(app$get_js("window.__values('DOSE')")), c("0", "54", "81"))
  expect_length(app$get_js("window.__groups('DOSE')"), 0)
  expect_equal(unlist(app$get_js("window.__cfDebug.filters.DOSE")), "81")

  # -- Reset all clears every card -------------------------------------------
  app$run_js("document.querySelector('.jscf-reset-btn').click()")
  expect_equal(app$get_js("Object.keys(window.__cfDebug.filters).length"), 0)
  expect_equal(
    app$get_js("document.querySelectorAll('.dm-cf-tw-row[aria-selected=\"true\"]').length"),
    0
  )
})
