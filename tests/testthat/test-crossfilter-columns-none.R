# Regression: None in the Columns list of a group split. A split keeps at
# least one column, so _setColumns() put the picks back when the list
# reported an empty pick. The list showed "0 of 3", then, a second later
# (the in-place list's report delay), "3 of 3" again. Now the list takes
# `min: 1`: None is held back and the last pick does not untick, so the
# count never reads 0.
#
# Drives the live JS with shinytest2 (chromote). Local-only, same opt-in as
# the other browser regressions here.

test_that("Columns: None is held back and the picks never bounce", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  skip_if_not_installed("shinytest2")
  testthat::skip_if_not(
    identical(Sys.getenv("BLOCKR_BROWSER_TESTS"), "true"),
    "set BLOCKR_BROWSER_TESTS=true to run browser regression tests"
  )

  df <- data.frame(
    id = 1:30,
    ARM = rep(c("Placebo", "Low", "High"), 10)
  )

  ui <- shiny::fluidPage(
    blockr.dm:::crossfilter_ui("xf"),
    shiny::tags$script(shiny::HTML("
      const tick = setInterval(() => {
        const el = document.querySelector('.js-crossfilter-container');
        if (el && el._block) { window.__cfDebug = el._block; clearInterval(tick); }
      }, 100);
    "))
  )
  server <- function(input, output, session) {
    data_r <- shiny::reactive(df)
    blockr.dm:::crossfilter_server(
      active_dims = list(df = "ARM"), filters = list(),
      range_filters = list(), measure = NULL, agg_func = NULL,
      featured = "ARM", pinned = "ARM"
    )("xf", data_r)
  }

  app <- suppressWarnings(shinytest2::AppDriver$new(
    shiny::shinyApp(ui, server),
    name = "crossfilter-columns-none", timeout = 30000, wait = FALSE
  ))
  on.exit(app$stop(), add = TRUE)

  deadline <- Sys.time() + 30
  repeat {
    ready <- isTRUE(tryCatch(
      app$get_js("document.querySelector('.jscf-columns-select .blockr-pick__count') != null"),
      error = function(e) FALSE
    ))
    if (ready || Sys.time() > deadline) break
    Sys.sleep(0.25)
  }
  expect_true(ready)

  count <- function() {
    app$get_js("document.querySelector('.jscf-columns-select .blockr-pick__count').textContent")
  }
  expect_match(count(), "^3 of 3")

  # Every count the list shows from here on.
  app$run_js("
    const host = document.querySelector('.jscf-columns-select');
    window.__counts = [];
    new MutationObserver(() => {
      const c = host.querySelector('.blockr-pick__count');
      if (c) window.__counts.push(c.textContent);
    }).observe(host, { subtree: true, childList: true, characterData: true });
    window.__bulk = (t) => Array.from(host.querySelectorAll('.blockr-pick__bulk-btn'))
      .find(b => b.textContent === t);
    window.__row = (v) => host.querySelector('.blockr-select__option[data-value=\"' + v + '\"]');
  ")

  expect_equal(app$get_js("window.__bulk('None').getAttribute('aria-disabled')"), "true")
  app$run_js("window.__bulk('None').click()")
  Sys.sleep(2)
  expect_match(count(), "^3 of 3")

  # Untick two, then try the last one.
  app$run_js("window.__row('Low').click(); window.__row('High').click();")
  expect_equal(app$get_js("window.__row('Placebo').getAttribute('aria-disabled')"), "true")
  app$run_js("window.__row('Placebo').click()")
  Sys.sleep(2)
  expect_match(count(), "^1 of 3")
  counts <- unlist(app$get_js("window.__counts"))
  expect_false(any(grepl("^0 of", counts)))
  expect_equal(
    app$get_js("window.__cfDebug.groupDefs.ARM.columns.map(c => c.members[0])"),
    list("Placebo")
  )
})
