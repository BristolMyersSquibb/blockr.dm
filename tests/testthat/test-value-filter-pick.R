# The value filter's multi values are a Blockr.Pick dropdown: picks are
# reported once, when it closes. These check the round trip in a browser: a
# burst of ticks is one change, None sticks (an empty multi is no condition,
# so every row), and nothing R sends back moves the picks afterwards.
#
# Drives the live JS with shinytest2 (chromote). Local-only, same opt-in as
# the other browser regressions here.

test_that("value filter: picks, a burst and None survive the round trip", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  skip_if_not_installed("shinytest2")
  testthat::skip_if_not(
    identical(Sys.getenv("BLOCKR_BROWSER_TESTS"), "true"),
    "set BLOCKR_BROWSER_TESTS=true to run browser regression tests"
  )

  blk <- new_value_filter_block(state = list(columns = list(
    list(name = "Species", mode = "multi", values = c("setosa", "versicolor"))
  )))

  ui <- shiny::fluidPage(
    blockr.dm:::value_filter_ui(drill = FALSE)(shiny::NS("vf", "expr")),
    shiny::textOutput("n")
  )
  server <- function(input, output, session) {
    # `swap` stands for an upstream change: one column fewer, so the block
    # redraws its fields.
    dat <- shiny::reactive(if (isTRUE(input$swap > 0)) iris[, -1] else iris)
    res <- blockr.core:::get_s3_method("block_server", blk)(
      "vf", x = blk, data = list(data = dat)
    )
    output$n <- shiny::renderText({
      out <- eval(res$expr(), list(data = dat(), . = identity))
      paste(nrow(out), paste(sort(unique(as.character(out$Species))), collapse = ","))
    })
  }

  app <- suppressWarnings(shinytest2::AppDriver$new(
    shiny::shinyApp(ui, server),
    name = "value-filter-pick", timeout = 30000, wait = FALSE
  ))
  on.exit(app$stop(), add = TRUE)

  deadline <- Sys.time() + 30
  repeat {
    ready <- isTRUE(tryCatch(
      app$get_js("document.querySelector('.bi-filter-container .blockr-pick .blockr-select__control') != null"),
      error = function(e) FALSE
    ))
    if (ready || Sys.time() > deadline) break
    Sys.sleep(0.25)
  }
  expect_true(ready)

  result <- function() app$get_js("document.getElementById('n').textContent")
  face <- function() app$get_js("document.querySelector('.bi-filter-container .blockr-pick__face').textContent")
  app$run_js("
    window.__ctl = () => document.querySelector('.bi-filter-container .blockr-pick .blockr-select__control');
    window.__list = () => document.querySelector('.blockr-select__dropdown.blockr-pick__panel');
    window.__row = (v) => window.__list().querySelector('.blockr-select__option[data-value=\"' + v + '\"]');
    window.__bulk = (t) => Array.from(window.__list().querySelectorAll('.blockr-pick__bulk-btn'))
      .find(b => b.textContent === t);
  ")
  Sys.sleep(1)
  expect_equal(result(), "100 setosa,versicolor")

  # Open the list and wait for its values: they load on first open.
  open_list <- function() {
    app$run_js("window.__ctl().click()")
    deadline <- Sys.time() + 10
    while (!isTRUE(app$get_js("!!(window.__list() && window.__row('virginica'))")) &&
           Sys.time() < deadline) Sys.sleep(0.2)
  }

  # A burst: three ticks, one close, one change.
  open_list()
  app$run_js("
    window.__row('virginica').click();
    window.__row('setosa').click();
    window.__row('versicolor').click();
    window.__ctl().click();
  ")
  Sys.sleep(2)
  expect_equal(result(), "50 virginica")
  expect_equal(face(), "virginica")

  # None, closed: the field is empty and stays so; no condition, every row.
  open_list()
  app$run_js("window.__bulk('None').click(); window.__ctl().click();")
  Sys.sleep(2)
  expect_equal(result(), "150 setosa,versicolor,virginica")
  expect_equal(face(), "Select values…")

  # All, closed.
  open_list()
  app$run_js("window.__bulk('All').click(); window.__ctl().click();")
  Sys.sleep(2)
  expect_equal(result(), "150 setosa,versicolor,virginica")
  expect_equal(face(), "All 3")

  # Upstream changes while the list is open with a new pick: the block
  # redraws, the list goes, and the pick is reported rather than lost.
  open_list()
  app$run_js("window.__row('setosa').click();")
  app$run_js("Shiny.setInputValue('swap', 1)")
  Sys.sleep(3)
  expect_equal(result(), "100 versicolor,virginica")
})
