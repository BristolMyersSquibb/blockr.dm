# Regression: a date dimension whose column contains missing values must
# filter correctly. Missing dates are shipped to the browser as JSON `null`;
# with the old raw-value crossfilter accessor a null landed at one end of the
# sort, so bottom(1)/top(1) returned the null record and `new Date(null)`
# collapsed the slider bound to 1970-01-01 — an inverted range and an unusable
# date filter (the ALLM-DB "known limitation"). The date dim now uses a numeric
# epoch-day accessor so crossfilter drops NA (NaN) from its sorted index.
#
# Drives the live JS with shinytest2 (chromote) and checks (a) the computed
# range bounds are the real min/max, not 1970, and (b) a date-range filter's
# JS row count mirrors R's `dm_filter` (NA rows excluded).
#
# Local-only: skipped on CI (needs a headless browser).

test_that("date dimension with NAs filters correctly (bounds + parity)", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("jsonlite")
  # Heavy, browser-driven regression test. shinytest2/chromote stability is
  # environment-sensitive (the headless session can crash mid-run on some
  # platforms), so gate it behind an explicit opt-in to keep the default
  # `R CMD check` green while still allowing deliberate local verification:
  #   BLOCKR_BROWSER_TESTS=true R CMD check / devtools::test()
  testthat::skip_if_not(
    identical(Sys.getenv("BLOCKR_BROWSER_TESTS"), "true"),
    "set BLOCKR_BROWSER_TESTS=true to run browser regression tests"
  )

  build_dm <- function() local({
    parent <- data.frame(
      id  = 1:6,
      ADT = as.Date(c(
        "2020-01-01", "2020-06-15", NA, "2021-03-20", NA, "2022-01-01"
      ))
    )
    child <- data.frame(cid = 1:6, id = 1:6, val = c(10, 20, 30, 40, 50, 60))
    d <- dm::dm(parent = parent, child = child)
    d <- dm::dm_add_pk(d, parent, id)
    d <- dm::dm_add_fk(d, child, id, parent)
    d
  })

  ui <- shiny::fluidPage(
    blockr.dm:::crossfilter_ui("xf"),
    shiny::verbatimTextOutput("diag_counts"),
    shiny::tags$script(shiny::HTML("
      const tick = setInterval(() => {
        const el = document.querySelector('.js-crossfilter-container');
        if (el && el._block) { window.__cfDebug = el._block; clearInterval(tick); }
      }, 100);
    "))
  )

  server <- function(input, output, session) {
    data_r <- shiny::reactive(build_dm())
    res <- blockr.dm:::crossfilter_server(
      active_dims = list(parent = "ADT"), filters = list(),
      range_filters = list(), measure = NULL, agg_func = NULL
    )("xf", data_r)

    filtered_dm <- shiny::reactive(eval(res$expr(), envir = list(data = data_r())))

    output$diag_counts <- shiny::renderText({
      tt <- dm::dm_get_tables(filtered_dm())
      n <- vapply(tt, function(t) nrow(as.data.frame(t)), integer(1))
      jsonlite::toJSON(as.list(n), auto_unbox = TRUE)
    })
  }

  # The crossfilter block keeps the Shiny session perpetually "busy" under a
  # headless browser (it streams filter state to the JS side), so AppDriver's
  # default wait-for-idle never returns. Build with wait = FALSE and instead
  # poll for the JS hook the UI script publishes once the block is initialized
  # — the probes below only need the live JS block, not Shiny idle.
  app <- suppressWarnings(shinytest2::AppDriver$new(
    shiny::shinyApp(ui, server),
    name = "crossfilter-date-na", timeout = 30000, wait = FALSE
  ))
  on.exit(app$stop(), add = TRUE)

  deadline <- Sys.time() + 20
  repeat {
    ready <- isTRUE(tryCatch(
      app$get_js("window.__cfDebug != null && window.__cfDebug.instances != null"),
      error = function(e) FALSE
    ))
    if (ready || Sys.time() > deadline) break
    Sys.sleep(0.25)
  }

  # (a) Range bounds are the real data extremes, NOT 1970 (epoch day 0). The
  # old bug surfaced as max == 0 (an inverted {min: 18261, max: 0} range).
  bounds <- app$get_js("window.__cfDebug._getRangeBounds('ADT', 'date')")
  expect_false(is.null(bounds))
  expect_equal(bounds$min, as.numeric(as.Date("2020-01-01")))
  expect_equal(bounds$max, as.numeric(as.Date("2022-01-01")))

  # (b) A date-range filter [2020-01-01 .. 2020-12-31] keeps the two in-range,
  # non-NA rows and excludes the NA dates — JS row count mirrors R's dm_filter.
  lo <- as.numeric(as.Date("2020-01-01"))
  hi <- as.numeric(as.Date("2020-12-31"))
  app$run_js(sprintf(
    "__cfDebug._applyFilter('ADT', {min: %f, max: %f, isDate: true})", lo, hi
  ))
  Sys.sleep(1)

  js_parent <- app$get_js("window.__cfDebug.instances.parent.allFiltered().length")
  r_counts <- jsonlite::fromJSON(app$get_value(output = "diag_counts"))
  expect_equal(js_parent, 2)
  expect_equal(r_counts$parent, 2)
})
