# Cross-layer parity test: the JS crossfilter UI must produce the same
# per-table row counts as `dm::dm_filter()` for every reachable filter
# state. Drives the live UI via shinytest2 (chromote), reads JS state with
# `app$get_js()`, reads the post-`dm_filter` row counts with `app$get_value()`,
# and asserts equality across a matrix of filter combinations.
#
# Local-only by default: skipped on CI to avoid requiring a headless browser
# in CI runners. Run with `devtools::test()` or plain `R CMD check` locally.

test_that("crossfilter JS state mirrors dm::dm_filter row counts", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("safetyData")
  skip_if_not_installed("jsonlite")

  # local() suppresses shinytest2's global-name analyzer noise from
  # dm_add_{pk,fk}'s NSE — the bare symbols `adsl`, `USUBJID`, etc. look
  # like undefined globals to static inspection but are NSE-quoted at call.
  build_dm <- function() local({
    d <- dm::dm(
      adsl  = safetyData::adam_adsl,
      adae  = safetyData::adam_adae,
      adlbc = safetyData::adam_adlbc
    )
    d <- dm::dm_add_pk(d, adsl, USUBJID)
    d <- dm::dm_add_fk(d, adae, USUBJID, adsl)
    d <- dm::dm_add_fk(d, adlbc, USUBJID, adsl)
    d
  })

  active_dims <- list(
    adsl  = c("ARM", "SEX"),
    adae  = c("AESEV", "AEBODSYS"),
    adlbc = c("PARAMCD", "AVAL")
  )

  ui <- shiny::fluidPage(
    blockr.dm:::crossfilter_ui("xf"),
    # Machine-readable diag outputs: JSON-encoded row counts and ARM table
    # so the test can parse them via jsonlite::fromJSON.
    shiny::verbatimTextOutput("diag_counts"),
    shiny::verbatimTextOutput("diag_arm"),
    # Expose `el._block` as window.__cfDebug for `app$get_js()` access.
    shiny::tags$script(shiny::HTML("
      const tick = setInterval(() => {
        const el = document.querySelector('.js-crossfilter-container');
        if (el && el._block) {
          window.__cfDebug = el._block;
          clearInterval(tick);
        }
      }, 100);
    "))
  )

  server <- function(input, output, session) {
    data_r <- shiny::reactive(build_dm())
    res <- blockr.dm:::crossfilter_server(
      active_dims = active_dims, filters = list(),
      range_filters = list(), measure = NULL, agg_func = NULL
    )("xf", data_r)

    filtered_dm <- shiny::reactive({
      eval(res$expr(), envir = list(data = data_r()))
    })

    output$diag_counts <- shiny::renderText({
      tt <- dm::dm_get_tables(filtered_dm())
      n <- vapply(tt, function(t) nrow(as.data.frame(t)), integer(1))
      jsonlite::toJSON(as.list(n), auto_unbox = TRUE)
    })

    output$diag_arm <- shiny::renderText({
      adsl <- as.data.frame(dm::dm_get_tables(filtered_dm())$adsl)
      tab <- as.list(table(adsl$ARM, useNA = "ifany"))
      jsonlite::toJSON(tab, auto_unbox = TRUE)
    })
  }

  # Silence the harmless globalsByName() warning from shinytest2's static
  # code analyzer — `dm_add_pk()` / `dm_add_fk()` use NSE so the bare symbols
  # `adsl`, `USUBJID`, etc. appear as undefined globals to static analysis.
  app <- suppressWarnings(shinytest2::AppDriver$new(
    shiny::shinyApp(ui, server),
    name = "crossfilter-dm-filter-mirror",
    timeout = 30000
  ))
  on.exit(app$stop(), add = TRUE)

  # Give JS a moment to wire up `__cfDebug` and the crossfilter to ingest the
  # initial lookup payload.
  Sys.sleep(2)
  app$wait_for_idle(timeout = 10000)

  # Helpers ---------------------------------------------------------------
  read_state <- function(label) {
    js <- app$get_js("({
      adsl: window.__cfDebug.instances.adsl.allFiltered().length,
      adae: window.__cfDebug.instances.adae.allFiltered().length,
      adlbc: window.__cfDebug.instances.adlbc.allFiltered().length,
      arm_invariant: (() => {
        // Sum of ARM-card counts for currently SELECTED values (or all
        // values if ARM has no own filter). Categorical group counts in
        // crossfilter exclude their own dim's filter, so the unfiltered sum
        // when ARM is filtered would equal the all-but-ARM-filtered total —
        // not adsl. Restricting to selected values gives an invariant that
        // matches R's dm_filter(adsl=ARM==...) row count in every state.
        const groups = window.__cfDebug.groups.ARM.all();
        const sel = window.__cfDebug.filters.ARM;
        const selSet = sel ? new Set(sel) : null;
        return groups
          .filter(d => !selSet || selSet.has(String(d.key)))
          .reduce((a, d) => a + window.__cfDebug._getGroupValue(d), 0);
      })()
    })")
    counts <- jsonlite::fromJSON(app$get_value(output = "diag_counts"))
    list(label = label, js = js, r = counts)
  }

  apply_js <- function(script) {
    app$run_js(script)
    # Submit debounce is 100ms; give R a moment to flush the new state.
    Sys.sleep(0.4)
    app$wait_for_idle(timeout = 10000)
  }

  expect_mirror <- function(s) {
    info <- s$label
    testthat::expect_equal(s$js$adsl,  s$r$adsl,  info = paste(info, "adsl"))
    testthat::expect_equal(s$js$adae,  s$r$adae,  info = paste(info, "adae"))
    testthat::expect_equal(s$js$adlbc, s$r$adlbc, info = paste(info, "adlbc"))
    # ARM card invariant: sum of selected ARM values' counts equals
    # nrow(adsl) post-dm_filter. Holds whether ARM is filtered or not.
    testthat::expect_equal(s$js$arm_invariant, s$r$adsl,
                           info = paste(info, "ARM card mirrors adsl"))
  }

  # Scenario matrix --------------------------------------------------------
  expect_mirror(read_state("no filters"))

  apply_js("__cfDebug._toggleCategorical('AESEV', 'MILD')")
  expect_mirror(read_state("AESEV=MILD"))

  apply_js("__cfDebug._applyFilter('AVAL', {min: 16, max: 30})")
  expect_mirror(read_state("AESEV=MILD + AVAL in [16, 30]"))

  apply_js("__cfDebug._resetAllFilters();
           __cfDebug._applyFilter('AVAL', {min: 16, max: 30})")
  expect_mirror(read_state("AVAL in [16, 30] only"))

  apply_js("__cfDebug._resetAllFilters();
           __cfDebug._toggleCategorical('ARM', 'Placebo')")
  expect_mirror(read_state("ARM=Placebo"))

  apply_js("__cfDebug._resetAllFilters()")
  expect_mirror(read_state("after reset"))
})
