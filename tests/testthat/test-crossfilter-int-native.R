# Int-native dictionary encoding: browser-level guarantees.
#
# Stage-1 of the payload-shrink work (see encode_crossfilter_payload) ships
# char/factor columns as integer codes + a levels vector, and the client
# keeps the CODES inside crossfilter (rows, dimensions, group keys,
# predicates), decoding to label strings only at its edges. This suite pins
# the edges from a live browser:
#
#   1. rows really hold ints and levels really arrived (no silent regression
#      to strings, which would hide a broken encode behind working display)
#   2. the DOM shows LABELS, never codes, and sentinels render as (NA)
#   3. a real bar click round-trips to R as the label STRING -- including
#      the __NA__ sentinel -- never a code (codes reaching r_filters would
#      corrupt saved board state)
#   4. restore-shaped constructor filters (incl. __NA__) paint and filter
#   5. the `_ready` re-ship of the server's cached payload is idempotent:
#      same bars, same filters, labels intact
#
# Local-only: skipped on CI (needs a headless browser), same contract as
# test-crossfilter-mirrors-dm-filter.R.

test_that("int-native payload: codes inside, labels at every edge", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("jsonlite")

  build_dm <- function() local({
    parent <- data.frame(
      id  = 1:8,
      GRP = c("A", "B", NA, "", "A", "B", "A", NA),
      stringsAsFactors = FALSE
    )
    child <- data.frame(
      cid = 1:12,
      id  = c(1, 1, 2, 3, 4, 5, 6, 7, 8, 2, 3, 4),
      SEV = c("MILD", "SEVERE", "MILD", "MILD", "SEVERE", "MILD",
              "SEVERE", "MILD", "MILD", "SEVERE", "MILD", "MILD"),
      stringsAsFactors = FALSE
    )
    d <- dm::dm(parent = parent, child = child)
    d <- dm::dm_add_pk(d, parent, id)
    d <- dm::dm_add_fk(d, child, id, parent)
    d
  })

  ui <- shiny::fluidPage(
    blockr.dm:::crossfilter_ui("xf"),
    shiny::verbatimTextOutput("diag_filters"),
    shiny::tags$script(shiny::HTML("
      const tick = setInterval(() => {
        const el = document.querySelector('.js-crossfilter-container');
        if (el && el._block) { window.__cfDebug = el._block; clearInterval(tick); }
      }, 100);
    "))
  )

  server <- function(input, output, session) {
    data_r <- shiny::reactive(build_dm())
    # Restore-shaped: constructor filters preset, exactly what a saved board
    # loads -- including the NA sentinel as a STRING.
    res <- blockr.dm:::crossfilter_server(
      active_dims = list(parent = "GRP", child = "SEV"),
      filters = list(parent = list(GRP = list("A", "__NA__"))),
      range_filters = list(), measure = NULL, agg_func = NULL
    )("xf", data_r)

    output$diag_filters <- shiny::renderText({
      jsonlite::toJSON(res$state$filters())
    })
  }

  app <- suppressWarnings(shinytest2::AppDriver$new(
    shiny::shinyApp(ui, server),
    name = "crossfilter-int-native", timeout = 30000, wait = FALSE
  ))
  on.exit(app$stop(), add = TRUE)

  deadline <- Sys.time() + 30
  repeat {
    ready <- isTRUE(tryCatch(
      app$get_js("window.__cfDebug != null && window.__cfDebug._ready === true"),
      error = function(e) FALSE
    ))
    if (ready || Sys.time() > deadline) break
    Sys.sleep(0.25)
  }
  expect_true(ready)

  # -- 1. rows hold ints; levels arrived ------------------------------------
  probes <- app$get_js("({
    grpIsNumber: typeof window.__cfDebug.instances.parent.all()[0].GRP === 'number',
    sevIsNumber: typeof window.__cfDebug.instances.child.all()[0].SEV === 'number',
    grpLevels: window.__cfDebug.levels.GRP,
    sevLevels: window.__cfDebug.levels.SEV
  })")
  expect_true(probes$grpIsNumber)
  expect_true(probes$sevIsNumber)
  expect_setequal(unlist(probes$grpLevels), c("A", "B", "__NA__", "__EMPTY__"))
  expect_setequal(unlist(probes$sevLevels), c("MILD", "SEVERE"))

  # -- 2. DOM shows labels, sentinels prettified ----------------------------
  dom <- app$get_js("(() => {
    const rows = Array.from(document.querySelectorAll(
      '.dm-cf-filter-card[data-dim=\"GRP\"] .dm-cf-tw-row'));
    return {
      values: rows.map(r => r.dataset.value),
      texts: rows.map(r => r.querySelector('td').textContent.trim())
    };
  })()")
  expect_setequal(unlist(dom$values), c("A", "B", "__NA__", "__EMPTY__"))
  expect_true(all(c("(NA)", "(empty)") %in% unlist(dom$texts)))
  # No bar row ever shows a bare code
  expect_false(any(grepl("^[0-9]+$", unlist(dom$texts))))

  # -- 3/4. restore-shaped filters applied, and R state is label strings ----
  js_filters <- app$get_js("window.__cfDebug.filters.GRP")
  expect_setequal(unlist(js_filters), c("A", "__NA__"))
  # 3 'A' rows + 2 NA rows selected out of 8
  expect_equal(
    app$get_js("window.__cfDebug.instances.parent.allFiltered().length"), 5
  )
  r_filters <- jsonlite::fromJSON(app$get_value(output = "diag_filters"))
  expect_identical(sort(unlist(r_filters$parent$GRP)), sort(c("A", "__NA__")))
  expect_type(unlist(r_filters$parent$GRP), "character")

  # -- 3b. a real DOM click round-trips the label string to R ---------------
  app$run_js("document.querySelector(
    '.dm-cf-filter-card[data-dim=\"SEV\"] .dm-cf-tw-row[data-value=\"SEVERE\"]'
  ).click()")
  Sys.sleep(0.5)
  app$wait_for_idle(timeout = 10000)
  r_filters <- jsonlite::fromJSON(app$get_value(output = "diag_filters"))
  expect_identical(as.character(unlist(r_filters$child$SEV)), "SEVERE")
  expect_type(unlist(r_filters$child$SEV), "character")

  # -- 5. `_ready` re-ship of the cached payload is idempotent --------------
  before <- app$get_js("(() => {
    const b = window.__cfDebug;
    b.__setDataCalls = 0;
    const orig = b.setData.bind(b);
    b.setData = (msg) => { b.__setDataCalls++; return orig(msg); };
    return { filters: b.filters, rows: b.instances.parent.allFiltered().length };
  })()")
  app$run_js("
    const el = document.querySelector('.js-crossfilter-container');
    Shiny.setInputValue(el.id + '_ready', 999, {priority: 'event'});
  ")
  Sys.sleep(1)
  app$wait_for_idle(timeout = 10000)
  after <- app$get_js("({
    calls: window.__cfDebug.__setDataCalls,
    filters: window.__cfDebug.filters,
    rows: window.__cfDebug.instances.parent.allFiltered().length,
    grpIsNumber: typeof window.__cfDebug.instances.parent.all()[0].GRP === 'number',
    naRow: !!document.querySelector(
      '.dm-cf-filter-card[data-dim=\"GRP\"] .dm-cf-tw-row[data-value=\"__NA__\"]')
  })")
  expect_equal(after$calls, 1)
  expect_equal(after$rows, before$rows)
  expect_identical(after$filters, before$filters)
  expect_true(after$grpIsNumber)
  expect_true(after$naRow)

  # R state untouched by the re-ship
  r_filters <- jsonlite::fromJSON(app$get_value(output = "diag_filters"))
  expect_setequal(unlist(r_filters$parent$GRP), c("A", "__NA__"))
})
