# Regression: the numeric / date range card, after two changes.
#
# 1. The min and max labels are editable: clicking one swaps in an input, and
#    the number typed there is the number the block filters on. The range
#    inputs snap to `step` ((max-min)/200), so the value has to survive a
#    round trip that does NOT go back through the DOM -- a typed 60 came back
#    as 59.93 while that was the case.
#
# 2. The blue density is the gray curve cut at the handles, not a fresh KDE of
#    the filtered rows. The old overlay re-smoothed a truncated sample, so it
#    sagged inside the selection and bled a bandwidth past both handles.
#
# Drives the live JS with shinytest2 (chromote). Local-only, same opt-in as
# the date-NA regression next door.

test_that("range card: typed bounds are exact, blue density is the gray cut", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  skip_if_not_installed("shinytest2")
  testthat::skip_if_not(
    identical(Sys.getenv("BLOCKR_BROWSER_TESTS"), "true"),
    "set BLOCKR_BROWSER_TESTS=true to run browser regression tests"
  )

  build_dm <- function() local({
    parent <- data.frame(id = 1:100, VAL = as.numeric(1:100))
    child <- data.frame(cid = 1:100, id = 1:100, w = as.numeric(1:100))
    d <- dm::dm(parent = parent, child = child)
    d <- dm::dm_add_pk(d, parent, id)
    dm::dm_add_fk(d, child, id, parent)
  })

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
    data_r <- shiny::reactive(build_dm())
    blockr.dm:::crossfilter_server(
      active_dims = list(parent = "VAL"), filters = list(),
      range_filters = list(), measure = NULL, agg_func = NULL
    )("xf", data_r)
  }

  # wait = FALSE: the block keeps the session busy, so idle never comes.
  app <- suppressWarnings(shinytest2::AppDriver$new(
    shiny::shinyApp(ui, server),
    name = "crossfilter-range-edit", timeout = 30000, wait = FALSE
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

  # -- 1. type a minimum -----------------------------------------------------
  # step here is (100-1)/200 = 0.495, so 60 is NOT on the step grid: if the
  # commit read the value back off the input it would land on 59.65.
  app$run_js("
    var card = window.__cfDebug.panels.VAL;
    card.querySelectorAll('.dm-cf-range-edit')[0].click();
    var inp = card.querySelector('.dm-cf-range-input');
    inp.value = '60';
    inp.dispatchEvent(new KeyboardEvent('keydown', {key: 'Enter', bubbles: true}));
  ")
  Sys.sleep(1)

  expect_equal(app$get_js("window.__cfDebug.panels.VAL._lo"), 60)
  expect_equal(app$get_js("window.__cfDebug.filters.VAL.min"), 60)
  expect_equal(
    app$get_js(
      "window.__cfDebug.panels.VAL.querySelectorAll('.dm-cf-range-edit')[0]
         .textContent"
    ),
    "60"
  )
  # The input is gone again: the card looks exactly as it did before the click.
  expect_null(app$get_js(
    "window.__cfDebug.panels.VAL.querySelector('.dm-cf-range-input')"
  ))

  # -- 2. the blue path is the gray one, cut at the handle -------------------
  probe <- app$get_js("
    (function () {
      var card = window.__cfDebug.panels.VAL;
      function pts(p) {
        return (p.getAttribute('d') || '').replace(/Z/g, '').split(/[ML]/)
          .slice(1)
          .map(function (s) { return s.trim().split(',').map(Number); })
          .filter(function (a) { return a.length === 2; });
      }
      // First and last point of each path are the baseline the fill closes
      // on (y = 80 at both ends); drop them or the curve at x = 300 gets
      // compared against the baseline point that shares its x.
      var gray = pts(card._pathAll).slice(1, -1);
      var bi = pts(card._pathFiltered).slice(1, -1);
      var blue = bi;
      var xs = blue.map(function (p) { return p[0]; });
      var handleX = (card._lo - card._min) / (card._max - card._min) * 300;
      // Gray heights at the blue grid points that sit inside the selection,
      // against blue's own: identical means blue IS the gray slice.
      var grayAt = {};
      gray.forEach(function (p) { grayAt[p[0].toFixed(1)] = p[1]; });
      var diffs = blue.filter(function (p) { return p[1] < 80; })
        .map(function (p) {
          var g = grayAt[p[0].toFixed(1)];
          return g === undefined ? 0 : Math.abs(g - p[1]);
        });
      // The path's own first and last points: the fill closes on the
      // baseline straight down from the curve, so both pairs share an x.
      var all = pts(card._pathFiltered);
      var openDx = Math.abs(all[0][0] - all[1][0]);
      var closeDx = Math.abs(all[all.length - 1][0] - all[all.length - 2][0]);
      // Biggest jump between neighbouring points inside the curve: a cut
      // that slopes instead of dropping shows up here.
      var innerStep = 0;
      for (var i = 1; i < bi.length; i++) {
        innerStep = Math.max(innerStep, Math.abs(bi[i][1] - bi[i - 1][1]));
      }
      return {
        handleX: handleX,
        blueMinX: Math.min.apply(null, xs),
        blueMaxX: Math.max.apply(null, xs),
        nShared: diffs.length,
        openDx: openDx,
        closeDx: closeDx,
        innerStep: innerStep,
        maxDiff: diffs.length ? Math.max.apply(null, diffs) : -1
      };
    })()
  ")

  # Cut sits under the handle, not a bandwidth either side of it.
  expect_equal(probe$blueMinX, probe$handleX, tolerance = 0.2)
  expect_equal(probe$blueMaxX, 300, tolerance = 0.2)
  # And inside the selection the two curves are the same curve.
  expect_gt(probe$nShared, 10)
  expect_lt(probe$maxDiff, 0.01)

  # The cut is a vertical drop from the curve to the baseline, not a slope:
  # both closing segments keep their x, and nothing inside the curve jumps
  # the way the cut edge used to when it interpolated against a zeroed cell.
  expect_equal(probe$openDx, 0)
  expect_equal(probe$closeDx, 0)
  expect_lt(probe$innerStep, 10)

  # -- 3. reset puts the card back on its bounds ----------------------------
  app$run_js(
    "window.__cfDebug.panels.VAL.querySelector('.dm-cf-reset-btn').click()"
  )
  Sys.sleep(1)
  expect_equal(app$get_js("window.__cfDebug.panels.VAL._lo"), 1)
  expect_equal(app$get_js("window.__cfDebug.panels.VAL._hi"), 100)
  expect_equal(
    app$get_js(
      "window.__cfDebug.panels.VAL.querySelectorAll('.dm-cf-range-edit')[0]
         .textContent"
    ),
    "1"
  )
  expect_null(app$get_js("window.__cfDebug.filters.VAL"))
})
