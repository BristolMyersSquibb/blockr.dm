# Several numeric and date cards filtering against each other.
#
# After each step the page is audited (crossfilter-audit.js) against a
# recount of the block's rows: every range card's scale, row count, handles,
# thumbs, labels and both curves, and every categorical count. The cases:
#
# - A filter is what the user set and stays that. Cutting one card used to
#   rewrite the filters on the other range cards to their shrunk scale, for
#   good, and each rewrite moved the first card's rows in turn. A single drag
#   through a narrow range was enough.
# - With one value in reach, or none, a card kept its old scale, curve and
#   "of N rows". It now stands on the column's whole range.
# - A column with missing values: the scale is the data's, not 0 or a bound
#   read off the wrong end of a sort that has no place for NA.
# - Whole numbers over a range the step does not divide (0 to 1003 by 5):
#   the right end of the slider is the end of the scale.
# - A handle at an end of a shrunk scale leaves that side open.
# - Filters pushed from R put the handles on their values.
#
# Drives the live JS with shinytest2 (chromote). Local-only, same opt-in as
# the other browser regressions here.

test_that("range cards: scale, handles and curves follow the other filters", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  skip_if_not_installed("shinytest2")
  testthat::skip_if_not(
    identical(Sys.getenv("BLOCKR_BROWSER_TESTS"), "true"),
    "set BLOCKR_BROWSER_TESTS=true to run browser regression tests"
  )

  set.seed(42)
  n <- 300
  num1 <- round(stats::runif(n, 0, 100), 2)
  pt <- data.frame(
    id = seq_len(n),
    ARM = sample(c("A", "B", "C"), n, TRUE),
    NUM1 = num1,
    # Follows NUM1, so a cut on one moves the other's scale.
    NUM2 = round(20 + 0.4 * num1 + stats::rnorm(n, 0, 3), 2),
    # Whole numbers over a range that 5 does not divide.
    INTBIG = c(0L, 1003L, sample(0:1003, n - 2, TRUE)),
    DATE1 = as.Date("2024-01-01") + sample(0:365, n, TRUE),
    # Positive numbers with gaps: the scale must not start at 0.
    NUMNA = round(stats::runif(n, 100, 200), 1),
    stringsAsFactors = FALSE
  )
  pt$DATE1[sample(n, 60)] <- NA
  pt$NUMNA[sample(n, 50)] <- NA
  # Arm S: five rows that share one NUM1 value.
  pt$ARM[1:5] <- "S"
  pt$NUM1[1:5] <- 50

  audit_js <- paste(readLines(testthat::test_path("crossfilter-audit.js")),
                    collapse = "\n")
  ui <- shiny::fluidPage(
    blockr.dm:::crossfilter_ui("xf"),
    shiny::tags$script(shiny::HTML(audit_js))
  )
  server <- function(input, output, session) {
    data_r <- shiny::reactive(dm::dm(pt = pt))
    blockr.dm:::crossfilter_server(
      active_dims = list(
        pt = c("ARM", "NUM1", "NUM2", "INTBIG", "DATE1", "NUMNA")
      ),
      filters = list(), range_filters = list(), measure = NULL, agg_func = NULL
    )("xf", data_r)
  }

  # wait = FALSE: the block keeps the session busy, so idle never comes.
  app <- suppressWarnings(shinytest2::AppDriver$new(
    shiny::shinyApp(ui, server),
    name = "crossfilter-range-cards", timeout = 30000, wait = FALSE
  ))
  on.exit(app$stop(), add = TRUE)

  deadline <- Sys.time() + 30
  repeat {
    ready <- isTRUE(tryCatch(
      app$get_js("window.__cfDebug != null && window.__cfDebug._ready === true &&
        window.__cfDebug.panels.NUMNA != null &&
        window.__cfDebug.panels.NUMNA._inputLo != null"),
      error = function(e) FALSE
    ))
    if (ready || Sys.time() > deadline) break
    Sys.sleep(0.25)
  }
  expect_true(ready)

  # Run a step, then expect nothing on the page to disagree with a recount.
  step <- function(label, js = NULL) {
    if (!is.null(js)) {
      app$run_js(js)
      Sys.sleep(0.5)
    }
    expect_identical(
      as.character(unlist(app$get_js("window.__audit()"))), character(),
      label = label
    )
  }
  filters <- function() app$get_js("window.__filters()")
  card <- function(dim) app$get_js(sprintf("window.__card('%s')", dim))
  reset_all <- function() {
    app$run_js("if (window.__cfDebug.resetBtn.style.display !== 'none')
      window.__cfDebug.resetBtn.click();")
    Sys.sleep(0.5)
  }
  reset_card <- function(dim) {
    sprintf(
      "window.__cfDebug.panels.%s.querySelector('.dm-cf-reset-btn').click();",
      dim
    )
  }

  # -- At rest: the scales are the data's, missing values or not ------------
  step("at rest")
  expect_equal(card("NUMNA")$min, min(pt$NUMNA, na.rm = TRUE))
  expect_equal(card("DATE1")$min, as.numeric(min(pt$DATE1, na.rm = TRUE)))
  expect_equal(card("DATE1")$max, as.numeric(max(pt$DATE1, na.rm = TRUE)))

  # -- Three range filters, then the other cards change: no drift -----------
  step("NUM1 typed", "__type('NUM1','lo','20'); __type('NUM1','hi','80');")
  step("NUM2 typed", "__type('NUM2','lo','30'); __type('NUM2','hi','50');")
  d1 <- card("DATE1")
  step("DATE1 dragged", sprintf(
    "__drag('DATE1','lo',%s); __drag('DATE1','hi',%s);", d1$min + 30, d1$max - 30
  ))
  set <- filters()
  expect_equal(set$NUM1, list(min = 20, max = 80))
  expect_equal(set$NUM2, list(min = 30, max = 50))

  step("arms ticked and unticked",
       "__tick('ARM','A'); __tick('ARM','B'); __tick('ARM','A');")
  step("INTBIG cut, then reset", paste(
    "__drag('INTBIG','lo',400); __drag('INTBIG','hi',600);", reset_card("INTBIG")
  ))
  step("arm B unticked", "__tick('ARM','B');")
  expect_equal(filters(), set)

  # -- One drag through a narrow range and back ------------------------------
  reset_all()
  step("NUM2 typed", "__type('NUM2','lo','36'); __type('NUM2','hi','48');")
  step("NUM1 dragged down to 3 and back up, one drag",
       "__drag('NUM1','hi',40,{release:false}); __drag('NUM1','hi',3,{release:false});
        __drag('NUM1','hi',60,{release:false}); __drag('NUM1','hi',100);")
  expect_equal(filters(), list(NUM2 = list(min = 36, max = 48)))

  # -- One value in reach, then none -----------------------------------------
  reset_all()
  step("arm S ticked: NUM1 has one value in reach", "__tick('ARM','S');")
  expect_equal(card("NUM1")$info, "5 of 5 rows")
  expect_equal(card("NUM1")$min, min(pt$NUM1))
  expect_equal(card("NUM1")$max, max(pt$NUM1))
  step("arm S unticked", "__tick('ARM','S');")

  step("NUM1 cut to 60..70", "__type('NUM1','lo','60'); __type('NUM1','hi','70');")
  step("arm S ticked: no rows left anywhere", "__tick('ARM','S');")
  expect_equal(card("NUM2")$info, "0 of 0 rows")
  expect_equal(filters()$NUM1, list(min = 60, max = 70))
  step("arm S unticked", "__tick('ARM','S');")
  expect_equal(filters(), list(NUM1 = list(min = 60, max = 70)))

  # -- The right end of a slider whose step does not land on it --------------
  reset_all()
  step("INTBIG upper handle to 500, then to the right end", sprintf(
    "__drag('INTBIG','hi',500); __drag('INTBIG','hi',%s);", card("INTBIG")$sliderMax
  ))
  expect_length(filters(), 0)

  # -- A handle at the end of a shrunk scale leaves that side open -----------
  step("NUM2 typed", "__type('NUM2','lo','35'); __type('NUM2','hi','50');")
  shrunk <- card("NUM1")
  expect_gt(shrunk$min, min(pt$NUM1))
  step("NUM1 lower handle in and back to the left end, upper to 60", sprintf(
    "__drag('NUM1','lo',50); __drag('NUM1','lo',%s); __drag('NUM1','hi',60);",
    shrunk$min
  ))
  step("NUM2 reset: NUM1's scale grows back", reset_card("NUM2"))
  expect_equal(card("NUM1")$min, min(pt$NUM1))
  expect_equal(filters()$NUM1$min, min(pt$NUM1))

  # -- Filters pushed from R, as a saved board restores them -----------------
  reset_all()
  step("NUM1 and INTBIG pushed from R",
       "window.__cfDebug.setExternalFilters({},
          {pt: {NUM1: [30, 70], INTBIG: [100, 900]}});")
  expect_equal(card("NUM1")[c("lo", "hi")], list(lo = 30, hi = 70))
  expect_equal(card("INTBIG")[c("lo", "hi")], list(lo = 100, hi = 900))
})
