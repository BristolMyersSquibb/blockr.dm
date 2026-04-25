# Diagnostic harness for crossfilter state-consistency.
#
# Mounts the crossfilter UI alongside live verbatim outputs that show the
# R-side reactive state and post-dm_filter row counts. The point is to make
# state mismatches between the JS UI, R reactives, and downstream blocks
# directly visible — useful for chasing the "ARM/SEX cards empty while
# demographics shows N>0" symptom.
#
# Run with:
#   Rscript blockr.dm/dev/diag-crossfilter.R
# Open http://localhost:3838

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dm")

library(shiny)

# Keep this file the single source of debug logging so source mods aren't
# needed. Tail with: tail -f /tmp/blockr-diag.log
DEBUG_LOG <- "/tmp/blockr-diag.log"
if (file.exists(DEBUG_LOG)) file.remove(DEBUG_LOG)
debug_log <- function(tag, ...) {
  msg <- paste(format(Sys.time(), "%H:%M:%OS3"), sprintf("[%s]", tag), ...)
  cat(msg, "\n", file = DEBUG_LOG, append = TRUE, sep = "")
}

# Build the same dm shape as safety-explorer (subset of tables, same FKs).
build_dm <- function() {
  d <- dm::dm(
    adsl  = safetyData::adam_adsl,
    adae  = safetyData::adam_adae,
    adlbc = safetyData::adam_adlbc
  )
  d <- dm::dm_add_pk(d, adsl, USUBJID)
  d <- dm::dm_add_fk(d, adae, USUBJID, adsl)
  d <- dm::dm_add_fk(d, adlbc, USUBJID, adsl)
  d
}

ACTIVE_DIMS <- list(
  adsl  = c("ARM", "SEX"),
  adae  = c("AESEV", "AEBODSYS"),
  adlbc = c("PARAMCD", "AVAL")
)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: system-ui, sans-serif; }
    .diag-col { padding: 12px; border-left: 1px solid #e5e7eb; }
    pre { font-size: 12px; background: #f9fafb; padding: 8px; max-height: 280px; overflow: auto; }
    h4 { margin: 12px 0 6px; font-size: 14px; color: #111827; }
    .invariant-ok   { color: #047857; font-weight: 600; }
    .invariant-fail { color: #b91c1c; font-weight: 700; }
  "))),
  titlePanel("crossfilter diagnostic harness"),
  fluidRow(
    column(5,
      h4("Crossfilter UI"),
      blockr.dm:::crossfilter_ui("xf")
    ),
    column(7, class = "diag-col",
      h4("Invariant: ARM-card sum == filtered adsl rows"),
      uiOutput("invariant"),
      h4("R reactive state"),
      verbatimTextOutput("rstate"),
      h4("Post dm_filter row counts (per table)"),
      verbatimTextOutput("counts"),
      h4("Filter expr (deparsed)"),
      verbatimTextOutput("expr_text"),
      h4("ARM breakdown (post-filter adsl)"),
      verbatimTextOutput("arm_breakdown"),
      h4("Tip"),
      tags$pre(paste(
        "In browser console:",
        "  window.__cfDebug              // the CrossfilterBlock instance",
        "  __cfDebug.filters             // current categorical/range filters",
        "  __cfDebug.groups.ARM.all()    // group counts for ARM",
        "  __cfDebug.instances.adae.allFiltered().length",
        sep = "\n"
      ))
    )
  ),
  # 1) Expose el._block as window.__cfDebug as soon as the input binding
  #    initialises it. 2) Install a hook that pushes the ARM-group sum back
  #    to R after every render so the cross-layer invariant can be checked.
  tags$script(HTML("
    (function() {
      const tick = setInterval(() => {
        const el = document.querySelector('.js-crossfilter-container');
        if (!(el && el._block)) return;
        clearInterval(tick);
        const b = window.__cfDebug = el._block;
        console.log('[diag] window.__cfDebug ready');
        const orig = b._renderCategoricalCounts.bind(b);
        b._renderCategoricalCounts = function(dim, counts) {
          orig(dim, counts);
          if (dim === 'ARM') {
            const gv = (d) => b._getGroupValue(d);
            const sum = counts.filter(d => gv(d) > 0)
              .reduce((a, d) => a + gv(d), 0);
            Shiny.setInputValue('diag_arm_sum', sum, {priority: 'event'});
          }
        };
        b._updateAllCounts(null);
      }, 200);
    })();
  "))
)

server <- function(input, output, session) {
  data_r <- reactive(build_dm())

  result <- blockr.dm:::crossfilter_server(
    active_dims = ACTIVE_DIMS, filters = list(), range_filters = list(),
    measure = NULL, agg_func = NULL
  )("xf", data_r)

  # Eval the expr against the dm. Safe to eval here because this harness is
  # a controlled environment — same as what blockr.core does for transform
  # blocks downstream.
  filtered_dm <- reactive({
    expr_val <- result$expr()
    debug_log("EXPR", deparse1(expr_val, collapse = " "))
    eval(expr_val, envir = list(data = data_r()))
  })

  output$rstate <- renderPrint({
    list(
      filters       = result$state$filters(),
      range_filters = result$state$range_filters(),
      active_dims   = result$state$active_dims()
    )
  })

  output$expr_text <- renderPrint({
    cat(deparse1(result$expr(), collapse = "\n"))
  })

  output$counts <- renderPrint({
    fdm <- filtered_dm()
    tbls <- dm::dm_get_tables(fdm)
    n <- vapply(tbls, function(t) nrow(as.data.frame(t)), integer(1))
    debug_log("COUNTS", paste(names(n), n, sep = "=", collapse = " "))
    n
  })

  output$arm_breakdown <- renderPrint({
    fdm <- filtered_dm()
    tbls <- dm::dm_get_tables(fdm)
    adsl <- as.data.frame(tbls$adsl)
    if (nrow(adsl) == 0) return("(0 rows)")
    table(adsl$ARM, useNA = "ifany")
  })

  # Cross-layer invariant: filtered adsl row count must match the sum of
  # ARM-group counts the JS card claims. JS pushes its computed sum back
  # via a custom message handler we install on the client (see below).
  r_js_arm_sum <- reactiveVal(NA_integer_)
  observeEvent(input$diag_arm_sum, {
    r_js_arm_sum(as.integer(input$diag_arm_sum))
    debug_log("JS-ARM-SUM", input$diag_arm_sum)
  })

  output$invariant <- renderUI({
    fdm <- filtered_dm()
    adsl_n <- nrow(as.data.frame(dm::dm_get_tables(fdm)$adsl))
    js_sum <- r_js_arm_sum()
    if (is.na(js_sum)) {
      tags$div(class = "invariant-ok",
        sprintf("R adsl=%d, JS ARM sum=(unset — interact with a filter)", adsl_n))
    } else if (adsl_n == js_sum) {
      tags$div(class = "invariant-ok",
        sprintf("OK — R adsl=%d == JS ARM sum=%d", adsl_n, js_sum))
    } else {
      tags$div(class = "invariant-fail",
        sprintf("MISMATCH — R adsl=%d, JS ARM sum=%d (delta=%d)",
                adsl_n, js_sum, adsl_n - js_sum))
    }
  })

}

port <- as.integer(Sys.getenv("DIAG_PORT", "3838"))
options(shiny.port = port, shiny.host = "0.0.0.0", shiny.launch.browser = FALSE)
shinyApp(ui, server)
