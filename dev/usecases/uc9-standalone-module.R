# Use Case 9: Standalone crossfilter module (no blockr.dag / run_app)
#
# Demonstrates dm_filter_ui() / dm_filter_server() as a plain Shiny module
# embedded in a regular Shiny app. No blockr board, no DAG, no blocks/links.
#
# The module takes a dm object as input and returns a reactive filtered dm.

pkgload::load_all("../blockr.dm")

library(shiny)
library(safetyData)

# --- Build dm outside of Shiny ---
tables <- list(
  adsl = adam_adsl,
  adae = adam_adae,
  adlb = adam_adlbc,
  advs = adam_advs
)
dm_obj <- do.call(dm::dm, tables)
dm_obj <- blockr.dm:::infer_keys_from_column_names(dm_obj)

# --- App ---
ui <- fluidPage(
  titlePanel("ADaM Crossfilter (standalone module)"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      dm_filter_ui("cf")
    ),
    mainPanel(
      width = 8,
      h4("Filtered ADSL"),
      reactable::reactableOutput("tbl"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output, session) {
  filtered <- dm_filter_server("cf", reactiveVal(dm_obj),
    active_dims = list(adsl = c("SEX", "AGE"), adae = c("AESEV"))
  )

  output$tbl <- reactable::renderReactable({
    reactable::reactable(filtered()[["adsl"]], compact = TRUE, pagination = TRUE)
  })

  output$summary <- renderPrint({
    dm <- filtered()
    cat("Tables:", paste(names(dm::dm_get_tables(dm)), collapse = ", "), "\n")
    for (tbl in names(dm::dm_get_tables(dm))) {
      cat(sprintf("  %s: %d rows\n", tbl, nrow(dm[[tbl]])))
    }
  })
}

shinyApp(ui, server)
