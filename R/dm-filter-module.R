#' Standalone Crossfilter Module for dm Objects
#'
#' A standard Shiny module pair that provides interactive crossfilter UI for
#' [dm][dm::dm()] objects. Unlike [new_dm_crossfilter_block()], this module can
#' be embedded in any Shiny app without requiring blockr.core, blockr.dag, or
#' the blockr board infrastructure.
#'
#' @param id Character string. The Shiny module namespace ID.
#'
#' @return `dm_filter_ui` returns a Shiny UI tag list containing the
#'   crossfilter search bar, filter widgets, and status display.
#'
#' @seealso [new_dm_crossfilter_block()] for the blockr-integrated version.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(dm)
#'
#'   orders <- data.frame(
#'     id = 1:100,
#'     region = sample(c("North", "South", "East", "West"), 100, replace = TRUE),
#'     amount = runif(100, 10, 1000)
#'   )
#'   items <- data.frame(
#'     order_id = sample(1:100, 200, replace = TRUE),
#'     product = sample(c("A", "B", "C"), 200, replace = TRUE)
#'   )
#'   my_dm <- dm(orders = orders, items = items) |>
#'     dm_add_pk(orders, id) |>
#'     dm_add_fk(items, order_id, orders)
#'
#'   ui <- fluidPage(
#'     column(4, dm_filter_ui("cf")),
#'     column(8, tableOutput("tbl"))
#'   )
#'
#'   server <- function(input, output, session) {
#'     filtered <- dm_filter_server("cf", reactiveVal(my_dm),
#'       active_dims = list(orders = c("region"), items = c("product"))
#'     )
#'     output$tbl <- renderTable(filtered()[["orders"]])
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
dm_filter_ui <- function(id) {
  ns <- shiny::NS(id)
  dm_crossfilter_ui(ns("cf"))
}

#' @param dm A reactive or [shiny::reactiveVal()] returning a [dm::dm()] object.
#'   The dm should have primary and foreign keys defined for cross-table
#'   filtering to work.
#' @param active_dims Named list of per-table active filter columns. Each
#'   element is a character vector of column names to show as initial filter
#'   widgets. E.g., `list(adsl = c("SEX", "AGE"), adae = c("AESEV"))`.
#'   Defaults to an empty list (no filters shown initially; users add via
#'   search).
#' @param filters Named list of per-table categorical filters (initial state).
#'   E.g., `list(adsl = list(SEX = c("F")))`.
#' @param range_filters Named list of per-table range filters (initial state).
#'   E.g., `list(adsl = list(AGE = c(65, 80)))`.
#' @param measure Measure column for aggregation, as `"table.column"`
#'   (e.g., `"orders.amount"`). When set, categorical filter bars show
#'   `SUM(column)` instead of row counts. Defaults to `NULL` (row count).
#'
#' @return `dm_filter_server` returns a reactive that yields the filtered
#'   [dm::dm()] object. When no filters are active, the original dm is returned
#'   unchanged.
#'
#' @rdname dm_filter_ui
#' @export
dm_filter_server <- function(id, dm,
                             active_dims = list(),
                             filters = list(),
                             range_filters = list(),
                             measure = NULL) {
  factory_server <- dm_crossfilter_server_factory(
    active_dims, filters, range_filters, measure
  )

  shiny::moduleServer(id, function(input, output, session) {
    result <- factory_server("cf", dm)

    shiny::reactive({
      expr <- result$expr()
      dm_obj <- dm()
      shiny::req(inherits(dm_obj, "dm"))
      eval(expr, list(data = dm_obj))
    })
  })
}
