#' Select tables from a dm object
#'
#' A block for selecting a subset of tables from a dm (data model) object.
#' Uses `dm::dm_select_tbl()` to keep only the specified tables.
#'
#' @param tables Character vector. Names of tables to keep. If empty,
#'   all tables are kept (no filtering).
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A blockr transform block that filters tables from a dm object
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dm)
#'   serve(new_dm_select_block())
#' }
#'
#' @export
new_dm_select_block <- function(tables = character(), ...) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Reactive value for selected tables
          r_tables <- shiny::reactiveVal(tables)

          # Get available tables from dm
          available_tables <- shiny::reactive({
            dm_obj <- data()
            if (!inherits(dm_obj, "dm")) return(character())
            names(dm::dm_get_tables(dm_obj))
          })

          # Update selectize choices when data changes
          shiny::observeEvent(available_tables(), {
            choices <- available_tables()
            current <- r_tables()
            # Keep only valid selections
            valid_selection <- intersect(current, choices)
            shiny::updateSelectizeInput(
              session, "tables",
              choices = choices,
              selected = if (length(valid_selection) > 0) valid_selection else choices
            )
          })

          # Update state from input
          shiny::observeEvent(input$tables, {
            r_tables(input$tables)
          }, ignoreNULL = FALSE)

          list(
            expr = shiny::reactive({
              selected <- r_tables()
              all_tables <- available_tables()

              # If no selection, use all tables
              if (length(selected) == 0) {
                selected <- all_tables
              }

              # Build dm_select_tbl call with table symbols
              # Always build the call (quote(data) is a symbol, not language)
              table_syms <- lapply(selected, as.name)
              call_args <- c(
                list(quote(dm::dm_select_tbl)),
                list(quote(data)),
                table_syms
              )
              as.call(call_args)
            }),
            state = list(
              tables = r_tables
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        block_responsive_css(),
        shiny::div(
          class = "block-container",
          shiny::tags$p(
            class = "text-muted mb-2",
            "Select which tables to keep in the dm object."
          ),
          shiny::selectizeInput(
            ns("tables"),
            label = "Tables to keep",
            choices = tables,
            selected = tables,
            multiple = TRUE,
            options = list(
              placeholder = "Select tables...",
              plugins = list("remove_button")
            )
          )
        )
      )
    },
    dat_valid = function(data) {
      if (!inherits(data, "dm")) {
        stop("Input must be a dm object")
      }
    },
    allow_empty_state = TRUE,
    class = "dm_select_block",
    ...
  )
}


#' @method block_output dm_select_block
#' @export
block_output.dm_select_block <- function(x, result, session) {
  block_output.dm_block(x, result, session)
}


#' @method block_ui dm_select_block
#' @export
block_ui.dm_select_block <- function(id, x, ...) {
  block_ui.dm_block(id, x, ...)
}


#' @method block_render_trigger dm_select_block
#' @export
block_render_trigger.dm_select_block <- function(x, session = blockr.core::get_session()) {
  NULL
}
