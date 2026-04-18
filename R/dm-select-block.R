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
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList req isolate
#'
#' @export
new_dm_select_block <- function(tables = character(), ...) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns
          r_tables <- reactiveVal(tables)

          available_tables <- reactive({
            dm_obj <- data()
            if (!inherits(dm_obj, "dm")) return(character())
            names(dm::dm_get_tables(dm_obj))
          })

          observeEvent(data(), {
            opts <- build_dm_table_options(data())
            choices <- vapply(opts, `[[`, character(1), "value")
            current <- isolate(r_tables())
            valid_selection <- intersect(current, choices)
            selected <- if (length(valid_selection) > 0) {
              valid_selection
            } else {
              choices
            }
            session$sendCustomMessage(
              "dm-table-picker",
              list(
                id = ns("tables"),
                mode = "multi",
                options = opts,
                selected = selected,
                placeholder = "Select tables\u2026"
              )
            )
            r_tables(selected)
          })

          observeEvent(input$tables, {
            val <- input$tables
            if (is.null(val)) return()
            r_tables(val)
          })

          list(
            expr = reactive({
              selected <- r_tables()
              all_tables <- available_tables()

              if (length(selected) == 0) {
                selected <- all_tables
              }

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
      tagList(
        dm_table_picker_deps(),
        div(
          class = "block-container",
          shiny::tags$p(
            class = "text-muted mb-2",
            "Select which tables to keep in the dm object."
          ),
          div(
            id = NS(id, "tables"),
            class = "dm-select-tables-picker"
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
block_render_trigger.dm_select_block <- function(
  x,
  session = blockr.core::get_session()
) {
  NULL
}
