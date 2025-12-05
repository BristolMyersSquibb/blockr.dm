#' dm Filter Block Constructor
#'
#' This block filters a dm object by applying a condition to one of its tables.
#' The filter cascades to related tables via semi-joins based on foreign key
#' relationships, keeping only rows that match across the data model.
#'
#' @param table Character, the name of the table to filter. Default "".
#' @param expr Character, the filter expression (e.g., "AVAL > 100"). Default "".
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A block object for filtering dm objects
#'
#' @details
#' The filter expression is evaluated in the context of the selected table.
#' Use column names from that table directly (e.g., "Species == 'setosa'").
#'
#' When the dm has foreign key relationships defined, filtering one table
#' automatically filters related tables to only include matching rows.
#' This is the key feature for queries like "get adverse events for subjects
#' with lab value > X".
#'
#' @examples
#' # Create a dm filter block
#' new_dm_filter_block(table = "flights", expr = "month == 1")
#'
#' @export
new_dm_filter_block <- function(table = "", expr = "", ...) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Reactive values
          r_table <- shiny::reactiveVal(table)
          r_expr <- shiny::reactiveVal(expr)

          # Debounced expression input
          expr_debounced <- shiny::debounce(shiny::reactive(input$expr), 800)

          # Update reactive from inputs
          shiny::observeEvent(input$table, {
            r_table(input$table)
          })

          shiny::observeEvent(expr_debounced(), {
            r_expr(expr_debounced() %||% "")
          })

          # Update table choices when dm changes
          shiny::observeEvent(data(), {
            dm_obj <- data()
            if (inherits(dm_obj, "dm")) {
              tables <- names(dm::dm_get_tables(dm_obj))
              current <- r_table()
              # Keep current selection if valid, otherwise use first table
              selected <- if (current %in% tables) current else tables[1]
              shiny::updateSelectInput(
                session, "table",
                choices = tables,
                selected = selected
              )
              r_table(selected)
            }
          })

          list(
            expr = shiny::reactive({
              tbl <- r_table()
              filter_expr <- r_expr()

              shiny::req(tbl, nzchar(tbl))

              if (!nzchar(filter_expr)) {
                # No filter, return dm unchanged
                quote(data)
              } else {
                # Build dm_filter call
                # dm::dm_filter(dm, table = (condition))
                filter_call <- tryCatch(
                  parse(text = filter_expr)[[1]],
                  error = function(e) NULL
                )

                if (is.null(filter_call)) {
                  quote(data)
                } else {
                  # Construct call manually since dm_filter uses special syntax
                  # dm_filter(dm, table = (condition))
                  call <- call("dm_filter", quote(data))
                  # Add the table argument with the filter as its value
                  call[[tbl]] <- filter_call
                  # Wrap with namespace
                  call[[1]] <- quote(dm::dm_filter)
                  call
                }
              }
            }),
            state = list(
              table = r_table,
              expr = r_expr
            )
          )
        }
      )
    },
    ui = function(id) {
      shiny::tagList(
        shiny::div(
          class = "block-container",
          shiny::div(
            class = "block-form-grid",
            shiny::div(
              class = "block-section",
              shiny::tags$h4("Filter dm"),
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    shiny::NS(id, "table"),
                    label = "Table to filter",
                    choices = if (nzchar(table)) table else character(),
                    selected = table
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::textInput(
                    shiny::NS(id, "expr"),
                    label = "Filter expression",
                    value = expr,
                    placeholder = "e.g., AVAL > 100"
                  )
                )
              ),
              shiny::tags$p(
                class = "text-muted",
                "Filter cascades to related tables via foreign key relationships."
              )
            )
          )
        )
      )
    },
    allow_empty_state = c("expr"),
    class = "dm_filter_block",
    ...
  )
}

#' @rdname block_output.dm_block
#' @method block_output dm_filter_block
#' @export
block_output.dm_filter_block <- function(x, result, session) {
  block_output.dm_block(x, result, session)
}

#' @rdname block_ui.dm_block
#' @method block_ui dm_filter_block
#' @export
block_ui.dm_filter_block <- function(id, x, ...) {
  block_ui.dm_block(id, x, ...)
}
