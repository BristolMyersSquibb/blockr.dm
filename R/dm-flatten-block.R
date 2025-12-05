#' dm Flatten Block Constructor
#'
#' This block flattens a dm object into a single data frame by joining
#' all related tables based on their foreign key relationships.
#'
#' @param start_table Character, the table to start flattening from. Default "".
#' @param recursive Logical, whether to recursively join all related tables.
#'   Default TRUE.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A block object for flattening dm objects
#'
#' @details
#' The starting table determines the base of the join. Related tables are
#' joined based on their foreign key relationships. With `recursive = TRUE`,
#' all transitively related tables are included.
#'
#' @examples
#' # Create a dm flatten block
#' new_dm_flatten_block(start_table = "flights", recursive = TRUE)
#'
#' @export
new_dm_flatten_block <- function(start_table = "", recursive = TRUE, ...) {

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Reactive values
          r_start_table <- shiny::reactiveVal(start_table)
          r_recursive <- shiny::reactiveVal(recursive)

          # Update reactives from inputs
          shiny::observeEvent(input$start_table, {
            r_start_table(input$start_table)
          })

          shiny::observeEvent(input$recursive, {
            r_recursive(input$recursive)
          })

          # Update table choices when dm changes
          shiny::observeEvent(data(), {
            dm_obj <- data()
            if (inherits(dm_obj, "dm")) {
              tables <- names(dm::dm_get_tables(dm_obj))
              current <- r_start_table()
              # Keep current selection if valid, otherwise use first table
              selected <- if (current %in% tables) current else tables[1]
              shiny::updateSelectInput(
                session, "start_table",
                choices = tables,
                selected = selected
              )
              r_start_table(selected)
            }
          })

          list(
            expr = shiny::reactive({
              tbl <- r_start_table()
              is_recursive <- r_recursive()

              shiny::req(tbl, nzchar(tbl))

              # Build dm_flatten_to_tbl call
              bquote(
                dm::dm_flatten_to_tbl(data, .(tbl_sym), .recursive = .(is_recursive)),
                list(
                  tbl_sym = as.name(tbl),
                  is_recursive = is_recursive
                )
              )
            }),
            state = list(
              start_table = r_start_table,
              recursive = r_recursive
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
              shiny::tags$h4("Flatten dm"),
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    shiny::NS(id, "start_table"),
                    label = "Start from table",
                    choices = if (nzchar(start_table)) start_table else character(),
                    selected = start_table
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::checkboxInput(
                    shiny::NS(id, "recursive"),
                    label = "Recursive (include all related tables)",
                    value = recursive
                  )
                )
              ),
              shiny::tags$p(
                class = "text-muted",
                "Joins all related tables into a single data frame."
              )
            )
          )
        )
      )
    },
    class = "dm_flatten_block",
    ...
  )
}
