#' dm Pull Block Constructor
#'
#' This block extracts a single table from a dm object as a regular data frame.
#' Use this after filtering to get a specific table for further analysis
#' or visualization.
#'
#' @param table Character, the name of the table to extract. Default "".
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A block object for extracting tables from dm objects
#'
#' @examples
#' # Create a dm pull block
#' new_dm_pull_block(table = "flights")
#'
#' @export
new_dm_pull_block <- function(table = "", ...) {

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Reactive value for table selection
          r_table <- shiny::reactiveVal(table)

          # Update reactive from input
          shiny::observeEvent(input$table, {
            r_table(input$table)
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
              shiny::req(tbl, nzchar(tbl))

              # Build pull_tbl call
              bquote(
                dm::pull_tbl(data, .(tbl_sym)),
                list(tbl_sym = as.name(tbl))
              )
            }),
            state = list(
              table = r_table
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
              shiny::tags$h4("Extract Table"),
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    shiny::NS(id, "table"),
                    label = "Table to extract",
                    choices = if (nzchar(table)) table else character(),
                    selected = table
                  )
                )
              ),
              shiny::tags$p(
                class = "text-muted",
                "Extracts selected table as a data frame",
                "for further processing."
              )
            )
          )
        )
      )
    },
    class = "dm_pull_block",
    ...
  )
}
