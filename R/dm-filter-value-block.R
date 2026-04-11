#' dm Filter Value Block Constructor
#'
#' This block filters a dm object by selecting a table, column, and value from
#' dropdown menus. The filter cascades to related tables via semi-joins based on
#' foreign key relationships. This is a point-and-click alternative to
#' [new_dm_filter_block()] which requires free-text expressions.
#'
#' @param table Character, the name of the table to filter. Default "".
#' @param column Character, the column to filter on. Default "".
#' @param value Character, the value to filter for. Default "".
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A block object for filtering dm objects by value
#'
#' @details
#' Three cascading dropdowns let the user pick a table, then a column within
#' that table, then a specific value from that column. The resulting filter is
#' equivalent to `dm::dm_filter(dm, <table> = <column> == <value>)`.
#'
#' When the dm has foreign key relationships defined, filtering one table
#' automatically filters related tables to only include matching rows.
#'
#' @examples
#' new_dm_filter_value_block(table = "adsl", column = "SEX", value = "F")
#'
#' @export
new_dm_filter_value_block <- function(
  table = "", column = "", value = "", ...
) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_table <- shiny::reactiveVal(table)
          r_column <- shiny::reactiveVal(column)
          r_value <- shiny::reactiveVal(value)

          # Update reactive values from inputs
          shiny::observeEvent(input$table, {
            r_table(input$table)
          })

          shiny::observeEvent(input$column, {
            r_column(input$column %||% "")
          })

          shiny::observeEvent(input$value, {
            r_value(input$value %||% "")
          })

          # Populate table + column choices when data arrives
          shiny::observe({
            dm_obj <- data()
            shiny::req(inherits(dm_obj, "dm"))
            tables <- names(dm::dm_get_tables(dm_obj))

            # Update table dropdown
            current_tbl <- r_table()
            selected_tbl <- if (current_tbl %in% tables) {
              current_tbl
            } else {
              tables[1]
            }
            shiny::updateSelectInput(
              session, "table",
              choices = tables,
              selected = selected_tbl
            )
            r_table(selected_tbl)

            # Update column dropdown (depends on selected table)
            tbl <- selected_tbl
            if (!nzchar(tbl) || !tbl %in% tables) return()
            tbl_data <- dm_obj[[tbl]]
            cols <- names(tbl_data)
            current_col <- r_column()
            selected_col <- if (current_col %in% cols) current_col else ""
            shiny::updateSelectizeInput(
              session, "column",
              choices = cols,
              selected = selected_col,
              options = list(placeholder = "Select column...")
            )
            r_column(selected_col)
          })

          # Populate value choices whenever table or column changes
          shiny::observe({
            dm_obj <- data()
            tbl <- r_table()
            col <- r_column()
            shiny::req(inherits(dm_obj, "dm"), nzchar(tbl), nzchar(col))

            tbl_names <- names(dm::dm_get_tables(dm_obj))
            shiny::req(tbl %in% tbl_names)
            tbl_data <- dm_obj[[tbl]]
            shiny::req(col %in% names(tbl_data))

            vals <- sort(unique(as.character(tbl_data[[col]])))
            current_val <- r_value()
            selected_val <- if (current_val %in% vals) current_val else ""
            shiny::updateSelectizeInput(
              session, "value",
              choices = vals,
              selected = selected_val,
              options = list(placeholder = "Select value...")
            )
            r_value(selected_val)
          })

          list(
            expr = shiny::reactive({
              tbl <- r_table()
              col <- r_column()
              val <- r_value()

              shiny::req(tbl, nzchar(tbl))

              if (!nzchar(col) || !nzchar(val)) {
                quote(identity(data))
              } else {
                # Detect numeric values
                num_val <- suppressWarnings(as.numeric(val))
                filter_val <- if (!is.na(num_val)) num_val else val

                # Equality filter call
                filter_call <- call("==", as.symbol(col), filter_val)

                # Build: dm::dm_filter(data, <table> = <condition>)
                cl <- call("dm_filter", quote(data))
                cl[[tbl]] <- filter_call
                cl[[1]] <- quote(dm::dm_filter)
                cl
              }
            }),
            state = list(
              table = r_table,
              column = r_column,
              value = r_value
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
              shiny::tags$h4("Filter dm by value"),
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    shiny::NS(id, "table"),
                    label = "Table",
                    choices = if (nzchar(table)) table else character(),
                    selected = table
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectizeInput(
                    shiny::NS(id, "column"),
                    label = "Column",
                    choices = if (nzchar(column)) column else character(),
                    selected = column,
                    options = list(placeholder = "Select column...")
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectizeInput(
                    shiny::NS(id, "value"),
                    label = "Value",
                    choices = if (nzchar(value)) value else character(),
                    selected = value,
                    options = list(placeholder = "Select value...")
                  )
                )
              ),
              shiny::tags$p(
                class = "text-muted",
                "Filter cascades to related tables",
                "via foreign key relationships."
              )
            )
          )
        )
      )
    },
    allow_empty_state = c("column", "value"),
    class = "dm_filter_value_block",
    ...
  )
}

#' @rdname block_output.dm_block
#' @method block_output dm_filter_value_block
#' @export
block_output.dm_filter_value_block <- function(x, result, session) {
  block_output.dm_block(x, result, session)
}

#' @rdname block_ui.dm_block
#' @method block_ui dm_filter_value_block
#' @export
block_ui.dm_filter_value_block <- function(id, x, ...) {
  block_ui.dm_block(id, x, ...)
}
