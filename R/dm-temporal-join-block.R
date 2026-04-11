#' dm Temporal Join Block Constructor
#'
#' This block creates a temporal join between two tables in a dm object.
#' It finds rows from the right table that fall within a time window of rows
#' in the left table, useful for causality assessment in clinical data.
#'
#' Returns a data frame with the joined result.
#'
#' @param left_table Character, the name of the left table
#'   (e.g., "adae"). Default "".
#' @param left_date Character, the date column in left table
#'   (e.g., "ASTDT"). Default "".
#' @param right_table Character, the name of the right table
#'   (e.g., "adlb"). Default "".
#' @param right_date Character, the date column in right table
#'   (e.g., "ADT"). Default "".
#' @param window_days Numeric, the time window in days.
#'   Default 7.
#' @param direction Character, one of "after", "before", or
#'   "around". Default "after".
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A block object that outputs a data frame with the
#'   temporal join result
#'
#' @details
#' The temporal join:
#' 1. Joins left and right tables by their common key (usually USUBJID)
#' 2. Calculates days between left_date and right_date
#' 3. Filters to rows within the window based on direction:
#'    - "after": 0 <= days_diff <= window_days (right after left)
#'    - "before": -window_days <= days_diff <= 0 (right before left)
#'    - "around": abs(days_diff) <= window_days (either direction)
#' 4. Returns the result as a data frame
#'
#' @examples
#' # Find labs within 7 days after each adverse event
#' new_dm_temporal_join_block(
#'   left_table = "adae",
#'   left_date = "ASTDT",
#'   right_table = "adlb",
#'   right_date = "ADT",
#'   window_days = 7,
#'   direction = "after"
#' )
#'
#' @export
new_dm_temporal_join_block <- function(
  left_table = "",
  left_date = "",
  right_table = "",
  right_date = "",
  window_days = 7,
  direction = "after",
  ...
) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Reactive values for state
          r_left_table <- shiny::reactiveVal(left_table)
          r_left_date <- shiny::reactiveVal(left_date)
          r_right_table <- shiny::reactiveVal(right_table)
          r_right_date <- shiny::reactiveVal(right_date)
          r_window_days <- shiny::reactiveVal(window_days)
          r_direction <- shiny::reactiveVal(direction)

          # Update reactives from inputs
          shiny::observeEvent(
            input$left_table, r_left_table(input$left_table)
          )
          shiny::observeEvent(
            input$right_table, r_right_table(input$right_table)
          )
          shiny::observeEvent(
            input$left_date, r_left_date(input$left_date)
          )
          shiny::observeEvent(
            input$right_date, r_right_date(input$right_date)
          )
          shiny::observeEvent(
            input$window_days, r_window_days(input$window_days)
          )
          shiny::observeEvent(input$direction, r_direction(input$direction))

          # Get table names from dm
          table_choices <- shiny::reactive({
            dm_obj <- data()
            if (inherits(dm_obj, "dm")) {
              names(dm::dm_get_tables(dm_obj))
            } else {
              character()
            }
          })

          # Get date columns for a table
          get_date_columns <- function(dm_obj, table_name) {
            if (!inherits(dm_obj, "dm") || !nzchar(table_name)) {
              return(character())
            }
            if (!table_name %in% names(dm::dm_get_tables(dm_obj))) {
              return(character())
            }
            tbl <- dm_obj[[table_name]]
            cols <- names(tbl)
            # Filter to date/datetime columns
            date_cols <- cols[sapply(cols, function(col) {
              inherits(tbl[[col]], c("Date", "POSIXt"))
            })]
            date_cols
          }

          # Update table choices when dm changes
          shiny::observeEvent(data(), {
            tables <- table_choices()
            if (length(tables) > 0) {
              # Left table
              current_left <- r_left_table()
              selected_left <- if (current_left %in% tables) {
                current_left
              } else {
                tables[1]
              }
              shiny::updateSelectInput(
                session, "left_table",
                choices = tables, selected = selected_left
              )
              r_left_table(selected_left)

              # Right table
              current_right <- r_right_table()
              selected_right <- if (current_right %in% tables) {
                current_right
              } else if (length(tables) > 1) {
                tables[2]
              } else {
                tables[1]
              }
              shiny::updateSelectInput(
                session, "right_table",
                choices = tables, selected = selected_right
              )
              r_right_table(selected_right)
            }
          })

          # Update left date column choices
          shiny::observeEvent(list(data(), r_left_table()), {
            dm_obj <- data()
            tbl_name <- r_left_table()
            date_cols <- get_date_columns(dm_obj, tbl_name)
            current <- r_left_date()
            selected <- if (current %in% date_cols) {
              current
            } else if (length(date_cols) > 0) {
              date_cols[1]
            } else {
              ""
            }
            shiny::updateSelectInput(
              session, "left_date",
              choices = date_cols, selected = selected
            )
            if (nzchar(selected)) r_left_date(selected)
          })

          # Update right date column choices
          shiny::observeEvent(list(data(), r_right_table()), {
            dm_obj <- data()
            tbl_name <- r_right_table()
            date_cols <- get_date_columns(dm_obj, tbl_name)
            current <- r_right_date()
            selected <- if (current %in% date_cols) {
              current
            } else if (length(date_cols) > 0) {
              date_cols[1]
            } else {
              ""
            }
            shiny::updateSelectInput(
              session, "right_date",
              choices = date_cols, selected = selected
            )
            if (nzchar(selected)) r_right_date(selected)
          })

          list(
            expr = shiny::reactive({
              left_tbl <- r_left_table()
              right_tbl <- r_right_table()
              left_dt <- r_left_date()
              right_dt <- r_right_date()
              window <- r_window_days()
              dir <- r_direction()

              # Validate inputs
              shiny::req(
                nzchar(left_tbl), nzchar(right_tbl),
                nzchar(left_dt), nzchar(right_dt),
                !is.null(window)
              )

              # Build filter expression based on direction
              filter_expr <- switch(dir,
                "after" = "days_diff >= 0 & days_diff <= window_days",
                "before" = "days_diff >= -window_days & days_diff <= 0",
                "around" = "abs(days_diff) <= window_days"
              )

              # Create the expression - returns a data frame
              expr_text <- sprintf('
local({

  window_days <- %d
  dm_obj <- data
  left_tbl <- dm_obj[["%s"]]
  right_tbl <- dm_obj[["%s"]]

  # Find common key column (usually USUBJID)
  common_cols <- intersect(names(left_tbl), names(right_tbl))
  key_col <- common_cols[!common_cols %%in%% c("%s", "%s")][1]

  if (is.na(key_col)) {
    stop("No common key column found between tables")
  }

  # Perform temporal join and return data frame
 left_tbl |>
    dplyr::inner_join(
      right_tbl,
      by = key_col,
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      days_diff = as.numeric(.data[["%s"]] - .data[["%s"]])
    ) |>
    dplyr::filter(%s)
})',
                window,
                left_tbl,
                right_tbl,
                left_dt, right_dt,
                right_dt, left_dt,
                filter_expr
              )

              parse(text = expr_text)[[1]]
            }),
            state = list(
              left_table = r_left_table,
              left_date = r_left_date,
              right_table = r_right_table,
              right_date = r_right_date,
              window_days = r_window_days,
              direction = r_direction
            )
          )
        }
      )
    },
    ui = function(id) {
      shiny::tagList(
        shiny::div(
          class = "block-container",

          # Add responsive CSS
          block_responsive_css(),

          # Set container query context
          block_container_script(),

          # Form inputs
          shiny::div(
            class = "block-form-grid",

            # Header section
            shiny::div(
              class = "block-section",
              shiny::tags$h4("Temporal Join"),
              shiny::div(
                class = "block-help-text",
                shiny::tags$p(
                  "Find rows from right table within",
                  "time window of left table"
                )
              )
            ),

            # Left table section
            shiny::div(
              class = "block-section",
              shiny::tags$h4("Left Table (anchor)"),
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    shiny::NS(id, "left_table"),
                    label = "Table",
                    choices = if (nzchar(left_table)) {
                      left_table
                    } else {
                      character()
                    },
                    selected = left_table,
                    width = "100%"
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    shiny::NS(id, "left_date"),
                    label = "Date column",
                    choices = if (nzchar(left_date)) left_date else character(),
                    selected = left_date,
                    width = "100%"
                  )
                )
              )
            ),

            # Right table section
            shiny::div(
              class = "block-section",
              shiny::tags$h4("Right Table (to match)"),
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    shiny::NS(id, "right_table"),
                    label = "Table",
                    choices = if (nzchar(right_table)) {
                      right_table
                    } else {
                      character()
                    },
                    selected = right_table,
                    width = "100%"
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    shiny::NS(id, "right_date"),
                    label = "Date column",
                    choices = if (nzchar(right_date)) {
                      right_date
                    } else {
                      character()
                    },
                    selected = right_date,
                    width = "100%"
                  )
                )
              )
            ),

            # Window settings section
            shiny::div(
              class = "block-section",
              shiny::tags$h4("Time Window"),
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::numericInput(
                    shiny::NS(id, "window_days"),
                    label = "Days",
                    value = window_days,
                    min = 1,
                    max = 365,
                    width = "100%"
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    shiny::NS(id, "direction"),
                    label = "Direction",
                    choices = c("after", "before", "around"),
                    selected = direction,
                    width = "100%"
                  )
                )
              )
            )
          )
        )
      )
    },
    class = "dm_temporal_join_block",
    ...
  )
}
