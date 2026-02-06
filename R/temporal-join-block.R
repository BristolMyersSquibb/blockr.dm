#' Temporal Join Block
#'
#' A standalone temporal join block that takes two data frames as inputs,
#' joins on a key column, computes time difference between date columns,
#' and filters to a time window. Designed for clinical trial data where
#' you need to find events within a time window (e.g., labs within 7 days
#' of an adverse event for causality assessment).
#'
#' @param by Character. Column to join on (e.g., "USUBJID"). If empty,
#'   auto-detected from common columns.
#' @param left_date Character. Date column in x (left table).
#' @param right_date Character. Date column in y (right table).
#' @param window_days Numeric. Time window in days. Default 7.
#' @param direction Character. One of "after", "before", or "around".
#'   "after": right_date 0 to window_days after left_date.
#'   "before": right_date 0 to window_days before left_date.
#'   "around": right_date within window_days in either direction.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A transform block that takes two data frame inputs (x, y) and
#'   returns a joined data frame with a `days_diff` column.
#'
#' @examples
#' # Find labs within 7 days after each adverse event
#' new_temporal_join_block(
#'   by = "USUBJID",
#'   left_date = "ASTDT",
#'   right_date = "ADT",
#'   window_days = 7,
#'   direction = "after"
#' )
#'
#' @export
new_temporal_join_block <- function(
    by = character(),
    left_date = character(),
    right_date = character(),
    window_days = 7,
    direction = "after",
    ...
) {
  direction <- match.arg(direction, c("after", "before", "around"))

  blockr.core::new_transform_block(
    server = function(id, x, y) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns

          # State
          r_by <- shiny::reactiveVal(if (length(by) > 0 && nzchar(by)) by else character())
          r_left_date <- shiny::reactiveVal(if (length(left_date) > 0 && nzchar(left_date)) left_date else character())
          r_right_date <- shiny::reactiveVal(if (length(right_date) > 0 && nzchar(right_date)) right_date else character())
          r_window_days <- shiny::reactiveVal(window_days)
          r_direction <- shiny::reactiveVal(direction)

          # Detect common columns for join key
          common_cols <- shiny::reactive({
            shiny::req(x(), y())
            intersect(colnames(x()), colnames(y()))
          })

          # Detect date columns in a data frame
          get_date_cols <- function(df) {
            if (!is.data.frame(df)) return(character())
            names(df)[vapply(df, function(col) inherits(col, c("Date", "POSIXt")), logical(1))]
          }

          # Update UI when data changes
          shiny::observeEvent(list(x(), y()), {
            x_df <- x()
            y_df <- y()
            shiny::req(is.data.frame(x_df), is.data.frame(y_df))

            common <- intersect(colnames(x_df), colnames(y_df))
            x_dates <- get_date_cols(x_df)
            y_dates <- get_date_cols(y_df)

            # Update by column
            current_by <- r_by()
            if (length(current_by) == 0 || !current_by %in% common) {
              # Auto-detect: pick the first common non-date column
              date_cols <- union(x_dates, y_dates)
              candidates <- setdiff(common, date_cols)
              selected_by <- if (length(candidates) > 0) candidates[1] else {
                if (length(common) > 0) common[1] else ""
              }
              r_by(selected_by)
            } else {
              selected_by <- current_by
            }
            shiny::updateSelectInput(session, "by", choices = common, selected = selected_by)

            # Update left_date
            current_ld <- r_left_date()
            if (length(current_ld) == 0 || !current_ld %in% x_dates) {
              selected_ld <- if (length(x_dates) > 0) x_dates[1] else ""
              r_left_date(selected_ld)
            } else {
              selected_ld <- current_ld
            }
            shiny::updateSelectInput(session, "left_date", choices = x_dates, selected = selected_ld)

            # Update right_date
            current_rd <- r_right_date()
            if (length(current_rd) == 0 || !current_rd %in% y_dates) {
              selected_rd <- if (length(y_dates) > 0) y_dates[1] else ""
              r_right_date(selected_rd)
            } else {
              selected_rd <- current_rd
            }
            shiny::updateSelectInput(session, "right_date", choices = y_dates, selected = selected_rd)
          })

          # Sync UI inputs to state
          shiny::observeEvent(input$by, r_by(input$by), ignoreInit = TRUE)
          shiny::observeEvent(input$left_date, r_left_date(input$left_date), ignoreInit = TRUE)
          shiny::observeEvent(input$right_date, r_right_date(input$right_date), ignoreInit = TRUE)
          shiny::observeEvent(input$window_days, r_window_days(input$window_days), ignoreInit = TRUE)
          shiny::observeEvent(input$direction, r_direction(input$direction), ignoreInit = TRUE)

          list(
            expr = shiny::reactive({
              by_col <- r_by()
              ld <- r_left_date()
              rd <- r_right_date()
              wd <- r_window_days()
              dir <- r_direction()

              shiny::req(
                length(by_col) > 0, nzchar(by_col),
                length(ld) > 0, nzchar(ld),
                length(rd) > 0, nzchar(rd),
                !is.null(wd)
              )

              filter_expr <- switch(dir,
                "after"  = "days_diff >= 0 & days_diff <= window_days",
                "before" = "days_diff >= -window_days & days_diff <= 0",
                "around" = "abs(days_diff) <= window_days"
              )

              expr_text <- sprintf(
                'local({
  window_days <- %s
  dplyr::left_join(x, y, by = "%s", suffix = c("", ".y"), relationship = "many-to-many") |>
    dplyr::mutate(days_diff = as.numeric(.data[["%s"]] - .data[["%s"]])) |>
    dplyr::filter(%s)
})',
                wd, by_col, rd, ld, filter_expr
              )

              parse(text = expr_text)[[1]]
            }),
            state = list(
              by = r_by,
              left_date = r_left_date,
              right_date = r_right_date,
              window_days = r_window_days,
              direction = r_direction
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)

      shiny::tagList(
        shiny::div(
          class = "block-container",
          block_responsive_css(),
          block_container_script(),

          shiny::div(
            class = "block-form-grid",

            # Join key
            shiny::div(
              class = "block-section",
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    ns("by"),
                    label = "Join key",
                    choices = if (length(by) > 0 && nzchar(by)) by else character(),
                    selected = if (length(by) > 0 && nzchar(by)) by else NULL,
                    width = "100%"
                  )
                )
              )
            ),

            # Date columns
            shiny::div(
              class = "block-section",
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    ns("left_date"),
                    label = "Date in x (left)",
                    choices = if (length(left_date) > 0 && nzchar(left_date)) left_date else character(),
                    selected = if (length(left_date) > 0 && nzchar(left_date)) left_date else NULL,
                    width = "100%"
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    ns("right_date"),
                    label = "Date in y (right)",
                    choices = if (length(right_date) > 0 && nzchar(right_date)) right_date else character(),
                    selected = if (length(right_date) > 0 && nzchar(right_date)) right_date else NULL,
                    width = "100%"
                  )
                )
              )
            ),

            # Window settings
            shiny::div(
              class = "block-section",
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::sliderInput(
                    ns("window_days"),
                    label = "Window (days)",
                    min = 0,
                    max = 90,
                    value = window_days,
                    step = 1,
                    width = "100%"
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    ns("direction"),
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
    allow_empty_state = c("by", "left_date", "right_date"),
    class = "temporal_join_block",
    ...
  )
}
