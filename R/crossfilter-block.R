#' @importFrom rlang .data :=
#' @importFrom stats setNames
#' @importFrom utils head
NULL

# Declare global variables to avoid R CMD check notes
utils::globalVariables(c(".count", ".selected"))

#' Crossfilter Block
#'
#' A crossfilter block using reactable tables and range sliders. Shows multiple
#' sortable, searchable tables for categorical dimensions with inline bar charts,
#' and range sliders for high-cardinality numeric columns.
#' Clicking on a row filters the data to only rows matching that selection.
#' Each widget uses the exclude-own-dimension pattern: it shows all its values
#' while reflecting filters from other dimensions.
#' Returns the filtered data frame.
#'
#' @param dimensions Character vector. Which columns to show as dimension tables.
#'   If NULL, auto-detected from data (non-numeric + low-cardinality numeric).
#' @param range_dimensions Character vector. Which numeric columns get range sliders.
#'   If NULL, auto-detected: numeric columns with >10 unique values that aren't the
#'   selected measure.
#' @param measure Character. Which numeric column to aggregate in the tables.
#'   Default is the first numeric column found. User can change via dropdown.
#' @param filters Named list. Active filters per dimension. Each element is a character
#'   vector of selected values. Default is empty list (no filters). This is saved as
#'   part of block state and restored when the block is restored.
#' @param range_filters Named list. Active range filters for numeric columns. Each
#'   element is a numeric vector of length 2 (min, max). Default is empty list.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A blockr transform block that returns filtered data
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dm)
#'
#'   serve(
#'     new_crossfilter_block(),
#'     data = list(data = iris)
#'   )
#' }
new_crossfilter_block <- function(
    dimensions = NULL,
    range_dimensions = NULL,
    measure = NULL,
    filters = list(),
    range_filters = list(),
    ...
) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns

          # Detect dimensions, range dimensions, and measures from data
          # Numeric columns with few unique values (<=10) are treated as dimensions
          # Numeric columns with many unique values get range sliders
          column_info <- shiny::reactive({
            df <- data()
            if (!is.data.frame(df) || ncol(df) == 0) {
              return(list(
                all_columns = character(),
                suggested_dimensions = character(),
                suggested_range_dimensions = character(),
                measures = character()
              ))
            }

            is_numeric <- vapply(df, is.numeric, logical(1))

            # Low-cardinality numeric columns are dimensions (e.g., Year, Quarter)
            is_low_cardinality <- vapply(df, function(col) {
              length(unique(col)) <= 10
            }, logical(1))

            # Dimension: non-numeric OR (numeric AND low-cardinality)
            is_dimension <- !is_numeric | (is_numeric & is_low_cardinality)

            # Range dimension: numeric AND high-cardinality (candidates for sliders)
            is_range_dim <- is_numeric & !is_low_cardinality

            list(
              all_columns = names(df),
              suggested_dimensions = names(df)[is_dimension],
              suggested_range_dimensions = names(df)[is_range_dim],
              measures = names(df)[is_numeric & !is_low_cardinality]
            )
          })

          # State: selected dimensions
          r_dimensions <- shiny::reactiveVal(dimensions)

          # State: selected range dimensions
          r_range_dimensions <- shiny::reactiveVal(range_dimensions)

          # State: selected measure
          r_measure <- shiny::reactiveVal(measure)

          # Update dimension choices when data changes
          shiny::observeEvent(column_info(), {
            info <- column_info()

            # Skip if data not ready yet — don't clobber constructor values
            # with empty defaults
            if (length(info$all_columns) == 0) return()

            current_dims <- r_dimensions()

            # Set defaults if dimensions is NULL or empty
            if (is.null(current_dims) || length(current_dims) == 0) {
              current_dims <- info$suggested_dimensions
              r_dimensions(current_dims)
            } else if (!all(current_dims %in% info$all_columns)) {
              # Constructor provided dimensions that don't exist in data - fall back
              current_dims <- info$suggested_dimensions
              r_dimensions(current_dims)
            }

            shiny::updateSelectizeInput(
              session, "dimensions",
              choices = info$all_columns,
              selected = current_dims
            )

            # Auto-detect range dimensions if NULL or empty
            current_range <- r_range_dimensions()
            if (is.null(current_range) || length(current_range) == 0) {
              r_range_dimensions(info$suggested_range_dimensions)
            } else if (!all(current_range %in% info$all_columns)) {
              r_range_dimensions(info$suggested_range_dimensions)
            }
          })

          # Update measure choices when data or dimensions change
          # Exclude selected dimensions from measure choices
          # Always include "Count" as an option
          shiny::observe({
            info <- column_info()
            dims <- r_dimensions()

            # Available measures = numeric columns minus selected dimensions
            numeric_measures <- setdiff(info$measures, dims)

            # Always add Count as first option
            available_measures <- c("Count" = ".count", stats::setNames(numeric_measures, numeric_measures))

            current_meas <- r_measure()

            # Only set defaults if measure is NULL (first load without constructor value)
            if (is.null(current_meas)) {
              current_meas <- if (length(numeric_measures) > 0) numeric_measures[1] else ".count"
              r_measure(current_meas)
            } else if (!(current_meas %in% available_measures)) {
              # Constructor provided measure that doesn't exist - fall back to defaults
              current_meas <- if (length(numeric_measures) > 0) numeric_measures[1] else ".count"
              r_measure(current_meas)
            }

            shiny::updateSelectInput(
              session, "measure",
              choices = available_measures,
              selected = current_meas
            )
          })

          # Update state from UI
          shiny::observeEvent(input$dimensions, {
            if (!identical(r_dimensions(), input$dimensions)) {
              r_dimensions(input$dimensions)
            }
          }, ignoreInit = TRUE)

          shiny::observeEvent(input$measure, {
            if (!identical(r_measure(), input$measure)) {
              r_measure(input$measure)
            }
          }, ignoreInit = TRUE)

          # Active dimensions (from user selection)
          active_dimensions <- shiny::reactive({
            r_dimensions()
          })

          # Active range dimensions (excluding the current measure)
          active_range_dimensions <- shiny::reactive({
            range_dims <- r_range_dimensions()
            meas <- r_measure()
            if (is.null(range_dims)) return(character())
            setdiff(range_dims, meas)
          })

          # Active measure (reactive wrapper)
          active_measure <- shiny::reactive({
            r_measure()
          })

          # State: active filters per dimension (initialized from parameter)
          r_filters <- shiny::reactiveVal(filters)

          # State: active range filters (initialized from parameter)
          r_range_filters <- shiny::reactiveVal(range_filters)

          # Clear all filters
          shiny::observeEvent(input$clear_filters, {
            r_filters(list())
            r_range_filters(list())
          })

          # Apply all filters (categorical + range) to data
          apply_filters <- function(df, cat_filters, rng_filters) {
            # Categorical filters
            for (dim in names(cat_filters)) {
              val <- cat_filters[[dim]]
              if (!is.null(val) && length(val) > 0 && dim %in% names(df)) {
                df <- dplyr::filter(df, .data[[dim]] %in% val)
              }
            }
            # Range filters
            for (dim in names(rng_filters)) {
              rng <- rng_filters[[dim]]
              if (!is.null(rng) && length(rng) == 2 && dim %in% names(df)) {
                df <- dplyr::filter(df, dplyr::between(.data[[dim]], rng[1], rng[2]))
              }
            }
            df
          }

          # Filtered data based on active filters (for output/downstream)
          filtered_data <- shiny::reactive({
            df <- data()
            shiny::req(is.data.frame(df))
            apply_filters(df, r_filters(), r_range_filters())
          })

          # Crossfilter: data filtered by OTHER dimensions (excluding one)
          # This allows each table/slider to show its values while respecting other filters
          crossfilter_data <- function(exclude_dim) {
            df <- data()
            shiny::req(is.data.frame(df))

            cat_filters <- r_filters()
            rng_filters <- r_range_filters()

            # Apply all categorical filters except excluded dim
            for (dim in names(cat_filters)) {
              if (dim == exclude_dim) next
              val <- cat_filters[[dim]]
              if (!is.null(val) && length(val) > 0 && dim %in% names(df)) {
                df <- dplyr::filter(df, .data[[dim]] %in% val)
              }
            }
            # Apply all range filters except excluded dim
            for (dim in names(rng_filters)) {
              if (dim == exclude_dim) next
              rng <- rng_filters[[dim]]
              if (!is.null(rng) && length(rng) == 2 && dim %in% names(df)) {
                df <- dplyr::filter(df, dplyr::between(.data[[dim]], rng[1], rng[2]))
              }
            }
            df
          }

          # Handle row clicks from any table
          shiny::observeEvent(input$table_click, {
            click <- input$table_click
            if (is.null(click)) return()

            dim <- click$dim
            value <- click$value

            if (!is.null(value) && value != "") {
              current <- r_filters()
              current_vals <- current[[dim]]

              # Toggle: if already in selection, remove; else add
              if (value %in% current_vals) {
                current_vals <- setdiff(current_vals, value)
                current[[dim]] <- if (length(current_vals) == 0) NULL else current_vals
              } else {
                current[[dim]] <- c(current_vals, value)
              }
              r_filters(current)
            }
          }, ignoreInit = TRUE)

          # Handle range slider changes
          shiny::observeEvent(input$range_change, {
            change <- input$range_change
            if (is.null(change)) return()

            dim <- change$dim
            val <- as.numeric(change$value)

            if (!is.null(dim) && length(val) == 2) {
              current <- r_range_filters()
              # Get the full data range for this dim to detect "reset to full"
              df <- data()
              if (dim %in% names(df)) {
                col_vals <- df[[dim]]
                col_vals <- col_vals[!is.na(col_vals)]
                data_min <- min(col_vals)
                data_max <- max(col_vals)
                # If slider is at full range, remove the filter
                if (val[1] <= data_min && val[2] >= data_max) {
                  current[[dim]] <- NULL
                } else {
                  current[[dim]] <- val
                }
              } else {
                current[[dim]] <- val
              }
              r_range_filters(current)
            }
          }, ignoreInit = TRUE)

          # Helper to format numbers compactly (supports negative values)
          format_number <- function(x) {
            sign_str <- ifelse(x < 0, "-", "")
            abs_x <- abs(x)
            ifelse(
              abs_x >= 1e6, paste0(sign_str, round(abs_x / 1e6, 1), "M"),
              ifelse(abs_x >= 1e3, paste0(sign_str, round(abs_x / 1e3, 0), "K"),
                     paste0(sign_str, format(round(abs_x), big.mark = ",")))
            )
          }

          # Build a single filter table for a dimension
          build_filter_table <- function(dim, meas) {
            # Crossfilter: show all values of this dim, filtered by OTHER dims
            df <- crossfilter_data(dim)
            shiny::req(nrow(df) > 0)

            # Aggregate by dimension
            if (meas == ".count") {
              agg <- dplyr::summarise(
                df,
                .count = dplyr::n(),
                .by = dplyr::all_of(dim)
              )
              value_col <- ".count"
            } else {
              agg <- dplyr::summarise(
                df,
                !!meas := sum(.data[[meas]], na.rm = TRUE),
                .by = dplyr::all_of(dim)
              )
              value_col <- meas
            }

            # Convert dimension to character
            agg <- dplyr::mutate(agg, !!dim := as.character(.data[[dim]]))

            # Sort by value descending
            agg <- dplyr::arrange(agg, dplyr::desc(.data[[value_col]]))

            # Get current filter for highlighting
            current_filter <- r_filters()[[dim]]
            has_filter <- !is.null(current_filter) && length(current_filter) > 0

            # Add selection indicator
            agg$.selected <- if (has_filter) {
              agg[[dim]] %in% current_filter
            } else {
              TRUE  # All selected when no filter
            }

            # Calculate max absolute value for bar scaling (supports negative values)
            max_abs_val <- max(abs(agg[[value_col]]), na.rm = TRUE)
            if (is.na(max_abs_val) || max_abs_val == 0) max_abs_val <- 1
            min_val <- min(agg[[value_col]], na.rm = TRUE)
            has_negative <- min_val < 0

            # Build columns list with dynamic names
            columns_list <- list()
            columns_list[[".selected"]] <- reactable::colDef(show = FALSE)

            # Label column (dimension)
            columns_list[[dim]] <- reactable::colDef(
              name = dim,
              minWidth = 120,
              cell = function(value, index) {
                is_selected <- agg$.selected[index]
                style <- if (has_filter && !is_selected) {
                  "color: #999;"
                } else {
                  "font-weight: 500;"
                }
                shiny::tags$span(style = style, value)
              }
            )

            # Value column with inline bar (diverging for negative values)
            columns_list[[value_col]] <- reactable::colDef(
              name = if (meas == ".count") "Count" else meas,
              minWidth = 140,
              align = "right",
              cell = function(value, index) {
                is_selected <- agg$.selected[index]
                pct <- abs(value) / max_abs_val * 100
                is_negative <- value < 0

                # Colors: blue for positive, red for negative
                if (is_negative) {
                  bar_color <- if (has_filter && !is_selected) {
                    "rgba(198, 84, 84, 0.2)"
                  } else {
                    "#c65454"
                  }
                } else {
                  bar_color <- if (has_filter && !is_selected) {
                    "rgba(84, 112, 198, 0.2)"
                  } else {
                    "#5470c6"
                  }
                }
                text_color <- if (has_filter && !is_selected) {
                  "#999"
                } else if (is_negative) {
                  "#c65454"
                } else {
                  "#333"
                }

                if (has_negative) {
                  # Diverging bar: left half for negative, right half for positive
                  shiny::div(
                    style = "display: flex; align-items: center; justify-content: flex-end; gap: 6px;",
                    # Diverging bar container
                    shiny::div(
                      style = "flex: 1; max-width: 100px; height: 14px; display: flex; position: relative;",
                      # Left half (negative)
                      shiny::div(
                        style = "width: 50%; height: 100%; background: #f0f0f0; border-radius: 2px 0 0 2px; display: flex; justify-content: flex-end; overflow: hidden;",
                        if (is_negative) {
                          shiny::div(
                            style = sprintf(
                              "height: 100%%; width: %.1f%%; background: %s;",
                              pct, bar_color
                            )
                          )
                        }
                      ),
                      # Center line
                      shiny::div(
                        style = "width: 1px; height: 100%; background: #ccc;"
                      ),
                      # Right half (positive)
                      shiny::div(
                        style = "width: 50%; height: 100%; background: #f0f0f0; border-radius: 0 2px 2px 0; overflow: hidden;",
                        if (!is_negative) {
                          shiny::div(
                            style = sprintf(
                              "height: 100%%; width: %.1f%%; background: %s;",
                              pct, bar_color
                            )
                          )
                        }
                      )
                    ),
                    shiny::span(
                      style = sprintf("color: %s; font-size: 12px; width: 45px; text-align: right;", text_color),
                      format_number(value)
                    )
                  )
                } else {
                  # Standard bar (all positive)
                  shiny::div(
                    style = "display: flex; align-items: center; justify-content: flex-end; gap: 6px;",
                    shiny::div(
                      style = "flex: 1; max-width: 80px; height: 14px; background: #f0f0f0; border-radius: 2px; overflow: hidden;",
                      shiny::div(
                        style = sprintf(
                          "height: 100%%; width: %.1f%%; background: %s;",
                          pct, bar_color
                        )
                      )
                    ),
                    shiny::span(
                      style = sprintf("color: %s; font-size: 12px; width: 38px; text-align: right;", text_color),
                      format_number(value)
                    )
                  )
                }
              }
            )

            # Create reactable (no checkboxes - just click rows to filter)
            reactable::reactable(
              agg,
              columns = columns_list,
              onClick = htmlwidgets::JS(sprintf(
                "function(rowInfo, column) {
                  Shiny.setInputValue('%s', {dim: '%s', value: rowInfo.row['%s']}, {priority: 'event'});
                }",
                ns("table_click"), dim, dim
              )),
              searchable = TRUE,
              compact = TRUE,
              borderless = TRUE,
              highlight = TRUE,
              height = 200,
              pagination = FALSE,
              theme = reactable::reactableTheme(
                searchInputStyle = list(fontSize = "12px", padding = "4px 8px"),
                headerStyle = list(fontSize = "12px", fontWeight = "600")
              )
            )
          }

          # Build a range slider for a numeric dimension
          build_range_slider <- function(dim) {
            # Full data range — slider always shows the complete range
            # (exclude-own-dimension: the slider sees through its own filter)
            full_df <- data()
            shiny::req(is.data.frame(full_df))
            full_vals <- full_df[[dim]]
            full_vals <- full_vals[!is.na(full_vals)]
            shiny::req(length(full_vals) > 0)

            full_min <- min(full_vals)
            full_max <- max(full_vals)

            # Crossfiltered data — for the row count display
            cf_df <- crossfilter_data(dim)
            shiny::req(nrow(cf_df) > 0)
            cf_vals <- cf_df[[dim]]
            cf_vals <- cf_vals[!is.na(cf_vals)]

            # Crossfiltered data range (for visual indicator)
            cf_min <- min(cf_vals)
            cf_max <- max(cf_vals)

            # Position of crossfiltered range within full range (as percentage)
            range_span <- full_max - full_min
            if (range_span > 0) {
              left_pct <- (cf_min - full_min) / range_span * 100
              width_pct <- (cf_max - cf_min) / range_span * 100
            } else {
              left_pct <- 0
              width_pct <- 100
            }

            # Get current range filter or use full range
            current_range <- r_range_filters()[[dim]]
            slider_min <- if (!is.null(current_range)) current_range[1] else full_min
            slider_max <- if (!is.null(current_range)) current_range[2] else full_max

            # Determine step: integer columns get step=1, otherwise auto
            is_integer <- all(full_vals == floor(full_vals))
            step <- if (is_integer) 1 else NULL

            # Count crossfiltered rows matching current range
            if (!is.null(current_range)) {
              n_match <- sum(cf_vals >= current_range[1] & cf_vals <= current_range[2])
            } else {
              n_match <- length(cf_vals)
            }

            slider_id <- ns(paste0("range_", dim))

            # Status text: row count + crossfiltered range
            range_text <- if (cf_min == full_min && cf_max == full_max) {
              paste0(n_match, " of ", length(cf_vals), " rows")
            } else {
              paste0(n_match, " of ", length(cf_vals), " rows | data: ", cf_min, "\u2013", cf_max)
            }

            shiny::div(
              style = "flex: 1; min-width: 200px; max-width: 350px;",
              shiny::tags$div(
                style = "font-weight: 600; font-size: 14px; margin-bottom: 4px; color: #333;",
                dim
              ),
              shiny::tags$div(
                style = "font-size: 11px; color: #888; margin-bottom: 2px;",
                range_text
              ),
              # Range indicator bar: grey track with blue segment for crossfiltered range
              shiny::tags$div(
                style = "height: 6px; background: #e8e8e8; border-radius: 3px; margin-bottom: 4px; position: relative; overflow: hidden;",
                shiny::tags$div(
                  style = sprintf(
                    "position: absolute; left: %.1f%%; width: %.1f%%; height: 100%%; background: #5470c6; border-radius: 3px; min-width: 3px;",
                    left_pct, max(width_pct, 0.5)
                  )
                )
              ),
              shiny::sliderInput(
                inputId = slider_id,
                label = NULL,
                min = full_min,
                max = full_max,
                value = c(slider_min, slider_max),
                step = step,
                width = "100%"
              ),
              # JS to send range changes back via a unified input
              shiny::tags$script(shiny::HTML(sprintf(
                "
                $(document).on('shiny:inputchanged', function(event) {
                  if (event.name === '%s') {
                    Shiny.setInputValue('%s', {dim: '%s', value: event.value}, {priority: 'event'});
                  }
                });
                ",
                slider_id, ns("range_change"), dim
              )))
            )
          }

          # Render the tables grid UI (includes both tables and range sliders)
          output$tables_grid <- shiny::renderUI({
            dims <- active_dimensions()
            range_dims <- active_range_dimensions()
            meas <- active_measure()

            if (length(dims) == 0 && length(range_dims) == 0) {
              return(shiny::div(
                style = "padding: 20px; text-align: center; color: #666;",
                "No dimension columns found in data"
              ))
            }

            shiny::req(meas)

            parts <- list()

            # Range sliders row (above dimension tables)
            if (length(range_dims) > 0) {
              parts <- c(parts, list(
                shiny::div(
                  class = "range-sliders-row",
                  style = "display: flex; flex-wrap: wrap; gap: 16px; margin-bottom: 12px;",
                  lapply(range_dims, build_range_slider)
                )
              ))
            }

            # Dimension tables row
            if (length(dims) > 0) {
              parts <- c(parts, list(
                shiny::div(
                  class = "tables-row",
                  style = "display: flex; flex-wrap: wrap; gap: 16px;",
                  lapply(dims, function(dim) {
                    shiny::div(
                      style = "flex: 1; min-width: 280px; max-width: 450px;",
                      shiny::tags$div(
                        style = "font-weight: 600; font-size: 14px; margin-bottom: 8px; color: #333;",
                        dim
                      ),
                      build_filter_table(dim, meas)
                    )
                  })
                )
              ))
            }

            shiny::tagList(parts)
          })

          # Active filters display with conditional Clear button
          output$filter_status <- shiny::renderUI({
            cat_filters <- r_filters()
            rng_filters <- r_range_filters()
            nrows <- nrow(filtered_data())
            total <- nrow(data())

            has_filters <- length(cat_filters) > 0 || length(rng_filters) > 0

            if (has_filters) {
              parts <- character()

              # Categorical filter text
              if (length(cat_filters) > 0) {
                cat_text <- paste(
                  names(cat_filters),
                  "=",
                  vapply(cat_filters, function(x) paste(x, collapse = ", "), character(1)),
                  collapse = " | "
                )
                parts <- c(parts, cat_text)
              }

              # Range filter text
              if (length(rng_filters) > 0) {
                rng_text <- paste(
                  names(rng_filters),
                  vapply(rng_filters, function(x) paste0("[", x[1], ", ", x[2], "]"), character(1)),
                  collapse = " | "
                )
                parts <- c(parts, rng_text)
              }

              filter_text <- paste(parts, collapse = " | ")
              status_text <- paste0(filter_text, " (", format(nrows, big.mark = ","), " / ", format(total, big.mark = ","), " rows)")
            } else {
              status_text <- paste0("No filters active - click on rows to filter (", format(total, big.mark = ","), " rows)")
            }

            shiny::div(
              style = "display: flex; align-items: center; gap: 10px; margin: 12px 0 8px 0;",
              shiny::span(
                class = "text-muted",
                style = "font-size: 0.8rem;",
                status_text
              ),
              if (has_filters) {
                shiny::actionButton(
                  ns("clear_filters"),
                  "Remove Filter",
                  class = "btn btn-outline-secondary btn-sm",
                  style = "font-size: 0.7rem; padding: 1px 6px; opacity: 0.6;"
                )
              }
            )
          })

          # Build filter expression that returns filtered data
          list(
            expr = shiny::reactive({
              cat_filters <- r_filters()
              rng_filters <- r_range_filters()

              if (length(cat_filters) == 0 && length(rng_filters) == 0) {
                return(quote(identity(data)))
              }

              conditions <- list()

              # Build categorical filter conditions (supports multi-select)
              for (dim in names(cat_filters)) {
                val <- cat_filters[[dim]]
                if (length(val) == 1) {
                  conditions <- c(conditions, list(call("==", as.name(dim), val)))
                } else {
                  conditions <- c(conditions, list(call("%in%", as.name(dim), val)))
                }
              }

              # Build range filter conditions
              for (dim in names(rng_filters)) {
                rng <- rng_filters[[dim]]
                if (!is.null(rng) && length(rng) == 2) {
                  lo <- call(">=", as.name(dim), rng[1])
                  hi <- call("<=", as.name(dim), rng[2])
                  conditions <- c(conditions, list(call("&", lo, hi)))
                }
              }

              if (length(conditions) == 0) {
                return(quote(identity(data)))
              }

              # Combine conditions with &
              if (length(conditions) == 1) {
                combined <- conditions[[1]]
              } else {
                combined <- Reduce(function(a, b) call("&", a, b), conditions)
              }

              # Return dplyr::filter expression
              as.call(list(quote(dplyr::filter), quote(data), combined))
            }),
            state = list(
              dimensions = r_dimensions,
              range_dimensions = r_range_dimensions,
              measure = r_measure,
              filters = r_filters,
              range_filters = r_range_filters
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)

      shiny::tagList(
        # CSS for advanced toggle
        shiny::tags$style(shiny::HTML(sprintf(
          "
          #%s {
            max-height: 0;
            overflow: hidden;
            transition: max-height 0.3s ease-out;
          }
          #%s.expanded {
            max-height: 500px;
            overflow: visible;
            transition: max-height 0.5s ease-in;
          }
          .block-advanced-toggle {
            cursor: pointer;
            user-select: none;
            padding: 8px 0;
            margin-bottom: 0;
            display: flex;
            align-items: center;
            gap: 6px;
            font-size: 0.8125rem;
          }
          .block-chevron {
            transition: transform 0.2s;
            display: inline-block;
            font-size: 14px;
            font-weight: bold;
          }
          .block-chevron.rotated {
            transform: rotate(90deg);
          }
          ",
          ns("advanced-options"),
          ns("advanced-options")
        ))),

        shiny::div(
          class = "table-filter-container",
          style = "padding: 10px;",

          # Tables grid (always visible) - includes both tables and range sliders
          shiny::uiOutput(ns("tables_grid")),

          # Filter status with conditional clear button
          shiny::uiOutput(ns("filter_status")),

          # Advanced options toggle
          shiny::div(
            class = "block-advanced-toggle text-muted",
            id = ns("advanced-toggle"),
            onclick = sprintf(
              "
              const section = document.getElementById('%s');
              const chevron = document.querySelector('#%s .block-chevron');
              section.classList.toggle('expanded');
              chevron.classList.toggle('rotated');
              ",
              ns("advanced-options"),
              ns("advanced-toggle")
            ),
            shiny::tags$span(class = "block-chevron", "\u203A"),
            "Show advanced options"
          ),

          # Advanced options section (collapsed by default)
          shiny::div(
            id = ns("advanced-options"),
            style = "padding-top: 10px;",
            shiny::div(
              style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
              shiny::div(
                style = "display: flex; align-items: center; gap: 5px;",
                shiny::tags$label("Dimensions:", style = "margin: 0; font-size: 12px;"),
                shiny::selectizeInput(
                  ns("dimensions"),
                  label = NULL,
                  choices = NULL,
                  multiple = TRUE,
                  width = "250px",
                  options = list(plugins = list("remove_button"))
                )
              ),
              shiny::div(
                style = "display: flex; align-items: center; gap: 5px;",
                shiny::tags$label("Measure:", style = "margin: 0; font-size: 12px;"),
                shiny::selectInput(
                  ns("measure"),
                  label = NULL,
                  choices = NULL,
                  width = "150px"
                )
              )
            )
          )
        )
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) {
        stop("Input must be a data frame")
      }
    },
    allow_empty_state = c("dimensions", "range_dimensions", "measure", "filters", "range_filters"),
    class = "crossfilter_block",
    ...
  )
}
