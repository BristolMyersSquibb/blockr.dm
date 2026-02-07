#' dm Crossfilter Block
#'
#' A crossfilter block that accepts a dm object, shows per-table filter panels,
#' and propagates filters across tables using the dm's key relationships.
#' For example, filtering AESEV=SEVERE in ADAE reduces the subject set visible
#' in ADLB and ADSL panels.
#'
#' @param active_dims Named list of per-table active filter columns. Each element
#'   is a character vector of column names to show as filter widgets.
#'   E.g., `list(adsl_data = c("SEX", "AGE"), adlb_data = c("PARAMCD"))`
#'   Start with an empty list (default) to show no filters initially.
#' @param filters Named list of per-table categorical filters. Each element is
#'   itself a named list of character vectors.
#'   E.g., `list(adsl = list(SEX = c("F")), adae = list(AESEV = c("SEVERE")))`
#' @param range_filters Named list of per-table range filters. Each element is
#'   a named list of numeric(2) vectors.
#'   E.g., `list(adsl = list(AGE = c(65, 80)))`
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A blockr transform block that returns a filtered dm object
#'
#' @export
new_dm_crossfilter_block <- function(
    active_dims = list(),
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

          # --- dm info: extract tables, PKs, FKs ---
          dm_info <- shiny::reactive({
            dm_obj <- data()
            shiny::req(inherits(dm_obj, "dm"))

            table_names <- names(dm::dm_get_tables(dm_obj))
            tables <- stats::setNames(
              lapply(table_names, function(tbl) dm_obj[[tbl]]),
              table_names
            )
            pks <- dm::dm_get_all_pks(dm_obj)
            fks <- dm::dm_get_all_fks(dm_obj)

            list(
              table_names = table_names,
              tables = tables,
              pks = pks,
              fks = fks
            )
          })

          # --- Find the key column linking a table to the rest ---
          # Returns the column name used for cross-table joins (e.g. "USUBJID")
          find_key_column <- function(tbl_name) {
            info <- dm_info()
            pks <- info$pks
            fks <- info$fks

            # Check if this table has a PK
            pk_row <- pks[pks$table == tbl_name, ]
            if (nrow(pk_row) > 0) {
              return(pk_row$pk_col[[1]][[1]])
            }

            # Check if this table has an FK to another table
            fk_row <- fks[fks$child_table == tbl_name, ]
            if (nrow(fk_row) > 0) {
              return(fk_row$child_fk_cols[[1]][[1]])
            }

            NULL
          }

          # --- Column classification per table ---
          column_info_per_table <- shiny::reactive({
            info <- dm_info()
            result <- list()

            for (tbl_name in info$table_names) {
              df <- info$tables[[tbl_name]]
              if (!is.data.frame(df) || ncol(df) == 0) next

              is_numeric <- vapply(df, is.numeric, logical(1))
              is_date <- vapply(df, function(col) {
                inherits(col, c("Date", "POSIXct", "POSIXlt"))
              }, logical(1))
              is_low_cardinality <- vapply(df, function(col) {
                length(unique(col)) <= 10
              }, logical(1))

              is_dimension <- (!is_numeric & !is_date) | (is_numeric & is_low_cardinality)
              is_range_dim <- is_numeric & !is_low_cardinality
              is_date_dim <- is_date

              # Exclude key columns from dimensions
              key_col <- find_key_column(tbl_name)
              if (!is.null(key_col)) {
                is_dimension[key_col] <- FALSE
                is_range_dim[key_col] <- FALSE
                is_date_dim[key_col] <- FALSE
              }

              result[[tbl_name]] <- list(
                dimensions = names(df)[is_dimension],
                range_dimensions = names(df)[is_range_dim],
                date_dimensions = names(df)[is_date_dim],
                measures = names(df)[is_numeric & !is_low_cardinality]
              )
            }
            result
          })

          # --- State: per-table active dims + filters ---
          r_active_dims <- shiny::reactiveVal(active_dims)
          r_filters <- shiny::reactiveVal(filters)
          r_range_filters <- shiny::reactiveVal(range_filters)

          # Clear all filters
          shiny::observeEvent(input$clear_filters, {
            r_active_dims(list())
            r_filters(list())
            r_range_filters(list())
          })

          # --- Apply categorical + range filters to a single data frame ---
          apply_filters <- function(df, cat_filters, rng_filters) {
            for (dim in names(cat_filters)) {
              val <- cat_filters[[dim]]
              if (!is.null(val) && length(val) > 0 && dim %in% names(df)) {
                df <- dplyr::filter(df, .data[[dim]] %in% val)
              }
            }
            for (dim in names(rng_filters)) {
              rng <- rng_filters[[dim]]
              if (!is.null(rng) && length(rng) == 2 && dim %in% names(df)) {
                col <- df[[dim]]
                if (inherits(col, c("Date", "POSIXct", "POSIXlt"))) {
                  rng_lo <- as.Date(rng[1], origin = "1970-01-01")
                  rng_hi <- as.Date(rng[2], origin = "1970-01-01")
                  df <- dplyr::filter(df, .data[[dim]] >= rng_lo & .data[[dim]] <= rng_hi)
                } else {
                  df <- dplyr::filter(df, dplyr::between(.data[[dim]], rng[1], rng[2]))
                }
              }
            }
            df
          }

          # --- Compute key set for a table after applying its local filters ---
          # If exclude_dim is provided, skip that dimension's filter
          compute_key_set <- function(tbl_name, exclude_dim = NULL) {
            info <- dm_info()
            df <- info$tables[[tbl_name]]
            key_col <- find_key_column(tbl_name)
            if (is.null(key_col)) return(NULL)

            cat_f <- r_filters()[[tbl_name]] %||% list()
            rng_f <- r_range_filters()[[tbl_name]] %||% list()

            # Remove excluded dimension
            if (!is.null(exclude_dim)) {
              cat_f[[exclude_dim]] <- NULL
              rng_f[[exclude_dim]] <- NULL
            }

            df <- apply_filters(df, cat_f, rng_f)
            unique(df[[key_col]])
          }

          # --- Cross-table crossfilter data for a specific dimension ---
          # Returns the data for tbl_name filtered by:
          #   - All OTHER tables' key sets (full filters)
          #   - This table's filters EXCEPT exclude_dim
          crossfilter_data_for_dim <- function(tbl_name, exclude_dim) {
            info <- dm_info()
            df <- info$tables[[tbl_name]]
            key_col <- find_key_column(tbl_name)

            # Get this table's key set without exclude_dim
            own_keys <- compute_key_set(tbl_name, exclude_dim = exclude_dim)

            # Intersect with all other tables' key sets (full filters)
            for (other_tbl in info$table_names) {
              if (other_tbl == tbl_name) next
              other_key_col <- find_key_column(other_tbl)
              if (is.null(other_key_col)) next
              other_keys <- compute_key_set(other_tbl)
              if (!is.null(other_keys) && !is.null(own_keys)) {
                own_keys <- intersect(own_keys, other_keys)
              }
            }

            # Filter to allowed keys
            if (!is.null(key_col) && !is.null(own_keys)) {
              df <- dplyr::filter(df, .data[[key_col]] %in% own_keys)
            }

            # Apply this table's local filters except exclude_dim
            cat_f <- r_filters()[[tbl_name]] %||% list()
            rng_f <- r_range_filters()[[tbl_name]] %||% list()
            cat_f[[exclude_dim]] <- NULL
            rng_f[[exclude_dim]] <- NULL

            apply_filters(df, cat_f, rng_f)
          }

          # --- Fully filtered data for expression output ---
          # Applies all filters from all tables via key intersection
          filtered_row_count <- shiny::reactive({
            info <- dm_info()
            total <- 0
            filtered <- 0
            for (tbl_name in info$table_names) {
              df <- info$tables[[tbl_name]]
              total <- total + nrow(df)

              key_col <- find_key_column(tbl_name)
              cat_f <- r_filters()[[tbl_name]] %||% list()
              rng_f <- r_range_filters()[[tbl_name]] %||% list()

              result_df <- apply_filters(df, cat_f, rng_f)

              # Cross-table key filtering
              if (!is.null(key_col)) {
                own_keys <- unique(result_df[[key_col]])
                for (other_tbl in info$table_names) {
                  if (other_tbl == tbl_name) next
                  other_keys <- compute_key_set(other_tbl)
                  if (!is.null(other_keys)) {
                    own_keys <- intersect(own_keys, other_keys)
                  }
                }
                result_df <- dplyr::filter(result_df, .data[[key_col]] %in% own_keys)
              }

              filtered <- filtered + nrow(result_df)
            }
            list(total = total, filtered = filtered)
          })

          # --- Handle row clicks ---
          shiny::observeEvent(input$table_click, {
            click <- input$table_click
            if (is.null(click)) return()

            tbl <- click$table
            dim <- click$dim
            value <- click$value

            if (!is.null(value) && value != "") {
              current <- r_filters()
              tbl_filters <- current[[tbl]] %||% list()
              current_vals <- tbl_filters[[dim]]

              if (value %in% current_vals) {
                current_vals <- setdiff(current_vals, value)
                tbl_filters[[dim]] <- if (length(current_vals) == 0) NULL else current_vals
              } else {
                tbl_filters[[dim]] <- c(current_vals, value)
              }

              # Clean up empty table entries
              if (length(tbl_filters) == 0) {
                current[[tbl]] <- NULL
              } else {
                current[[tbl]] <- tbl_filters
              }
              r_filters(current)
            }
          }, ignoreInit = TRUE)

          # --- Handle range slider changes ---
          shiny::observeEvent(input$range_change, {
            change <- input$range_change
            if (is.null(change)) return()

            tbl <- change$table
            dim <- change$dim
            val <- as.numeric(change$value)

            if (!is.null(dim) && length(val) == 2) {
              current <- r_range_filters()
              tbl_filters <- current[[tbl]] %||% list()

              # Get full data range to detect reset
              info <- dm_info()
              df <- info$tables[[tbl]]
              if (dim %in% names(df)) {
                col_vals <- df[[dim]]
                col_vals <- col_vals[!is.na(col_vals)]
                data_min <- min(col_vals)
                data_max <- max(col_vals)
                if (val[1] <= data_min && val[2] >= data_max) {
                  tbl_filters[[dim]] <- NULL
                } else {
                  tbl_filters[[dim]] <- val
                }
              } else {
                tbl_filters[[dim]] <- val
              }

              if (length(tbl_filters) == 0) {
                current[[tbl]] <- NULL
              } else {
                current[[tbl]] <- tbl_filters
              }
              r_range_filters(current)
            }
          }, ignoreInit = TRUE)

          # --- Handle date slider changes ---
          shiny::observeEvent(input$date_change, {
            change <- input$date_change
            if (is.null(change)) return()

            tbl <- change$table
            dim <- change$dim
            val <- as.Date(change$value)

            if (!is.null(dim) && length(val) == 2) {
              current <- r_range_filters()
              tbl_filters <- current[[tbl]] %||% list()

              # Get full data range to detect reset
              info <- dm_info()
              df <- info$tables[[tbl]]
              if (dim %in% names(df)) {
                col_vals <- as.Date(df[[dim]])
                col_vals <- col_vals[!is.na(col_vals)]
                data_min <- min(col_vals)
                data_max <- max(col_vals)
                if (val[1] <= data_min && val[2] >= data_max) {
                  tbl_filters[[dim]] <- NULL
                } else {
                  tbl_filters[[dim]] <- as.numeric(val)
                }
              } else {
                tbl_filters[[dim]] <- as.numeric(val)
              }

              if (length(tbl_filters) == 0) {
                current[[tbl]] <- NULL
              } else {
                current[[tbl]] <- tbl_filters
              }
              r_range_filters(current)
            }
          }, ignoreInit = TRUE)

          # --- Format numbers compactly ---
          format_number <- function(x) {
            sign_str <- ifelse(x < 0, "-", "")
            abs_x <- abs(x)
            ifelse(
              abs_x >= 1e6, paste0(sign_str, round(abs_x / 1e6, 1), "M"),
              ifelse(abs_x >= 1e3, paste0(sign_str, round(abs_x / 1e3, 0), "K"),
                     paste0(sign_str, format(round(abs_x), big.mark = ",")))
            )
          }

          # --- Build filter table for a dimension in a specific table ---
          build_filter_table <- function(tbl_name, dim) {
            df <- crossfilter_data_for_dim(tbl_name, dim)

            if (nrow(df) == 0) {
              # Show empty table with dim column and zero counts
              info <- dm_info()
              full_df <- info$tables[[tbl_name]]
              all_vals <- unique(as.character(full_df[[dim]]))
              agg <- data.frame(
                x = all_vals,
                .count = rep(0L, length(all_vals)),
                .selected = rep(FALSE, length(all_vals)),
                stringsAsFactors = FALSE
              )
              names(agg)[1] <- dim
            } else {
              # Always aggregate by count for dm crossfilter
              agg <- dplyr::summarise(
                df,
                .count = dplyr::n(),
                .by = dplyr::all_of(dim)
              )
              agg <- dplyr::mutate(agg, !!dim := as.character(.data[[dim]]))
              agg <- dplyr::arrange(agg, dplyr::desc(.data[[".count"]]))

              current_filter <- r_filters()[[tbl_name]][[dim]]
              has_filter <- !is.null(current_filter) && length(current_filter) > 0
              agg$.selected <- if (has_filter) {
                agg[[dim]] %in% current_filter
              } else {
                TRUE
              }
            }

            value_col <- ".count"

            current_filter <- r_filters()[[tbl_name]][[dim]]
            has_filter <- !is.null(current_filter) && length(current_filter) > 0

            max_abs_val <- max(abs(agg[[value_col]]), na.rm = TRUE)
            if (is.na(max_abs_val) || max_abs_val == 0) max_abs_val <- 1

            columns_list <- list()
            columns_list[[".selected"]] <- reactable::colDef(show = FALSE)

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

            columns_list[[value_col]] <- reactable::colDef(
              name = "Count",
              minWidth = 120,
              align = "right",
              cell = function(value, index) {
                is_selected <- agg$.selected[index]
                pct <- abs(value) / max_abs_val * 100
                bar_color <- if (has_filter && !is_selected) {
                  "rgba(84, 112, 198, 0.2)"
                } else {
                  "#5470c6"
                }
                text_color <- if (has_filter && !is_selected) "#999" else "#333"

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
            )

            reactable::reactable(
              agg,
              columns = columns_list,
              onClick = htmlwidgets::JS(sprintf(
                "function(rowInfo, column) {
                  Shiny.setInputValue('%s', {table: '%s', dim: '%s', value: rowInfo.row['%s']}, {priority: 'event'});
                }",
                ns("table_click"), tbl_name, dim, dim
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

          # --- Build range slider for a numeric dimension in a specific table ---
          build_range_slider <- function(tbl_name, dim) {
            info <- dm_info()
            full_df <- info$tables[[tbl_name]]
            shiny::req(is.data.frame(full_df))
            full_vals <- full_df[[dim]]
            full_vals <- full_vals[!is.na(full_vals)]
            shiny::req(length(full_vals) > 0)

            full_min <- min(full_vals)
            full_max <- max(full_vals)

            cf_df <- crossfilter_data_for_dim(tbl_name, dim)
            cf_vals <- cf_df[[dim]]
            cf_vals <- cf_vals[!is.na(cf_vals)]

            cf_min <- if (length(cf_vals) > 0) min(cf_vals) else full_min
            cf_max <- if (length(cf_vals) > 0) max(cf_vals) else full_max

            range_span <- full_max - full_min
            if (range_span > 0) {
              left_pct <- (cf_min - full_min) / range_span * 100
              width_pct <- (cf_max - cf_min) / range_span * 100
            } else {
              left_pct <- 0
              width_pct <- 100
            }

            current_range <- r_range_filters()[[tbl_name]][[dim]]
            slider_min <- if (!is.null(current_range)) current_range[1] else full_min
            slider_max <- if (!is.null(current_range)) current_range[2] else full_max

            is_integer <- all(full_vals == floor(full_vals))
            step <- if (is_integer) 1 else NULL

            if (!is.null(current_range)) {
              n_match <- sum(cf_vals >= current_range[1] & cf_vals <= current_range[2])
            } else {
              n_match <- length(cf_vals)
            }

            slider_id <- ns(paste0("range_", tbl_name, "_", dim))

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
              shiny::tags$script(shiny::HTML(sprintf(
                "
                $(document).on('shiny:inputchanged', function(event) {
                  if (event.name === '%s') {
                    Shiny.setInputValue('%s', {table: '%s', dim: '%s', value: event.value}, {priority: 'event'});
                  }
                });
                ",
                slider_id, ns("range_change"), tbl_name, dim
              )))
            )
          }

          # --- Build date slider for a date dimension in a specific table ---
          build_date_slider <- function(tbl_name, dim) {
            info <- dm_info()
            full_df <- info$tables[[tbl_name]]
            shiny::req(is.data.frame(full_df))
            full_vals <- full_df[[dim]]
            full_vals <- full_vals[!is.na(full_vals)]
            shiny::req(length(full_vals) > 0)

            full_vals <- as.Date(full_vals)
            full_min <- min(full_vals)
            full_max <- max(full_vals)

            cf_df <- crossfilter_data_for_dim(tbl_name, dim)
            cf_vals <- as.Date(cf_df[[dim]])
            cf_vals <- cf_vals[!is.na(cf_vals)]

            cf_min <- if (length(cf_vals) > 0) min(cf_vals) else full_min
            cf_max <- if (length(cf_vals) > 0) max(cf_vals) else full_max

            range_span <- as.numeric(full_max - full_min)
            if (range_span > 0) {
              left_pct <- as.numeric(cf_min - full_min) / range_span * 100
              width_pct <- as.numeric(cf_max - cf_min) / range_span * 100
            } else {
              left_pct <- 0
              width_pct <- 100
            }

            current_range <- r_range_filters()[[tbl_name]][[dim]]
            slider_min <- if (!is.null(current_range)) as.Date(current_range[1], origin = "1970-01-01") else full_min
            slider_max <- if (!is.null(current_range)) as.Date(current_range[2], origin = "1970-01-01") else full_max

            if (!is.null(current_range)) {
              cr_min <- as.Date(current_range[1], origin = "1970-01-01")
              cr_max <- as.Date(current_range[2], origin = "1970-01-01")
              n_match <- sum(cf_vals >= cr_min & cf_vals <= cr_max)
            } else {
              n_match <- length(cf_vals)
            }

            slider_id <- ns(paste0("date_", tbl_name, "_", dim))

            range_text <- paste0(n_match, " of ", length(cf_vals), " rows")

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
                width = "100%"
              ),
              shiny::tags$script(shiny::HTML(sprintf(
                "
                $(document).on('shiny:inputchanged', function(event) {
                  if (event.name === '%s') {
                    Shiny.setInputValue('%s', {table: '%s', dim: '%s', value: event.value}, {priority: 'event'});
                  }
                });
                ",
                slider_id, ns("date_change"), tbl_name, dim
              )))
            )
          }

          # --- Helper: get filter type for a column in a table ---
          get_dim_type <- function(tbl_name, dim_name) {
            col_info <- column_info_per_table()
            tbl_info <- col_info[[tbl_name]]
            if (is.null(tbl_info)) return(NULL)
            if (dim_name %in% tbl_info$date_dimensions) return("date")
            if (dim_name %in% tbl_info$range_dimensions) return("range")
            if (dim_name %in% tbl_info$dimensions) return("categorical")
            NULL
          }

          # --- Observer: "Add filter" selectize input ---
          shiny::observeEvent(input$add_filter, {
            req <- input$add_filter
            if (is.null(req)) return()
            tbl <- req$table
            dim_name <- req$dim
            if (is.null(tbl) || is.null(dim_name) || dim_name == "") return()

            current <- r_active_dims()
            tbl_dims <- current[[tbl]] %||% character()
            if (!dim_name %in% tbl_dims) {
              current[[tbl]] <- c(tbl_dims, dim_name)
              r_active_dims(current)
            }
          }, ignoreInit = TRUE)

          # --- Observer: remove filter (× button) ---
          shiny::observeEvent(input$remove_filter, {
            req <- input$remove_filter
            if (is.null(req)) return()
            tbl <- req$table
            dim_name <- req$dim
            if (is.null(tbl) || is.null(dim_name)) return()

            # Remove from active dims
            current <- r_active_dims()
            tbl_dims <- current[[tbl]] %||% character()
            tbl_dims <- setdiff(tbl_dims, dim_name)
            if (length(tbl_dims) == 0) {
              current[[tbl]] <- NULL
            } else {
              current[[tbl]] <- tbl_dims
            }
            r_active_dims(current)

            # Clear filter values for removed dim
            cat_f <- r_filters()
            if (!is.null(cat_f[[tbl]][[dim_name]])) {
              cat_f[[tbl]][[dim_name]] <- NULL
              if (length(cat_f[[tbl]]) == 0) cat_f[[tbl]] <- NULL
              r_filters(cat_f)
            }
            rng_f <- r_range_filters()
            if (!is.null(rng_f[[tbl]][[dim_name]])) {
              rng_f[[tbl]][[dim_name]] <- NULL
              if (length(rng_f[[tbl]]) == 0) rng_f[[tbl]] <- NULL
              r_range_filters(rng_f)
            }
          }, ignoreInit = TRUE)

          # --- Render per-table UI ---
          output$tables_grid <- shiny::renderUI({
            col_info <- column_info_per_table()
            info <- dm_info()
            active <- r_active_dims()

            if (length(info$table_names) == 0) {
              return(shiny::div(
                style = "padding: 20px; text-align: center; color: #666;",
                "No tables found in dm object"
              ))
            }

            table_panels <- lapply(info$table_names, function(tbl_name) {
              tbl_info <- col_info[[tbl_name]]
              if (is.null(tbl_info)) return(NULL)

              all_filterable <- c(
                tbl_info$dimensions,
                tbl_info$range_dimensions,
                tbl_info$date_dimensions
              )
              if (length(all_filterable) == 0) return(NULL)

              active_cols <- active[[tbl_name]] %||% character()
              # Only show columns that are still valid
              active_cols <- intersect(active_cols, all_filterable)
              available_cols <- setdiff(all_filterable, active_cols)

              # Build "Add filter" selectize
              add_filter_id <- ns(paste0("add_filter_", tbl_name))
              choices <- stats::setNames(available_cols, available_cols)

              add_filter_ui <- shiny::div(
                style = "display: inline-block; min-width: 180px; vertical-align: middle;",
                shiny::selectizeInput(
                  inputId = add_filter_id,
                  label = NULL,
                  choices = c("Add filter..." = "", choices),
                  selected = "",
                  width = "180px",
                  options = list(
                    placeholder = "Add filter...",
                    onInitialize = I('function() { this.setValue(""); }')
                  )
                ),
                shiny::tags$script(shiny::HTML(sprintf(
                  "
                  $(document).on('change', '#%s', function() {
                    var val = $(this).val();
                    if (val && val !== '') {
                      Shiny.setInputValue('%s', {table: '%s', dim: val}, {priority: 'event'});
                      var selectize = $(this)[0].selectize;
                      if (selectize) { selectize.setValue('', true); }
                    }
                  });
                  ",
                  add_filter_id, ns("add_filter"), tbl_name
                )))
              )

              # Build filter widgets for active columns
              parts <- list()
              range_cols <- character()
              date_cols <- character()
              cat_cols <- character()

              for (col in active_cols) {
                dtype <- get_dim_type(tbl_name, col)
                if (identical(dtype, "range")) {
                  range_cols <- c(range_cols, col)
                } else if (identical(dtype, "date")) {
                  date_cols <- c(date_cols, col)
                } else {
                  cat_cols <- c(cat_cols, col)
                }
              }

              # Build widget with × button wrapper
              wrap_with_remove <- function(tbl_name, dim_name, widget) {
                shiny::div(
                  style = "position: relative;",
                  shiny::tags$button(
                    type = "button",
                    style = "position: absolute; top: 0; right: 0; z-index: 10; background: none; border: none; cursor: pointer; font-size: 16px; color: #999; padding: 2px 6px; line-height: 1;",
                    title = paste0("Remove ", dim_name, " filter"),
                    onclick = sprintf(
                      "Shiny.setInputValue('%s', {table: '%s', dim: '%s'}, {priority: 'event'});",
                      ns("remove_filter"), tbl_name, dim_name
                    ),
                    shiny::HTML("&times;")
                  ),
                  widget
                )
              }

              # Range sliders
              if (length(range_cols) > 0) {
                parts <- c(parts, list(
                  shiny::div(
                    style = "display: flex; flex-wrap: wrap; gap: 16px; margin-bottom: 12px;",
                    lapply(range_cols, function(dim) {
                      wrap_with_remove(tbl_name, dim, build_range_slider(tbl_name, dim))
                    })
                  )
                ))
              }

              # Date sliders
              if (length(date_cols) > 0) {
                parts <- c(parts, list(
                  shiny::div(
                    style = "display: flex; flex-wrap: wrap; gap: 16px; margin-bottom: 12px;",
                    lapply(date_cols, function(dim) {
                      wrap_with_remove(tbl_name, dim, build_date_slider(tbl_name, dim))
                    })
                  )
                ))
              }

              # Dimension tables (categorical)
              if (length(cat_cols) > 0) {
                parts <- c(parts, list(
                  shiny::div(
                    style = "display: flex; flex-wrap: wrap; gap: 16px;",
                    lapply(cat_cols, function(dim) {
                      shiny::div(
                        style = "flex: 1; min-width: 250px; max-width: 400px;",
                        wrap_with_remove(tbl_name, dim, build_filter_table(tbl_name, dim))
                      )
                    })
                  )
                ))
              }

              # No filters message
              if (length(active_cols) == 0) {
                parts <- list(
                  shiny::div(
                    style = "padding: 8px 0; color: #999; font-size: 13px; font-style: italic;",
                    "No filters active"
                  )
                )
              }

              # Table section with header + add filter dropdown
              shiny::div(
                style = "margin-bottom: 20px;",
                shiny::tags$div(
                  style = "display: flex; align-items: center; gap: 12px; border-bottom: 2px solid #ddd; padding-bottom: 4px; margin-bottom: 10px;",
                  shiny::tags$span(
                    style = "font-weight: 700; font-size: 15px; color: #444;",
                    tbl_name
                  ),
                  add_filter_ui
                ),
                shiny::tagList(parts)
              )
            })

            # Remove NULLs
            table_panels <- Filter(Negate(is.null), table_panels)

            if (length(table_panels) == 0) {
              return(shiny::div(
                style = "padding: 20px; text-align: center; color: #666;",
                "No filterable columns found in dm tables"
              ))
            }

            shiny::tagList(table_panels)
          })

          # --- Filter status display ---
          output$filter_status <- shiny::renderUI({
            cat_filters <- r_filters()
            rng_filters <- r_range_filters()
            counts <- filtered_row_count()

            has_filters <- length(cat_filters) > 0 || length(rng_filters) > 0

            if (has_filters) {
              parts <- character()

              for (tbl in names(cat_filters)) {
                tbl_f <- cat_filters[[tbl]]
                for (dim in names(tbl_f)) {
                  parts <- c(parts, paste0(
                    tbl, ".", dim, "=",
                    paste(tbl_f[[dim]], collapse = ",")
                  ))
                }
              }

              for (tbl in names(rng_filters)) {
                tbl_f <- rng_filters[[tbl]]
                for (dim in names(tbl_f)) {
                  rng <- tbl_f[[dim]]
                  # Format date ranges as dates, not raw numerics
                  info <- dm_info()
                  tbl_df <- info$tables[[tbl]]
                  is_date_col <- dim %in% names(tbl_df) &&
                    inherits(tbl_df[[dim]], c("Date", "POSIXct", "POSIXlt"))
                  if (is_date_col) {
                    lo_str <- as.character(as.Date(rng[1], origin = "1970-01-01"))
                    hi_str <- as.character(as.Date(rng[2], origin = "1970-01-01"))
                  } else {
                    lo_str <- rng[1]
                    hi_str <- rng[2]
                  }
                  parts <- c(parts, paste0(
                    tbl, ".", dim, " [", lo_str, ", ", hi_str, "]"
                  ))
                }
              }

              filter_text <- paste(parts, collapse = " | ")
              status_text <- paste0(
                filter_text,
                " (", format(counts$filtered, big.mark = ","),
                " / ", format(counts$total, big.mark = ","), " rows)"
              )
            } else {
              status_text <- paste0(
                "No filters active - click on rows to filter (",
                format(counts$total, big.mark = ","), " rows)"
              )
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

          # --- Expression: dm::dm_filter() ---
          list(
            expr = shiny::reactive({
              cat_filters <- r_filters()
              rng_filters <- r_range_filters()

              if (length(cat_filters) == 0 && length(rng_filters) == 0) {
                return(quote(identity(data)))
              }

              # Build per-table filter conditions
              table_conditions <- list()
              all_tables <- unique(c(names(cat_filters), names(rng_filters)))

              for (tbl in all_tables) {
                conditions <- list()

                # Categorical filters for this table
                tbl_cat <- cat_filters[[tbl]]
                if (!is.null(tbl_cat)) {
                  for (dim in names(tbl_cat)) {
                    val <- tbl_cat[[dim]]
                    if (length(val) == 1) {
                      conditions <- c(conditions, list(call("==", as.name(dim), val)))
                    } else {
                      conditions <- c(conditions, list(call("%in%", as.name(dim), val)))
                    }
                  }
                }

                # Range filters for this table
                tbl_rng <- rng_filters[[tbl]]
                if (!is.null(tbl_rng)) {
                  # Detect which dims are date columns
                  info <- dm_info()
                  tbl_df <- info$tables[[tbl]]
                  for (dim in names(tbl_rng)) {
                    rng <- tbl_rng[[dim]]
                    if (!is.null(rng) && length(rng) == 2) {
                      is_date_col <- dim %in% names(tbl_df) &&
                        inherits(tbl_df[[dim]], c("Date", "POSIXct", "POSIXlt"))
                      if (is_date_col) {
                        lo_val <- call("as.Date", as.character(as.Date(rng[1], origin = "1970-01-01")))
                        hi_val <- call("as.Date", as.character(as.Date(rng[2], origin = "1970-01-01")))
                      } else {
                        lo_val <- rng[1]
                        hi_val <- rng[2]
                      }
                      lo <- call(">=", as.name(dim), lo_val)
                      hi <- call("<=", as.name(dim), hi_val)
                      conditions <- c(conditions, list(call("&", lo, hi)))
                    }
                  }
                }

                if (length(conditions) > 0) {
                  if (length(conditions) == 1) {
                    combined <- conditions[[1]]
                  } else {
                    combined <- Reduce(function(a, b) call("&", a, b), conditions)
                  }
                  table_conditions[[tbl]] <- combined
                }
              }

              if (length(table_conditions) == 0) {
                return(quote(identity(data)))
              }

              # Build dm::dm_filter(data, tbl1 = cond1, tbl2 = cond2, ...)
              call <- call("dm_filter", quote(data))
              for (tbl in names(table_conditions)) {
                call[[tbl]] <- table_conditions[[tbl]]
              }
              call[[1]] <- quote(dm::dm_filter)
              call
            }),
            state = list(
              active_dims = r_active_dims,
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
        shiny::div(
          class = "dm-crossfilter-container",
          style = "padding: 10px;",
          shiny::uiOutput(ns("tables_grid")),
          shiny::uiOutput(ns("filter_status"))
        )
      )
    },
    dat_valid = function(data) {
      if (!inherits(data, "dm")) {
        stop("Input must be a dm object")
      }
    },
    allow_empty_state = c("active_dims", "filters", "range_filters"),
    class = "dm_crossfilter_block",
    ...
  )
}

#' @rdname block_output.dm_block
#' @method block_output dm_crossfilter_block
#' @export
block_output.dm_crossfilter_block <- function(x, result, session) {
  block_output.dm_block(x, result, session)
}

#' @rdname block_ui.dm_block
#' @method block_ui dm_crossfilter_block
#' @export
block_ui.dm_crossfilter_block <- function(id, x, ...) {
  block_ui.dm_block(id, x, ...)
}

#' @method block_render_trigger dm_crossfilter_block
#' @export
block_render_trigger.dm_crossfilter_block <- function(x, session = blockr.core::get_session()) {
  NULL
}
