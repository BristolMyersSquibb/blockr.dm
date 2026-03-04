# --- CSS for dm crossfilter block ---
dm_crossfilter_search_css <- function() {
  shiny::tags$style(shiny::HTML("
    .dm-cf-search-wrapper {
      position: relative;
      margin-top: 10px;
      margin-bottom: 12px;
    }
    .dm-cf-search-icon {
      position: absolute;
      left: 12px;
      top: 50%;
      transform: translateY(-50%);
      color: var(--blockr-grey-400, #9ca3af);
      pointer-events: none;
      display: flex;
      align-items: center;
    }
    .dm-cf-search-input {
      width: 100%;
      padding: 10px 12px 10px 40px;
      font-size: 14px;
      background: var(--blockr-grey-50, #f9fafb);
      border: 1px solid var(--blockr-color-border, #e5e7eb);
      border-radius: 8px;
      color: var(--blockr-color-text-primary, #111827);
      transition: border-color 0.15s ease, box-shadow 0.15s ease;
      box-sizing: border-box;
    }
    .dm-cf-search-input:focus {
      outline: none;
      border-color: var(--blockr-blue-500, #3b82f6);
      box-shadow: 0 0 0 3px var(--blockr-blue-100, #dbeafe);
    }
    .dm-cf-search-results {
      margin-bottom: 12px;
      border: 1px solid var(--blockr-color-border, #e5e7eb);
      border-radius: 8px;
      overflow: hidden;
      max-height: 400px;
      overflow-y: auto;
    }
    .dm-cf-search-group-header {
      padding: 8px 14px;
      font-size: 13px;
      font-weight: var(--blockr-font-weight-semibold, 600);
      text-transform: uppercase;
      letter-spacing: 0.05em;
      color: var(--blockr-blue-600, #2563eb);
      background: var(--blockr-blue-50, #eff6ff);
      border-bottom: 2px solid var(--blockr-blue-400, #60a5fa);
    }
    .dm-cf-search-item {
      display: flex;
      align-items: center;
      gap: 10px;
      padding: 10px 14px;
      font-size: 14px;
      cursor: pointer;
      border-bottom: 1px solid var(--blockr-color-border, #e5e7eb);
      transition: background-color 0.1s ease;
    }
    .dm-cf-search-item:last-child {
      border-bottom: none;
    }
    .dm-cf-search-item:hover {
      background: var(--blockr-color-bg-hover, #f3f4f6);
    }
    .dm-cf-search-item-icon {
      color: var(--blockr-grey-400, #9ca3af);
      flex-shrink: 0;
      width: 20px;
      text-align: center;
      font-size: 16px;
    }
    .dm-cf-search-item-name {
      flex: 1;
      font-weight: 500;
    }
    .dm-cf-search-item-badge {
      font-size: 12px;
      padding: 2px 10px;
      border-radius: 9999px;
      font-weight: 500;
      flex-shrink: 0;
    }
    .dm-cf-badge-categorical {
      color: #7c3aed;
      background: #f3e8ff;
    }
    .dm-cf-badge-numeric {
      color: #059669;
      background: #ecfdf5;
    }
    .dm-cf-badge-date {
      color: #d97706;
      background: #fffbeb;
    }
    .dm-cf-search-empty {
      padding: 16px;
      text-align: center;
      color: var(--blockr-color-text-muted, #6b7280);
      font-style: italic;
    }
    .dm-crossfilter-container .recalculating {
      --_shiny-fade-opacity: 1;
      opacity: 1 !important;
    }
    .dm-crossfilter-container .recalculating > * {
      opacity: 1 !important;
    }
    .dm-cf-table-section {
      margin-bottom: 20px;
    }
    .dm-cf-table-header {
      display: flex;
      align-items: center;
      gap: 8px;
      padding: 8px 14px;
      font-size: 13px;
      font-weight: var(--blockr-font-weight-semibold, 600);
      text-transform: uppercase;
      letter-spacing: 0.05em;
      color: var(--blockr-blue-600, #2563eb);
      background: var(--blockr-blue-50, #eff6ff);
      border-bottom: 2px solid var(--blockr-blue-400, #60a5fa);
      margin-bottom: 10px;
    }
    .dm-cf-table-header-count {
      font-size: 11px;
      font-weight: 400;
      color: var(--blockr-color-text-muted, #6b7280);
    }
    .dm-cf-filter-card {
      position: relative;
    }
    .dm-cf-filter-card-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      margin-bottom: 4px;
    }
    .dm-cf-filter-card-label {
      font-weight: 600;
      font-size: 14px;
      color: var(--blockr-color-text-primary, #111827);
    }
    .dm-cf-filter-card-actions {
      display: flex;
      gap: 2px;
      align-items: center;
    }
    .dm-cf-reset-btn {
      display: flex;
      align-items: center;
      justify-content: center;
      width: 28px;
      height: 28px;
      padding: 0;
      background: transparent;
      border: none;
      border-radius: 6px;
      color: var(--blockr-grey-400, #9ca3af);
      cursor: pointer;
      transition: background-color 0.15s, color 0.15s;
    }
    .dm-cf-reset-btn:hover {
      background: var(--blockr-grey-100, #f3f4f6);
      color: var(--blockr-grey-700, #374151);
    }
    .dm-cf-remove-btn {
      display: flex;
      align-items: center;
      justify-content: center;
      width: 28px;
      height: 28px;
      padding: 0;
      background: transparent;
      border: none;
      border-radius: 6px;
      color: var(--blockr-grey-400, #9ca3af);
      cursor: pointer;
      transition: background-color 0.15s, color 0.15s;
    }
    .dm-cf-remove-btn:hover {
      background: var(--blockr-grey-100, #f3f4f6);
      color: var(--blockr-grey-700, #374151);
    }
    /* Status bar */
    .dm-cf-status-bar {
      display: flex;
      align-items: center;
      gap: 8px;
      margin: 10px 0 8px 0;
      flex-wrap: wrap;
      min-height: 26px;
    }
    .dm-cf-filter-chip {
      display: inline-flex;
      align-items: center;
      gap: 2px;
      padding: 2px 8px;
      font-size: 11px;
      font-weight: 500;
      border-radius: 4px;
      background: var(--blockr-blue-50, #eff6ff);
      color: var(--blockr-color-text-primary, #1f2937);
      white-space: nowrap;
      line-height: 1.4;
    }
    .dm-cf-chip-table {
      color: var(--blockr-color-text-muted, #6b7280);
      font-weight: 400;
    }
    .dm-cf-row-count {
      font-size: 11px;
      color: var(--blockr-color-text-muted, #6b7280);
      font-variant-numeric: tabular-nums;
      white-space: nowrap;
    }
    .dm-cf-timing {
      font-size: 10px;
      color: #b0b7c3;
      font-variant-numeric: tabular-nums;
    }
    .dm-cf-clear-btn,
    .dm-cf-reset-all-btn {
      display: inline-flex;
      align-items: center;
      gap: 4px;
      font-size: 11px;
      color: var(--blockr-color-text-muted, #6b7280);
      background: none;
      border: 1px solid var(--blockr-color-border, #e5e7eb);
      border-radius: 4px;
      padding: 1px 8px;
      cursor: pointer;
      transition: all 0.15s;
      line-height: 1.4;
    }
    .dm-cf-clear-btn:hover,
    .dm-cf-reset-all-btn:hover {
      color: var(--blockr-color-text-primary, #374151);
      border-color: #d1d5db;
      background: var(--blockr-grey-50, #f9fafb);
    }
    .dm-cf-separator {
      width: 1px;
      height: 14px;
      background: var(--blockr-color-border, #e5e7eb);
      flex-shrink: 0;
    }
    /* Advanced options toggle */
    .dm-cf-advanced-toggle {
      cursor: pointer;
      user-select: none;
      padding: 4px 0;
      margin-bottom: 0;
      display: flex;
      align-items: center;
      gap: 6px;
      font-size: 0.8125rem;
      color: var(--blockr-color-text-muted, #6b7280);
    }
    .dm-cf-advanced-toggle:hover {
      color: var(--blockr-color-text-secondary, #374151);
    }
    .dm-cf-advanced-section {
      max-height: 0;
      overflow: hidden;
      transition: max-height 0.3s ease-out;
    }
    .dm-cf-advanced-section.expanded {
      max-height: 500px;
      overflow: visible;
      transition: max-height 0.5s ease-in;
    }
    .dm-cf-advanced-grid {
      display: grid;
      gap: 15px;
      grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
      padding: 8px 0;
    }
    .dm-cf-chevron {
      transition: transform 0.2s;
      display: inline-block;
      font-size: 14px;
      font-weight: bold;
    }
    .dm-cf-chevron.rotated {
      transform: rotate(90deg);
    }
  "))
}

dm_crossfilter_table_css <- function() {
  shiny::tags$style(shiny::HTML("
    .dm-cf-tw-search {
      width: 100%;
      padding: 4px 8px;
      font-size: 12px;
      border: 1px solid #e5e7eb;
      border-radius: 4px;
      margin-bottom: 4px;
      box-sizing: border-box;
      outline: none;
    }
    .dm-cf-tw-search:focus {
      border-color: #3b82f6;
      box-shadow: 0 0 0 2px rgba(59, 130, 246, 0.15);
    }
    .dm-cf-tw-scroll {
      max-height: 200px;
      overflow-y: auto;
      border: 1px solid #e5e7eb;
      border-radius: 4px;
    }
    .dm-cf-tw-table {
      width: 100%;
      border-collapse: collapse;
      font-size: 13px;
      table-layout: fixed;
    }
    .dm-cf-tw-th {
      position: sticky;
      top: 0;
      background: #fff;
      font-size: 12px;
      font-weight: 600;
      padding: 4px 8px;
      text-align: left;
      cursor: pointer;
      user-select: none;
      border-bottom: 1px solid #e5e7eb;
      white-space: nowrap;
      z-index: 1;
    }
    .dm-cf-tw-th:hover {
      background: #f9fafb;
    }
    .dm-cf-tw-row {
      cursor: pointer;
    }
    .dm-cf-tw-row:hover {
      background: #f3f4f6;
    }
    .dm-cf-tw-row td {
      padding: 3px 8px;
      border-bottom: 1px solid #f3f4f6;
    }
    .dm-cf-tw-bar-cell {
      display: flex;
      align-items: center;
      justify-content: flex-end;
      gap: 6px;
    }
    .dm-cf-tw-bar-track {
      flex: 1;
      max-width: 80px;
      height: 14px;
      background: #f0f0f0;
      border-radius: 2px;
      overflow: hidden;
      position: relative;
    }
    .dm-cf-tw-bar-fill {
      height: 100%;
      border-radius: 2px;
    }
    .dm-cf-tw-bar-label {
      font-size: 12px;
      width: 38px;
      text-align: right;
      flex-shrink: 0;
    }
    .dm-cf-tw-bar-diverging {
      position: relative;
      height: 100%;
    }
    .dm-cf-tw-bar-center {
      position: absolute;
      left: 50%;
      top: 0;
      bottom: 0;
      width: 1px;
      background: #ccc;
    }
    .dm-cf-tw-bar-label-wide {
      width: 48px;
    }
  "))
}

dm_crossfilter_server_factory <- function(active_dims, filters, range_filters, measure,
                                           agg_func = NULL) {
  function(id, data) {
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

            # Sort tables: parents before children (topological order).
            # Count outgoing FKs per table — pure parents have 0.
            n_fks <- vapply(table_names, function(tbl) {
              sum(fks$child_table == tbl)
            }, integer(1))
            table_names <- table_names[order(n_fks)]

            list(
              table_names = table_names,
              tables = tables,
              pks = pks,
              fks = fks
            )
          })

          # --- Parse "table.column" measure spec by matching known table names ---
          parse_measure_spec <- function(measure) {
            info <- dm_info()
            for (tbl in info$table_names) {
              prefix <- paste0(tbl, ".")
              if (startsWith(measure, prefix)) {
                return(list(
                  table = tbl,
                  col = substr(measure, nchar(prefix) + 1, nchar(measure))
                ))
              }
            }
            NULL
          }

          # --- Backend detection + reactive selector ---
          has_duckdb <- requireNamespace("duckdb", quietly = TRUE) &&
            requireNamespace("DBI", quietly = TRUE)
          has_duckplyr <- requireNamespace("duckplyr", quietly = TRUE)

          available_backends <- c(
            "lookup",
            "dplyr",
            if (has_duckdb) "duckdb",
            if (has_duckplyr) "duckplyr",
            "dm"
          )
          default_backend <- "lookup"
          r_backend <- shiny::reactiveVal(default_backend)
          r_last_timing <- shiny::reactiveVal(NULL)

          shiny::observeEvent(input$backend_switch, {
            val <- input$backend_switch
            if (!is.null(val) && val %in% available_backends) {
              r_backend(val)
            }
          }, ignoreInit = TRUE)

          shiny::observeEvent(input$measure_switch, {
            val <- input$measure_switch
            if (!is.null(val)) {
              r_measure(val)
            }
          }, ignoreInit = TRUE)

          shiny::observeEvent(input$agg_func_switch, {
            val <- input$agg_func_switch
            if (!is.null(val)) r_agg_func(val)
          }, ignoreInit = TRUE)

          # --- DuckDB connection (when available) ---
          duck_con <- if (has_duckdb) {
            DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
          } else {
            NULL
          }

          if (has_duckdb) {
            shiny::observe({
              info <- dm_info()
              for (nm in info$table_names) {
                duckdb::duckdb_register(duck_con, nm, info$tables[[nm]],
                                         overwrite = TRUE)
              }
            })
            session$onSessionEnded(function() {
              if (!is.null(duck_con)) {
                try(DBI::dbDisconnect(duck_con, shutdown = TRUE), silent = TRUE)
              }
            })
          }

          # --- Duckplyr tables (when available) ---
          r_duck_tables <- shiny::reactiveVal(NULL)
          r_col_classes <- shiny::reactiveVal(NULL)

          if (has_duckplyr) {
            shiny::observe({
              info <- dm_info()
              col_classes <- build_col_classes(info$tables)
              duck_tables <- lapply(
                stats::setNames(info$table_names, info$table_names),
                function(nm) {
                  df <- info$tables[[nm]]
                  # Strip non-essential column attributes (duckplyr rejects them)
                  for (cn in names(df)) {
                    # Convert factors to character (duckplyr doesn't support factors)
                    if (is.factor(df[[cn]])) {
                      df[[cn]] <- as.character(df[[cn]])
                    }
                    a <- attributes(df[[cn]])
                    keep <- intersect(names(a), c("class", "levels", "names", "dim", "tzone"))
                    attributes(df[[cn]]) <- a[keep]
                  }
                  duckplyr::as_duckdb_tibble(df)
                }
              )
              r_duck_tables(duck_tables)
              r_col_classes(col_classes)
            })
          }

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

              # Exclude all key columns (PK + all FK cols) from dimensions
              key_col <- find_key_column(tbl_name)
              fk_rows <- info$fks[info$fks$child_table == tbl_name, ]
              all_fk_cols <- if (nrow(fk_rows) > 0) {
                unique(unlist(fk_rows$child_fk_cols))
              } else {
                character()
              }
              exclude_cols <- unique(c(key_col, all_fk_cols))
              for (ec in exclude_cols) {
                if (ec %in% names(is_dimension)) {
                  is_dimension[ec] <- FALSE
                  is_range_dim[ec] <- FALSE
                  is_date_dim[ec] <- FALSE
                }
              }

              tbl_label <- attr(df, "label")
              if (is.null(tbl_label)) tbl_label <- ""

              col_labels <- vapply(names(df), function(cn) {
                lbl <- attr(df[[cn]], "label")
                if (is.null(lbl)) "" else lbl
              }, character(1))

              result[[tbl_name]] <- list(
                dimensions = names(df)[is_dimension],
                range_dimensions = names(df)[is_range_dim],
                date_dimensions = names(df)[is_date_dim],
                measures = names(df)[is_numeric & !is_low_cardinality],
                labels = col_labels,
                table_label = tbl_label
              )
            }
            result
          })

          # --- State: per-table active dims + filters ---
          # as_rv: if x is already a reactiveVal (injected by external_ctrl),
          # reuse it; otherwise create a fresh one.
          as_rv <- function(x, default = x) {
            if (inherits(x, "reactiveVal")) x else shiny::reactiveVal(default)
          }
          r_active_dims <- as_rv(active_dims)
          r_filters <- as_rv(filters)
          r_range_filters <- as_rv(range_filters)
          r_measure <- as_rv(measure, measure %||% ".count")
          r_agg_func <- as_rv(agg_func, agg_func %||% "sum")

          # --- Precomputed lookup tables (invalidates on data/dims/measure, NOT on filter values) ---
          r_lookup_info <- shiny::reactive({
            info <- dm_info()
            active <- r_active_dims()
            measure <- r_measure()
            build_crossfilter_lookups(
              info$tables, active, info$pks, info$fks, measure
            )
          })

          # Clear all filters (remove all filters entirely)
          shiny::observeEvent(input$clear_filters, {
            r_active_dims(list())
            r_filters(list())
            r_range_filters(list())
          })

          # Reset all filters (clear values but keep filters visible)
          shiny::observeEvent(input$reset_all_filters, {
            r_filters(list())
            r_range_filters(list())
          })

          # --- Cross-table crossfilter data for a specific dimension ---
          # Dispatches to selected backend, falls back to dplyr on error
          crossfilter_data_for_dim <- function(tbl_name, exclude_dim) {
            # Use lookup backend (avoids semi-joins entirely)
            if (r_backend() == "lookup") {
              lookup_info <- r_lookup_info()
              if (!is.null(lookup_info)) {
                result <- tryCatch(
                  lookup_crossfilter_data(lookup_info, tbl_name, exclude_dim,
                                           r_filters(), r_range_filters()),
                  error = function(e) NULL
                )
                if (!is.null(result)) return(result)
              }
            }
            info <- dm_info()
            cf <- r_filters()
            rf <- r_range_filters()
            backend <- r_backend()
            if (backend == "duckdb") {
              result <- tryCatch(
                duckdb_crossfilter_data(
                  duck_con, info$tables, info$table_names, find_key_column,
                  tbl_name, exclude_dim, cf, rf
                ),
                error = function(e) NULL
              )
              if (!is.null(result)) return(result)
            } else if (backend == "duckplyr") {
              dt <- r_duck_tables()
              cc <- r_col_classes()
              if (!is.null(dt)) {
                result <- tryCatch(
                  duckplyr_crossfilter_data(
                    dt, cc, find_key_column,
                    tbl_name, exclude_dim, cf, rf
                  ),
                  error = function(e) NULL
                )
                if (!is.null(result)) return(result)
              }
            } else if (backend == "dm") {
              result <- tryCatch(
                dm_crossfilter_data(data(), tbl_name, exclude_dim, cf, rf),
                error = function(e) NULL
              )
              if (!is.null(result)) return(result)
            }
            dplyr_crossfilter_data(
              info$tables, find_key_column, tbl_name, exclude_dim, cf, rf
            )
          }

          # --- Aggregated counts for a categorical dimension ---
          crossfilter_agg_for_dim <- function(tbl_name, dim) {
            # Use lookup backend (avoids semi-joins entirely)
            if (r_backend() == "lookup") {
              lookup_info <- r_lookup_info()
              if (!is.null(lookup_info)) {
                result <- tryCatch(
                  lookup_crossfilter_agg(lookup_info, tbl_name, dim,
                                          r_filters(), r_range_filters()),
                  error = function(e) NULL
                )
                if (!is.null(result)) return(result)
              }
            }
            info <- dm_info()
            cf <- r_filters()
            rf <- r_range_filters()
            backend <- r_backend()
            if (backend == "duckdb") {
              result <- tryCatch(
                duckdb_crossfilter_agg(
                  duck_con, info$tables, info$table_names, find_key_column,
                  tbl_name, dim, cf, rf
                ),
                error = function(e) NULL
              )
              if (!is.null(result)) return(result)
            } else if (backend == "duckplyr") {
              dt <- r_duck_tables()
              cc <- r_col_classes()
              if (!is.null(dt)) {
                result <- tryCatch(
                  duckplyr_crossfilter_agg(
                    dt, cc, find_key_column,
                    tbl_name, dim, cf, rf
                  ),
                  error = function(e) NULL
                )
                if (!is.null(result)) return(result)
              }
            } else if (backend == "dm") {
              result <- tryCatch(
                dm_crossfilter_agg(data(), tbl_name, dim, cf, rf),
                error = function(e) NULL
              )
              if (!is.null(result)) return(result)
            }
            dplyr_crossfilter_agg(
              info$tables, find_key_column, tbl_name, dim, cf, rf
            )
          }

          # --- Aggregated measure for a categorical dimension ---
          # Uses crossfilter_data_for_dim() for the cross-table filtering
          # then does a lightweight SUM aggregation on top
          crossfilter_agg_for_dim_measure <- function(tbl_name, dim,
                                                       measure_table,
                                                       measure_col) {
            cf_data <- crossfilter_data_for_dim(tbl_name, dim)

            agg_fn <- switch(r_agg_func(),
              mean = function(x) mean(x, na.rm = TRUE),
              median = function(x) stats::median(x, na.rm = TRUE),
              min = function(x) min(x, na.rm = TRUE),
              max = function(x) max(x, na.rm = TRUE),
              function(x) sum(x, na.rm = TRUE)
            )

            if (measure_table == tbl_name) {
              # Same table: aggregate with selected function
              if (!measure_col %in% names(cf_data)) {
                return(data.frame(
                  x = character(0), .value = numeric(0),
                  stringsAsFactors = FALSE
                ))
              }
              agg <- dplyr::summarise(
                cf_data,
                .value = agg_fn(.data[[measure_col]]),
                .by = dplyr::all_of(dim)
              )
            } else {
              # Cross-table: join dim→key mapping to measure table, then aggregate
              key_col <- find_key_column(tbl_name)
              measure_key_col <- find_key_column(measure_table)

              if (is.null(key_col) || is.null(measure_key_col)) {
                # No key relationship — fall back to count
                return(NULL)
              }

              # Build dim→key mapping from crossfiltered data
              keep_cols <- intersect(c(key_col, dim), names(cf_data))
              dim_keys <- unique(cf_data[, keep_cols, drop = FALSE])
              allowed_keys <- unique(cf_data[[key_col]])

              # Get measure table data, apply its own filters, restrict to allowed keys
              info <- dm_info()
              measure_df <- info$tables[[measure_table]]
              measure_filtered <- apply_crossfilter_filters(
                measure_df,
                r_filters()[[measure_table]] %||% list(),
                r_range_filters()[[measure_table]] %||% list()
              )
              measure_filtered <- dplyr::filter(
                measure_filtered,
                .data[[measure_key_col]] %in% allowed_keys
              )

              # Join and aggregate
              by_spec <- stats::setNames(key_col, measure_key_col)
              joined <- dplyr::inner_join(
                measure_filtered[, c(measure_key_col, measure_col), drop = FALSE],
                dim_keys,
                by = by_spec
              )
              agg <- dplyr::summarise(
                joined,
                .value = agg_fn(.data[[measure_col]]),
                .by = dplyr::all_of(dim)
              )
            }

            agg[[dim]] <- as.character(agg[[dim]])
            dplyr::arrange(agg, dplyr::desc(.data[[".value"]]))
          }

          # --- Fully filtered row counts ---
          filtered_row_count <- shiny::reactive({
            t0 <- proc.time()[["elapsed"]]
            on.exit(r_last_timing(round((proc.time()[["elapsed"]] - t0) * 1000)))
            # Use lookup backend (avoids semi-joins entirely)
            if (r_backend() == "lookup") {
              lookup_info <- r_lookup_info()
              if (!is.null(lookup_info)) {
                info <- dm_info()
                result <- tryCatch(
                  lookup_crossfilter_counts(lookup_info, info$tables,
                                             info$table_names,
                                             r_filters(), r_range_filters()),
                  error = function(e) NULL
                )
                if (!is.null(result)) return(result)
              }
            }
            info <- dm_info()
            cf <- r_filters()
            rf <- r_range_filters()
            backend <- r_backend()

            result <- NULL
            if (backend == "duckdb") {
              result <- tryCatch(
                duckdb_crossfilter_counts(
                  duck_con, info$tables, info$table_names, find_key_column,
                  cf, rf
                ),
                error = function(e) NULL
              )
            } else if (backend == "duckplyr") {
              dt <- r_duck_tables()
              cc <- r_col_classes()
              if (!is.null(dt)) {
                result <- tryCatch(
                  duckplyr_crossfilter_counts(
                    dt, cc, find_key_column, info$table_names, cf, rf
                  ),
                  error = function(e) NULL
                )
              }
            } else if (backend == "dm") {
              result <- tryCatch(
                dm_crossfilter_counts(data(), info$table_names, cf, rf),
                error = function(e) NULL
              )
            }
            if (is.null(result)) {
              result <- dplyr_crossfilter_counts(
                info$tables, find_key_column, info$table_names, cf, rf
              )
            }
            result
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
                eps <- 0.001 * (data_max - data_min)
                if (val[1] <= data_min + eps && val[2] >= data_max - eps) {
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
            val <- as.Date(as.numeric(change$value), origin = "1970-01-01")

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
                eps <- as.numeric(data_max - data_min) * 0.001
                if (as.numeric(val[1] - data_min) <= eps &&
                    as.numeric(data_max - val[2]) <= eps) {
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
            measure <- r_measure()
            use_measure <- !is.null(measure) && measure != ".count"

            if (use_measure) {
              # Parse "table.column" measure spec using table-name-aware helper
              mspec <- parse_measure_spec(measure)
              if (is.null(mspec)) {
                use_measure <- FALSE
              } else {
                agg <- crossfilter_agg_for_dim_measure(
                  tbl_name, dim, mspec$table, mspec$col
                )
                # NULL means no key relationship — fall back to count
                if (is.null(agg)) {
                  use_measure <- FALSE
                }
              }
            }

            if (!use_measure) {
              agg <- crossfilter_agg_for_dim(tbl_name, dim)
            }

            value_col <- if (use_measure) ".value" else ".count"
            col_header <- if (use_measure) {
              agg_label <- switch(r_agg_func(),
                mean = "Avg", median = "Median", min = "Min", max = "Max",
                "Sum"
              )
              paste0(agg_label, " of ", mspec$col)
            } else {
              "Count"
            }

            if (nrow(agg) == 0) {
              # Show empty table with dim column and zero values
              info <- dm_info()
              full_df <- info$tables[[tbl_name]]
              all_vals <- unique(as.character(full_df[[dim]]))
              all_vals[is.na(all_vals)] <- CROSSFILTER_NA
              agg <- data.frame(
                x = all_vals,
                v = rep(0, length(all_vals)),
                .selected = rep(FALSE, length(all_vals)),
                stringsAsFactors = FALSE
              )
              names(agg) <- c(dim, value_col, ".selected")
            } else {
              current_filter <- r_filters()[[tbl_name]][[dim]]
              has_filter <- !is.null(current_filter) && length(current_filter) > 0
              agg$.selected <- if (has_filter) {
                agg[[dim]] %in% current_filter
              } else {
                TRUE
              }
            }

            current_filter <- r_filters()[[tbl_name]][[dim]]
            has_filter <- !is.null(current_filter) && length(current_filter) > 0

            # Detect if values can be negative (for diverging bars)
            has_negative <- any(agg[[value_col]] < 0, na.rm = TRUE)
            max_abs_val <- max(abs(agg[[value_col]]), na.rm = TRUE)
            if (is.na(max_abs_val) || max_abs_val == 0) max_abs_val <- 1

            # Build reactable columns
            columns_list <- list()
            columns_list[[".selected"]] <- reactable::colDef(show = FALSE)

            # Dimension label column
            columns_list[[dim]] <- reactable::colDef(
              name = dim,
              minWidth = 120,
              cell = function(value, index) {
                is_selected <- agg$.selected[index]
                is_na_val <- identical(value, CROSSFILTER_NA)
                style <- if (has_filter && !is_selected) {
                  "color: #999;"
                } else if (is_na_val) {
                  "color: #9ca3af; font-size: 0.75rem;"
                } else {
                  "font-weight: 500;"
                }
                display <- if (is_na_val) "NA" else value
                shiny::tags$span(style = style, display)
              }
            )

            # Value column with inline bar (diverging for negative values)
            columns_list[[value_col]] <- reactable::colDef(
              name = col_header,
              minWidth = 140,
              align = "right",
              cell = function(value, index) {
                is_selected <- agg$.selected[index]
                pct <- abs(value) / max_abs_val * 100
                is_negative <- value < 0
                text_color <- if (has_filter && !is_selected) {
                  "#999"
                } else {
                  "#333"
                }

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

                if (has_negative) {
                  half_pct <- pct / 2
                  shiny::div(
                    style = "display: flex; align-items: center; justify-content: flex-end; gap: 6px;",
                    shiny::div(
                      style = "flex: 1; max-width: 100px; height: 14px; display: flex; position: relative;",
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
                      shiny::div(style = "width: 1px; height: 100%; background: #ccc;"),
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

          # --- Helper: compute SVG area path from density ---
          # Returns an SVG path d-attribute string for a filled area chart
          # vals: numeric vector, data_min/data_max: x-axis range
          # svg_w/svg_h: viewBox dimensions, n: density grid points
          density_to_svg_path <- function(vals, data_min, data_max,
                                          svg_w = 300, svg_h = 60, n = 64) {
            if (length(vals) < 2) return(NULL)
            d <- stats::density(vals, n = n, from = data_min, to = data_max)
            y_max <- max(d$y)
            if (y_max == 0) return(NULL)
            # Scale x to [0, svg_w], y to [svg_h, 0] (SVG y is inverted)
            xs <- (d$x - data_min) / (data_max - data_min) * svg_w
            ys <- svg_h - (d$y / y_max) * (svg_h * 0.9)  # 90% height max
            # Build path: M start, L points, L bottom-right, L bottom-left, Z
            pts <- paste(sprintf("L%.1f,%.1f", xs, ys), collapse = " ")
            sprintf("M%.1f,%.1f %s L%.1f,%.1f L%.1f,%.1f Z",
                    xs[1], ys[1], pts, xs[n], svg_h, xs[1], svg_h)
          }

          # --- Build range slider for a numeric dimension in a specific table ---
          build_range_slider <- function(tbl_name, dim) {
            info <- dm_info()
            full_df <- info$tables[[tbl_name]]
            shiny::req(is.data.frame(full_df))
            full_vals <- full_df[[dim]]
            full_vals <- full_vals[!is.na(full_vals)]
            shiny::req(length(full_vals) > 0)

            cf_df <- crossfilter_data_for_dim(tbl_name, dim)
            cf_vals <- cf_df[[dim]]
            cf_vals <- cf_vals[!is.na(cf_vals)]

            if (!is.null(r_range_filters()[[tbl_name]][[dim]])) {
              n_match <- sum(cf_vals >= r_range_filters()[[tbl_name]][[dim]][1] &
                             cf_vals <= r_range_filters()[[tbl_name]][[dim]][2])
            } else {
              n_match <- length(cf_vals)
            }

            range_text <- paste0(n_match, " of ", length(cf_vals), " rows")
            slider_uid <- gsub("[.]", "_", paste0("drs_", tbl_name, "_", dim))

            # For < 2 unique values, show static text
            if (length(unique(full_vals)) < 2) {
              return(shiny::div(
                style = "flex: 1; min-width: 200px; max-width: 350px;",
                shiny::tags$div(
                  style = "font-size: 11px; color: #888; margin-bottom: 2px;",
                  range_text
                ),
                shiny::tags$div(
                  style = "font-size: 12px; color: #666; padding: 8px 0;",
                  paste0("Single value: ", unique(full_vals))
                )
              ))
            }

            full_min <- min(full_vals)
            full_max <- max(full_vals)
            is_integer <- all(full_vals == floor(full_vals))

            current_range <- r_range_filters()[[tbl_name]][[dim]]
            cur_lo <- if (!is.null(current_range)) current_range[1] else full_min
            cur_hi <- if (!is.null(current_range)) current_range[2] else full_max

            step_val <- if (is_integer) 1 else (full_max - full_min) / 200
            min_label <- if (is_integer) format(as.integer(full_min), big.mark = ",") else format_number(full_min)
            max_label <- if (is_integer) format(as.integer(full_max), big.mark = ",") else format_number(full_max)

            # SVG density overlay (blue only when filters are active)
            has_any_filter <- length(r_filters()) > 0 || length(r_range_filters()) > 0
            svg_w <- 300
            svg_h <- 100
            path_full <- density_to_svg_path(full_vals, full_min, full_max,
                                              svg_w, svg_h)
            path_cf <- if (has_any_filter) {
              density_to_svg_path(cf_vals, full_min, full_max, svg_w, svg_h)
            }

            svg_tag <- if (!is.null(path_full)) {
              paths <- list(
                shiny::tags$path(
                  d = path_full,
                  fill = "rgba(200, 200, 200, 0.6)",
                  stroke = "rgba(160, 160, 160, 0.8)",
                  `stroke-width` = "1"
                )
              )
              if (!is.null(path_cf)) {
                paths <- c(paths, list(
                  shiny::tags$path(
                    d = path_cf,
                    fill = "rgba(84, 112, 198, 0.55)",
                    stroke = "#5470c6",
                    `stroke-width` = "1"
                  )
                ))
              }
              shiny::tags$svg(
                viewBox = sprintf("0 0 %s %s", svg_w, svg_h),
                preserveAspectRatio = "none",
                style = "width: 100%; height: 80px; display: block;",
                shiny::tagList(paths)
              )
            }

            shiny::div(
              style = "flex: 1; min-width: 200px; max-width: 350px;",
              shiny::tags$div(
                style = "font-size: 11px; color: #888; margin-bottom: 2px;",
                range_text
              ),
              svg_tag,
              # Dual-handle range slider container
              shiny::tags$div(
                id = slider_uid,
                class = "dm-cf-dual-range",
                style = "position: relative; height: 28px; margin: 0 0 4px 0;",
                # Track background
                shiny::tags$div(
                  class = "dm-cf-dual-range-track",
                  style = "position: absolute; top: 12px; left: 0; right: 0; height: 4px; background: #e5e7eb; border-radius: 2px;"
                ),
                # Fill highlight
                shiny::tags$div(
                  class = "dm-cf-dual-range-fill",
                  style = "position: absolute; top: 12px; height: 4px; background: #5470c6; border-radius: 2px;"
                ),
                # Lo handle
                shiny::tags$input(
                  type = "range",
                  class = "dm-cf-dual-range-lo",
                  min = full_min, max = full_max, value = cur_lo, step = step_val,
                  style = "position: absolute; width: 100%; top: 0; height: 28px; margin: 0; padding: 0; background: transparent; pointer-events: none; -webkit-appearance: none; appearance: none; z-index: 3;"
                ),
                # Hi handle
                shiny::tags$input(
                  type = "range",
                  class = "dm-cf-dual-range-hi",
                  min = full_min, max = full_max, value = cur_hi, step = step_val,
                  style = "position: absolute; width: 100%; top: 0; height: 28px; margin: 0; padding: 0; background: transparent; pointer-events: none; -webkit-appearance: none; appearance: none; z-index: 4;"
                ),
                # Tooltip bubbles
                shiny::tags$span(class = "dm-cf-bubble dm-cf-bubble-lo"),
                shiny::tags$span(class = "dm-cf-bubble dm-cf-bubble-hi")
              ),
              shiny::tags$div(
                style = "display: flex; justify-content: space-between; font-size: 11px; color: #999; margin-top: -2px;",
                shiny::span(min_label),
                shiny::span(max_label)
              ),
              # CSS + JS for this slider
              shiny::tags$style(shiny::HTML(sprintf(
                "
                #%s input[type=range]::-webkit-slider-thumb {
                  -webkit-appearance: none; appearance: none;
                  width: 16px; height: 16px; border-radius: 50%%;
                  background: #5470c6; border: 2px solid #fff;
                  box-shadow: 0 1px 3px rgba(0,0,0,0.3);
                  cursor: pointer; pointer-events: auto;
                }
                #%s input[type=range]::-moz-range-thumb {
                  width: 16px; height: 16px; border-radius: 50%%;
                  background: #5470c6; border: 2px solid #fff;
                  box-shadow: 0 1px 3px rgba(0,0,0,0.3);
                  cursor: pointer; pointer-events: auto;
                }
                #%s .dm-cf-bubble {
                  position: absolute;
                  top: -26px;
                  transform: translateX(-50%%);
                  background: #374151;
                  color: #fff;
                  font-size: 11px;
                  font-weight: 500;
                  padding: 2px 6px;
                  border-radius: 4px;
                  white-space: nowrap;
                  pointer-events: none;
                  opacity: 0;
                  transition: opacity 0.15s;
                  z-index: 10;
                }
                #%s .dm-cf-bubble::after {
                  content: '';
                  position: absolute;
                  top: 100%%;
                  left: 50%%;
                  transform: translateX(-50%%);
                  border: 4px solid transparent;
                  border-top-color: #374151;
                }
                #%s:hover .dm-cf-bubble,
                #%s.dm-cf-active .dm-cf-bubble {
                  opacity: 1;
                }
                ",
                slider_uid, slider_uid,
                slider_uid, slider_uid,
                slider_uid, slider_uid
              ))),
              shiny::tags$script(shiny::HTML(sprintf(
                "
                (function() {
                  var el = document.getElementById('%s');
                  if (!el) return;
                  var lo = el.querySelector('.dm-cf-dual-range-lo');
                  var hi = el.querySelector('.dm-cf-dual-range-hi');
                  var fill = el.querySelector('.dm-cf-dual-range-fill');
                  var bubLo = el.querySelector('.dm-cf-bubble-lo');
                  var bubHi = el.querySelector('.dm-cf-bubble-hi');
                  var dataMin = %s, dataMax = %s, isInt = %s;
                  var inputId = '%s', tbl = '%s', dim = '%s';
                  var debounce = null;

                  function fmtNum(v) {
                    if (isInt) return Math.round(v).toLocaleString();
                    var abs = Math.abs(v), sign = v < 0 ? '-' : '';
                    if (abs >= 1e6) return sign + (abs/1e6).toFixed(1) + 'M';
                    if (abs >= 1e3) return sign + Math.round(abs/1e3) + 'K';
                    return sign + abs.toFixed(1);
                  }

                  function updateFill() {
                    var loVal = parseFloat(lo.value), hiVal = parseFloat(hi.value);
                    var range = dataMax - dataMin;
                    if (range <= 0) return;
                    var loPct = (loVal - dataMin) / range * 100;
                    var hiPct = (hiVal - dataMin) / range * 100;
                    fill.style.left = loPct + '%%';
                    fill.style.width = (hiPct - loPct) + '%%';
                    bubLo.textContent = fmtNum(loVal);
                    bubHi.textContent = fmtNum(hiVal);
                    bubLo.style.left = loPct + '%%';
                    bubHi.style.left = hiPct + '%%';
                  }

                  function onInput() {
                    var loVal = parseFloat(lo.value), hiVal = parseFloat(hi.value);
                    if (loVal > hiVal) { lo.value = hiVal; loVal = hiVal; }
                    if (hiVal < loVal) { hi.value = loVal; hiVal = loVal; }
                    if (isInt) { loVal = Math.round(loVal); hiVal = Math.round(hiVal); }
                    updateFill();
                    clearTimeout(debounce);
                    debounce = setTimeout(function() {
                      Shiny.setInputValue(inputId, {table: tbl, dim: dim, value: [loVal, hiVal]}, {priority: 'event'});
                    }, 1000);
                  }

                  lo.addEventListener('input', function() { el.classList.add('dm-cf-active'); onInput(); });
                  hi.addEventListener('input', function() { el.classList.add('dm-cf-active'); onInput(); });
                  lo.addEventListener('change', function() { el.classList.remove('dm-cf-active'); });
                  hi.addEventListener('change', function() { el.classList.remove('dm-cf-active'); });
                  updateFill();
                })();
                ",
                slider_uid,
                full_min, full_max,
                tolower(as.character(is_integer)),
                ns("range_change"), tbl_name, dim
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

            cf_df <- crossfilter_data_for_dim(tbl_name, dim)
            cf_vals <- as.Date(cf_df[[dim]])
            cf_vals <- cf_vals[!is.na(cf_vals)]

            current_range <- r_range_filters()[[tbl_name]][[dim]]
            if (!is.null(current_range)) {
              cr_min <- as.Date(current_range[1], origin = "1970-01-01")
              cr_max <- as.Date(current_range[2], origin = "1970-01-01")
              n_match <- sum(cf_vals >= cr_min & cf_vals <= cr_max)
            } else {
              n_match <- length(cf_vals)
            }

            range_text <- paste0(n_match, " of ", length(cf_vals), " rows")
            slider_uid <- gsub("[.]", "_", paste0("dds_", tbl_name, "_", dim))

            # For < 2 unique values, show static text
            if (length(unique(full_vals)) < 2) {
              return(shiny::div(
                style = "flex: 1; min-width: 200px; max-width: 350px;",
                shiny::tags$div(
                  style = "font-size: 11px; color: #888; margin-bottom: 2px;",
                  range_text
                ),
                shiny::tags$div(
                  style = "font-size: 12px; color: #666; padding: 8px 0;",
                  paste0("Single value: ", unique(full_vals))
                )
              ))
            }

            # Convert dates to numeric (days since epoch) for range inputs
            full_min_num <- as.numeric(min(full_vals))
            full_max_num <- as.numeric(max(full_vals))

            cur_lo_num <- if (!is.null(current_range)) current_range[1] else full_min_num
            cur_hi_num <- if (!is.null(current_range)) current_range[2] else full_max_num

            min_label <- as.character(min(full_vals))
            max_label <- as.character(max(full_vals))

            # SVG density overlay (blue only when filters are active)
            has_any_filter <- length(r_filters()) > 0 || length(r_range_filters()) > 0
            full_vals_num <- as.numeric(full_vals)
            cf_vals_num <- as.numeric(cf_vals)
            svg_w <- 300
            svg_h <- 100
            path_full <- density_to_svg_path(full_vals_num, full_min_num, full_max_num,
                                              svg_w, svg_h)
            path_cf <- if (has_any_filter) {
              density_to_svg_path(cf_vals_num, full_min_num, full_max_num, svg_w, svg_h)
            }

            svg_tag <- if (!is.null(path_full)) {
              paths <- list(
                shiny::tags$path(
                  d = path_full,
                  fill = "rgba(200, 200, 200, 0.6)",
                  stroke = "rgba(160, 160, 160, 0.8)",
                  `stroke-width` = "1"
                )
              )
              if (!is.null(path_cf)) {
                paths <- c(paths, list(
                  shiny::tags$path(
                    d = path_cf,
                    fill = "rgba(84, 112, 198, 0.55)",
                    stroke = "#5470c6",
                    `stroke-width` = "1"
                  )
                ))
              }
              shiny::tags$svg(
                viewBox = sprintf("0 0 %s %s", svg_w, svg_h),
                preserveAspectRatio = "none",
                style = "width: 100%; height: 80px; display: block;",
                shiny::tagList(paths)
              )
            }

            shiny::div(
              style = "flex: 1; min-width: 200px; max-width: 350px;",
              shiny::tags$div(
                style = "font-size: 11px; color: #888; margin-bottom: 2px;",
                range_text
              ),
              svg_tag,
              # Dual-handle date slider container
              shiny::tags$div(
                id = slider_uid,
                class = "dm-cf-dual-range",
                style = "position: relative; height: 28px; margin: 0 0 4px 0;",
                shiny::tags$div(
                  class = "dm-cf-dual-range-track",
                  style = "position: absolute; top: 12px; left: 0; right: 0; height: 4px; background: #e5e7eb; border-radius: 2px;"
                ),
                shiny::tags$div(
                  class = "dm-cf-dual-range-fill",
                  style = "position: absolute; top: 12px; height: 4px; background: #5470c6; border-radius: 2px;"
                ),
                shiny::tags$input(
                  type = "range",
                  class = "dm-cf-dual-range-lo",
                  min = full_min_num, max = full_max_num, value = cur_lo_num, step = 1,
                  style = "position: absolute; width: 100%; top: 0; height: 28px; margin: 0; padding: 0; background: transparent; pointer-events: none; -webkit-appearance: none; appearance: none; z-index: 3;"
                ),
                shiny::tags$input(
                  type = "range",
                  class = "dm-cf-dual-range-hi",
                  min = full_min_num, max = full_max_num, value = cur_hi_num, step = 1,
                  style = "position: absolute; width: 100%; top: 0; height: 28px; margin: 0; padding: 0; background: transparent; pointer-events: none; -webkit-appearance: none; appearance: none; z-index: 4;"
                ),
                # Tooltip bubbles
                shiny::tags$span(class = "dm-cf-bubble dm-cf-bubble-lo"),
                shiny::tags$span(class = "dm-cf-bubble dm-cf-bubble-hi")
              ),
              shiny::tags$div(
                style = "display: flex; justify-content: space-between; font-size: 11px; color: #999; margin-top: -2px;",
                shiny::span(min_label),
                shiny::span(max_label)
              ),
              # CSS + JS for this slider
              shiny::tags$style(shiny::HTML(sprintf(
                "
                #%s input[type=range]::-webkit-slider-thumb {
                  -webkit-appearance: none; appearance: none;
                  width: 16px; height: 16px; border-radius: 50%%;
                  background: #5470c6; border: 2px solid #fff;
                  box-shadow: 0 1px 3px rgba(0,0,0,0.3);
                  cursor: pointer; pointer-events: auto;
                }
                #%s input[type=range]::-moz-range-thumb {
                  width: 16px; height: 16px; border-radius: 50%%;
                  background: #5470c6; border: 2px solid #fff;
                  box-shadow: 0 1px 3px rgba(0,0,0,0.3);
                  cursor: pointer; pointer-events: auto;
                }
                #%s .dm-cf-bubble {
                  position: absolute;
                  top: -26px;
                  transform: translateX(-50%%);
                  background: #374151;
                  color: #fff;
                  font-size: 11px;
                  font-weight: 500;
                  padding: 2px 6px;
                  border-radius: 4px;
                  white-space: nowrap;
                  pointer-events: none;
                  opacity: 0;
                  transition: opacity 0.15s;
                  z-index: 10;
                }
                #%s .dm-cf-bubble::after {
                  content: '';
                  position: absolute;
                  top: 100%%;
                  left: 50%%;
                  transform: translateX(-50%%);
                  border: 4px solid transparent;
                  border-top-color: #374151;
                }
                #%s:hover .dm-cf-bubble,
                #%s.dm-cf-active .dm-cf-bubble {
                  opacity: 1;
                }
                ",
                slider_uid, slider_uid,
                slider_uid, slider_uid,
                slider_uid, slider_uid
              ))),
              shiny::tags$script(shiny::HTML(sprintf(
                "
                (function() {
                  var el = document.getElementById('%s');
                  if (!el) return;
                  var lo = el.querySelector('.dm-cf-dual-range-lo');
                  var hi = el.querySelector('.dm-cf-dual-range-hi');
                  var fill = el.querySelector('.dm-cf-dual-range-fill');
                  var bubLo = el.querySelector('.dm-cf-bubble-lo');
                  var bubHi = el.querySelector('.dm-cf-bubble-hi');
                  var dataMin = %s, dataMax = %s;
                  var inputId = '%s', tbl = '%s', dim = '%s';
                  var debounce = null;

                  function fmtDate(days) {
                    var d = new Date(days * 86400000);
                    return d.toISOString().slice(0, 10);
                  }

                  function updateFill() {
                    var loVal = parseFloat(lo.value), hiVal = parseFloat(hi.value);
                    var range = dataMax - dataMin;
                    if (range <= 0) return;
                    var loPct = (loVal - dataMin) / range * 100;
                    var hiPct = (hiVal - dataMin) / range * 100;
                    fill.style.left = loPct + '%%';
                    fill.style.width = (hiPct - loPct) + '%%';
                    bubLo.textContent = fmtDate(loVal);
                    bubHi.textContent = fmtDate(hiVal);
                    bubLo.style.left = loPct + '%%';
                    bubHi.style.left = hiPct + '%%';
                  }

                  function onInput() {
                    var loVal = parseFloat(lo.value), hiVal = parseFloat(hi.value);
                    if (loVal > hiVal) { lo.value = hiVal; loVal = hiVal; }
                    if (hiVal < loVal) { hi.value = loVal; hiVal = loVal; }
                    updateFill();
                    clearTimeout(debounce);
                    debounce = setTimeout(function() {
                      Shiny.setInputValue(inputId, {table: tbl, dim: dim, value: [loVal, hiVal]}, {priority: 'event'});
                    }, 1000);
                  }

                  lo.addEventListener('input', function() { el.classList.add('dm-cf-active'); onInput(); });
                  hi.addEventListener('input', function() { el.classList.add('dm-cf-active'); onInput(); });
                  lo.addEventListener('change', function() { el.classList.remove('dm-cf-active'); });
                  hi.addEventListener('change', function() { el.classList.remove('dm-cf-active'); });
                  updateFill();
                })();
                ",
                slider_uid,
                full_min_num, full_max_num,
                ns("date_change"), tbl_name, dim
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

          # --- Helper: format table name with label ---
          format_tbl_label <- function(tbl_name) {
            info <- dm_info()
            # Suppress table name header for single-table dm
            if (length(info$table_names) == 1) return("")
            col_info <- column_info_per_table()
            lbl <- col_info[[tbl_name]]$table_label
            if (is.null(lbl) || lbl == "") return(toupper(tbl_name))
            shiny::tagList(
              toupper(tbl_name),
              shiny::span(
                style = "font-weight: 400; font-size: 0.85em; color: #6b7280; margin-left: 6px; text-transform: none; letter-spacing: normal;",
                lbl
              )
            )
          }

          # --- Helper: format column name with label ---
          format_col_label <- function(tbl_name, col_name) {
            col_info <- column_info_per_table()
            lbl <- col_info[[tbl_name]]$labels[[col_name]]
            if (is.null(lbl) || lbl == "") return(col_name)
            shiny::tagList(
              shiny::span(style = "font-weight: 600;", col_name),
              shiny::span(
                style = "font-size: 0.85em; color: #999; margin-left: 6px;",
                lbl
              )
            )
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

          # --- Observer: reset filter (circular-arrow button) ---
          shiny::observeEvent(input$reset_filter, {
            req <- input$reset_filter
            if (is.null(req)) return()
            tbl <- req$table
            dim_name <- req$dim
            if (is.null(tbl) || is.null(dim_name)) return()

            # Clear categorical filter values
            cat_f <- r_filters()
            if (!is.null(cat_f[[tbl]][[dim_name]])) {
              cat_f[[tbl]][[dim_name]] <- NULL
              if (length(cat_f[[tbl]]) == 0) cat_f[[tbl]] <- NULL
              r_filters(cat_f)
            }

            # Clear range filter values
            rng_f <- r_range_filters()
            if (!is.null(rng_f[[tbl]][[dim_name]])) {
              rng_f[[tbl]][[dim_name]] <- NULL
              if (length(rng_f[[tbl]]) == 0) rng_f[[tbl]] <- NULL
              r_range_filters(rng_f)
            }
          }, ignoreInit = TRUE)

          # --- Debounced search input (plain input with shiny-input-text) ---
          search_term <- shiny::reactive(input$search_input) |> shiny::debounce(150)

          # --- Track search focus state ---
          search_focused <- shiny::reactiveVal(FALSE)

          shiny::observeEvent(input$search_focus_state, {
            search_focused(isTRUE(input$search_focus_state))
          }, ignoreInit = TRUE)

          # --- Bind focus/blur events (rendered once, always present) ---
          output$search_init <- shiny::renderUI({
            shiny::tags$script(shiny::HTML(sprintf(
              "
              (function() {
                var el = document.getElementById('%s');
                if (!el || el._dmCfBound) return;
                el._dmCfBound = true;
                el.addEventListener('focus', function() {
                  Shiny.setInputValue('%s', true);
                });
                el.addEventListener('blur', function() {
                  setTimeout(function() { Shiny.setInputValue('%s', false); }, 200);
                });
              })();
              ",
              ns("search_input"), ns("search_focus_state"), ns("search_focus_state")
            )))
          })

          # --- Search results rendered inline below the search bar ---
          output$search_results <- shiny::renderUI({
            focused <- search_focused()
            if (!isTRUE(focused)) return(NULL)

            term <- search_term()
            has_search <- !is.null(term) && nzchar(trimws(term))

            col_info <- column_info_per_table()
            info <- dm_info()
            active <- r_active_dims()

            search_lower <- if (has_search) tolower(trimws(term)) else ""

            groups <- list()
            for (tbl_name in info$table_names) {
              tbl_info <- col_info[[tbl_name]]
              if (is.null(tbl_info)) next
              all_filterable <- c(
                tbl_info$dimensions,
                tbl_info$range_dimensions,
                tbl_info$date_dimensions
              )
              active_cols <- active[[tbl_name]] %||% character()
              available <- setdiff(all_filterable, active_cols)
              if (has_search) {
                tbl_labels <- tbl_info$labels
                available <- available[vapply(available, function(col) {
                  grepl(search_lower, tolower(col), fixed = TRUE) ||
                    (!is.null(tbl_labels[[col]]) &&
                     grepl(search_lower, tolower(tbl_labels[[col]]), fixed = TRUE))
                }, logical(1))]
              }
              if (length(available) > 0) {
                groups[[tbl_name]] <- available
              }
            }

            if (length(groups) == 0) {
              if (has_search) {
                return(shiny::div(
                  class = "dm-cf-search-results",
                  shiny::div(class = "dm-cf-search-empty", "No matching columns")
                ))
              }
              return(NULL)
            }

            search_input_id <- ns("search_input")
            add_filter_id <- ns("add_filter")
            focus_state_id <- ns("search_focus_state")

            # Type icon + badge helpers
            type_icon <- function(dtype) {
              icon_char <- if (is.null(dtype)) "\u2026" else switch(
                dtype,
                "categorical" = "\u2261",
                "range" = "#",
                "date" = "\u25f4",
                "\u2026"
              )
              shiny::span(class = "dm-cf-search-item-icon", icon_char)
            }

            type_badge <- function(dtype) {
              if (is.null(dtype)) return(NULL)
              info <- switch(
                dtype,
                "categorical" = list(label = "Categorical", class = "dm-cf-badge-categorical"),
                "range" = list(label = "Numeric", class = "dm-cf-badge-numeric"),
                "date" = list(label = "Date", class = "dm-cf-badge-date"),
                NULL
              )
              if (is.null(info)) return(NULL)
              shiny::span(
                class = paste("dm-cf-search-item-badge", info$class),
                info$label
              )
            }

            items <- lapply(names(groups), function(tbl_name) {
              cols <- groups[[tbl_name]]
              col_items <- lapply(cols, function(col_name) {
                dtype <- get_dim_type(tbl_name, col_name)
                shiny::div(
                  class = "dm-cf-search-item",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', {table:'%s', dim:'%s'}, {priority:'event'}); var el=document.getElementById('%s'); el.value=''; $(el).trigger('input'); el.blur(); Shiny.setInputValue('%s', false);",
                    add_filter_id, tbl_name, col_name, search_input_id, focus_state_id
                  ),
                  type_icon(dtype),
                  shiny::span(class = "dm-cf-search-item-name", format_col_label(tbl_name, col_name)),
                  type_badge(dtype)
                )
              })
              if (length(info$table_names) > 1) {
                shiny::tagList(
                  shiny::div(class = "dm-cf-search-group-header", format_tbl_label(tbl_name)),
                  col_items
                )
              } else {
                shiny::tagList(col_items)
              }
            })

            shiny::div(class = "dm-cf-search-results", items)
          })

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

            # SVG X icon for remove button (matches blockr-btn-icon pattern)
            x_icon_svg <- '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" viewBox="0 0 16 16" fill="currentColor"><path d="M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z"/></svg>'

            # SVG circular-arrow reset icon (same 14x14 size)
            reset_icon_svg <- '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" viewBox="0 0 16 16" fill="currentColor"><path fill-rule="evenodd" d="M8 3a5 5 0 1 0 4.546 2.914.5.5 0 1 1 .908-.418A6 6 0 1 1 8 2v1z"/><path d="M8 4.466V.534a.25.25 0 0 1 .41-.192l2.36 1.966c.12.1.12.284 0 .384L8.41 4.658A.25.25 0 0 1 8 4.466z"/></svg>'

            # Build widget with header (label + reset/remove buttons) above widget
            wrap_with_remove <- function(tbl_name, dim_name, widget) {
              shiny::div(
                class = "dm-cf-filter-card",
                shiny::div(
                  class = "dm-cf-filter-card-header",
                  shiny::span(class = "dm-cf-filter-card-label", format_col_label(tbl_name, dim_name)),
                  shiny::div(
                    class = "dm-cf-filter-card-actions",
                    shiny::tags$button(
                      type = "button",
                      class = "dm-cf-reset-btn",
                      title = paste0("Reset ", dim_name, " filter"),
                      onclick = sprintf(
                        "Shiny.setInputValue('%s', {table: '%s', dim: '%s'}, {priority: 'event'});",
                        ns("reset_filter"), tbl_name, dim_name
                      ),
                      shiny::HTML(reset_icon_svg)
                    ),
                    shiny::tags$button(
                      type = "button",
                      class = "dm-cf-remove-btn",
                      title = paste0("Remove ", dim_name, " filter"),
                      onclick = sprintf(
                        "Shiny.setInputValue('%s', {table: '%s', dim: '%s'}, {priority: 'event'});",
                        ns("remove_filter"), tbl_name, dim_name
                      ),
                      shiny::HTML(x_icon_svg)
                    )
                  )
                ),
                widget
              )
            }

            # Only show tables that have active filters
            table_panels <- lapply(info$table_names, function(tbl_name) {
              tbl_info <- col_info[[tbl_name]]
              if (is.null(tbl_info)) return(NULL)

              all_filterable <- c(
                tbl_info$dimensions,
                tbl_info$range_dimensions,
                tbl_info$date_dimensions
              )
              active_cols <- active[[tbl_name]] %||% character()
              active_cols <- intersect(active_cols, all_filterable)
              if (length(active_cols) == 0) return(NULL)

              # Classify active columns by type
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

              # Build filter widgets
              parts <- list()

              if (length(range_cols) > 0) {
                parts <- c(parts, list(
                  shiny::div(
                    style = "display: flex; flex-wrap: wrap; gap: 16px; margin-bottom: 12px;",
                    lapply(range_cols, function(dim) {
                      widget_id <- paste0("cf_", tbl_name, "__", dim)
                      wrap_with_remove(tbl_name, dim, shiny::uiOutput(ns(widget_id)))
                    })
                  )
                ))
              }

              if (length(date_cols) > 0) {
                parts <- c(parts, list(
                  shiny::div(
                    style = "display: flex; flex-wrap: wrap; gap: 16px; margin-bottom: 12px;",
                    lapply(date_cols, function(dim) {
                      widget_id <- paste0("cf_", tbl_name, "__", dim)
                      wrap_with_remove(tbl_name, dim, shiny::uiOutput(ns(widget_id)))
                    })
                  )
                ))
              }

              if (length(cat_cols) > 0) {
                parts <- c(parts, list(
                  shiny::div(
                    style = "display: flex; flex-wrap: wrap; gap: 16px;",
                    lapply(cat_cols, function(dim) {
                      widget_id <- paste0("cf_", tbl_name, "__", dim)
                      shiny::div(
                        style = "flex: 1; min-width: 250px; max-width: 400px;",
                        wrap_with_remove(tbl_name, dim, reactable::reactableOutput(ns(widget_id), height = "auto"))
                      )
                    })
                  )
                ))
              }

              # Table section with header (suppress header for single-table dm)
              header_tag <- if (length(info$table_names) > 1) {
                shiny::div(
                  class = "dm-cf-table-header",
                  format_tbl_label(tbl_name),
                  shiny::span(
                    class = "dm-cf-table-header-count",
                    paste0(nrow(info$tables[[tbl_name]]), " rows")
                  )
                )
              }
              shiny::div(
                class = "dm-cf-table-section",
                header_tag,
                shiny::tagList(parts)
              )
            })

            table_panels <- Filter(Negate(is.null), table_panels)

            if (length(table_panels) == 0) {
              return(shiny::div(
                style = "padding: 20px; text-align: center; color: var(--blockr-color-text-muted, #6b7280); font-style: italic;",
                "No filters active. Use the search bar above to add filters."
              ))
            }

            shiny::tagList(table_panels)
          })

          # --- Per-widget dynamic outputs (only widget content re-renders on filter change) ---
          shiny::observe({
            active <- r_active_dims()
            col_info <- column_info_per_table()

            for (tbl_name in names(active)) {
              tbl_info <- col_info[[tbl_name]]
              if (is.null(tbl_info)) next
              active_cols <- intersect(
                active[[tbl_name]],
                c(tbl_info$dimensions, tbl_info$range_dimensions, tbl_info$date_dimensions)
              )
              for (dim in active_cols) {
                local({
                  my_tbl <- tbl_name
                  my_dim <- dim
                  dtype <- get_dim_type(my_tbl, my_dim)
                  widget_id <- paste0("cf_", my_tbl, "__", my_dim)
                  if (identical(dtype, "range") || identical(dtype, "date")) {
                    output[[widget_id]] <- shiny::renderUI({
                      if (identical(dtype, "range")) {
                        build_range_slider(my_tbl, my_dim)
                      } else {
                        build_date_slider(my_tbl, my_dim)
                      }
                    })
                  } else {
                    output[[widget_id]] <- reactable::renderReactable({
                      build_filter_table(my_tbl, my_dim)
                    })
                  }
                })
              }
            }
          })

          # Helper: TRUE when dm has only one table (e.g. df crossfilter wrapper)
          is_single_table <- function() length(dm_info()$table_names) == 1

          # --- Filter status text (re-renders on filter clicks — lightweight) ---
          output$filter_status_text <- shiny::renderUI({
            cat_filters <- r_filters()
            rng_filters <- r_range_filters()
            counts <- filtered_row_count()

            has_filters <- length(cat_filters) > 0 || length(rng_filters) > 0
            has_active_dims <- length(unlist(r_active_dims())) > 0

            # Build filter chips
            chips <- list()
            if (has_filters) {
              single_tbl <- is_single_table()

              for (tbl in names(cat_filters)) {
                tbl_f <- cat_filters[[tbl]]
                for (dim in names(tbl_f)) {
                  vals <- tbl_f[[dim]]
                  display_vals <- gsub(CROSSFILTER_NA, "NA", vals, fixed = TRUE)
                  label <- if (length(display_vals) <= 2) {
                    paste(display_vals, collapse = ", ")
                  } else {
                    paste0(display_vals[1], " +", length(display_vals) - 1)
                  }
                  chips <- c(chips, list(
                    shiny::span(
                      class = "dm-cf-filter-chip",
                      if (!single_tbl) shiny::span(
                        class = "dm-cf-chip-table", paste0(tbl, ".")
                      ),
                      paste0(dim, "=", label)
                    )
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
                  chips <- c(chips, list(
                    shiny::span(
                      class = "dm-cf-filter-chip",
                      if (!single_tbl) shiny::span(
                        class = "dm-cf-chip-table", paste0(tbl, ".")
                      ),
                      paste0(dim, " [", lo_str, "\u2013", hi_str, "]")
                    )
                  ))
                }
              }
            }

            # Row count
            if (has_filters) {
              row_text <- paste0(
                format(counts$filtered, big.mark = ","), " / ",
                format(counts$total, big.mark = ","), " rows"
              )
            } else {
              row_text <- paste0(
                format(counts$total, big.mark = ","),
                " rows in ", counts$n_tables, " tables"
              )
            }

            timing <- r_last_timing()
            timing_text <- if (!is.null(timing)) paste0(timing, "ms") else ""

            reset_icon_small <- shiny::HTML('<svg xmlns="http://www.w3.org/2000/svg" width="12" height="12" viewBox="0 0 16 16" fill="currentColor"><path fill-rule="evenodd" d="M8 3a5 5 0 1 0 4.546 2.914.5.5 0 1 1 .908-.418A6 6 0 1 1 8 2v1z"/><path d="M8 4.466V.534a.25.25 0 0 1 .41-.192l2.36 1.966c.12.1.12.284 0 .384L8.41 4.658A.25.25 0 0 1 8 4.466z"/></svg>')
            x_icon_small <- shiny::HTML('<svg xmlns="http://www.w3.org/2000/svg" width="12" height="12" viewBox="0 0 16 16" fill="currentColor"><path d="M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z"/></svg>')

            shiny::tagList(
              if (length(chips) > 0) chips,
              if (has_filters) {
                shiny::tags$button(
                  class = "dm-cf-reset-all-btn",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', Date.now(), {priority: 'event'});",
                    ns("reset_all_filters")
                  ),
                  reset_icon_small,
                  "Reset all"
                )
              },
              if (has_active_dims) {
                shiny::tags$button(
                  class = "dm-cf-clear-btn",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', Date.now(), {priority: 'event'});",
                    ns("clear_filters")
                  ),
                  x_icon_small,
                  "Clear all"
                )
              },
              if (has_active_dims || has_filters) shiny::span(class = "dm-cf-separator"),
              shiny::span(class = "dm-cf-row-count", row_text),
              if (nzchar(timing_text)) {
                shiny::span(class = "dm-cf-timing", timing_text)
              }
            )
          })

          # --- Filter controls (advanced options section) ---
          output$filter_controls <- shiny::renderUI({
            backend <- r_backend()

            # Build measure choices from column_info_per_table
            col_info <- column_info_per_table()
            single_tbl <- is_single_table()
            current_measure <- r_measure() %||% ".count"
            measure_choices <- ".count"
            measure_labels <- "Count"
            for (tbl in names(col_info)) {
              m_cols <- col_info[[tbl]]$measures
              if (length(m_cols) > 0) {
                measure_choices <- c(
                  measure_choices,
                  paste0(tbl, ".", m_cols)
                )
                measure_labels <- c(
                  measure_labels,
                  if (single_tbl) m_cols else paste0(tbl, ": ", m_cols)
                )
              }
            }
            names(measure_choices) <- measure_labels

            backend_choices <- stats::setNames(
              available_backends, available_backends
            )

            shiny::div(
              class = "dm-cf-advanced-grid",
              # Measure selector
              shiny::div(
                shiny::selectizeInput(
                  ns("measure_switch"),
                  label = "Measure",
                  choices = measure_choices,
                  selected = current_measure,
                  width = "100%"
                )
              ),
              # Aggregation function selector (only when measure != .count)
              if (current_measure != ".count") shiny::div(
                shiny::selectizeInput(
                  ns("agg_func_switch"),
                  label = "Aggregation",
                  choices = c(Sum = "sum", Average = "mean", Median = "median",
                              Min = "min", Max = "max"),
                  selected = r_agg_func(),
                  width = "100%"
                )
              ),
              # Backend selector
              shiny::div(
                shiny::selectizeInput(
                  ns("backend_switch"),
                  label = "Backend",
                  choices = backend_choices,
                  selected = backend,
                  width = "100%"
                )
              )
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
              range_filters = r_range_filters,
              measure = r_measure,
              agg_func = r_agg_func
            )
          )
        }
      )
  }
}

dm_crossfilter_ui <- function(id) {
  ns <- shiny::NS(id)

  # SVG search icon (16x16)
  search_icon <- shiny::HTML(
    '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="7" cy="7" r="4.5"/><line x1="10.2" y1="10.2" x2="14" y2="14"/></svg>'
  )

  shiny::tagList(
    shiny::div(
      class = "dm-crossfilter-container",
      style = "margin: -16px -16px -10px -16px; padding: 6px 10px;",
      dm_crossfilter_search_css(),
      dm_crossfilter_table_css(),
      # Search bar — plain input with shiny-input-text for auto-binding
      shiny::tags$div(
        class = "dm-cf-search-wrapper",
        shiny::tags$span(
          class = "dm-cf-search-icon",
          search_icon
        ),
        shiny::tags$input(
          type = "text",
          id = ns("search_input"),
          class = "dm-cf-search-input shiny-input-text",
          placeholder = "Search columns to add filter...",
          autocomplete = "off"
        )
      ),
      # Search results (rendered server-side when typing)
      # Invisible output to bind focus/blur events on search input
      shiny::uiOutput(ns("search_init")),
      shiny::uiOutput(ns("search_results")),
      shiny::div(
        class = "dm-cf-status-bar",
        shiny::uiOutput(ns("filter_status_text"))
      ),
      shiny::uiOutput(ns("tables_grid")),
      # Advanced options toggle (below filters)
      shiny::div(
        class = "dm-cf-advanced-toggle",
        id = ns("advanced-toggle"),
        onclick = sprintf(
          "document.getElementById('%s').classList.toggle('expanded');
           document.querySelector('#%s .dm-cf-chevron').classList.toggle('rotated');",
          ns("advanced-options"),
          ns("advanced-toggle")
        ),
        shiny::tags$span(class = "dm-cf-chevron", "\u203A"),
        "Advanced options"
      ),
      # Collapsible section
      shiny::div(
        id = ns("advanced-options"),
        class = "dm-cf-advanced-section",
        shiny::uiOutput(ns("filter_controls"))
      )
    )
  )
}

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
#' @param measure Measure column to aggregate in categorical filters, as
#'   `"table.column"` (e.g., `"sales.amount"`). Defaults to `NULL` (row count).
#'   When set, categorical filter bars show aggregated values per dimension value
#'   instead of row counts.
#' @param agg_func Aggregation function name: `"sum"` (default), `"mean"`,
#'   `"median"`, `"min"`, or `"max"`. Controls how the measure column is
#'   aggregated per dimension value.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A blockr transform block that returns a filtered dm object
#'
#' @export
new_dm_crossfilter_block <- function(
    active_dims = list(),
    filters = list(),
    range_filters = list(),
    measure = NULL,
    agg_func = NULL,
    ...
) {
  blockr.core::new_transform_block(
    server = dm_crossfilter_server_factory(
      active_dims, filters, range_filters, measure, agg_func = agg_func
    ),
    ui = dm_crossfilter_ui,
    # Note: dat_valid intentionally omitted to avoid double evaluation on startup.
    # The server already validates via shiny::req(inherits(dm_obj, "dm")).
    allow_empty_state = c("active_dims", "filters", "range_filters", "measure", "agg_func"),
    external_ctrl = TRUE,
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
