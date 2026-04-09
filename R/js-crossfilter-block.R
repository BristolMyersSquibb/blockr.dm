#' JS Crossfilter Block (experimental)
#'
#' Client-side crossfilter using crossfilter2.js. R builds lookup tables and
#' ships them to the browser; all filtering, counting, and UI updates happen
#' in JavaScript for instant responsiveness.
#'
#' @param active_dims Named list of per-table active filter columns.
#'   E.g., `list(adsl = c("SEX", "AGE"), adae = c("AESEV"))`
#' @param filters Named list of per-table categorical filters.
#'   E.g., `list(adae = list(AESEV = c("SEVERE")))`
#' @param range_filters Named list of per-table range filters.
#'   E.g., `list(adsl = list(AGE = c(40, 80)))`
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A blockr transform block with client-side crossfiltering
#'
#' @export
new_js_crossfilter_block <- function(
    active_dims = list(),
    filters = list(),
    range_filters = list(),
    ...
) {
  blockr.core::new_transform_block(
    server = js_crossfilter_server(active_dims, filters, range_filters),
    ui = js_crossfilter_ui,
    allow_empty_state = c("active_dims", "filters", "range_filters"),
    external_ctrl = TRUE,
    class = "js_crossfilter_block",
    ...
  )
}

#' @method block_output js_crossfilter_block
#' @export
block_output.js_crossfilter_block <- function(x, result, session) {
  block_output.dm_block(x, result, session)
}

#' @method block_ui js_crossfilter_block
#' @export
block_ui.js_crossfilter_block <- function(id, x, ...) {
  block_ui.dm_block(id, x, ...)
}

#' @method block_render_trigger js_crossfilter_block
#' @export
block_render_trigger.js_crossfilter_block <- function(
    x, session = blockr.core::get_session()
) {
  NULL
}


# -- Server ------------------------------------------------------------------

js_crossfilter_server <- function(active_dims, filters, range_filters) {
  function(id, data) {
    shiny::moduleServer(id, function(input, output, session) {
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

        # Sort: parents before children
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

      # --- Key column helper ---
      find_key_column <- function(tbl_name) {
        info <- dm_info()
        pk_row <- info$pks[info$pks$table == tbl_name, ]
        if (nrow(pk_row) > 0) return(pk_row$pk_col[[1]][[1]])
        fk_row <- info$fks[info$fks$child_table == tbl_name, ]
        if (nrow(fk_row) > 0) return(fk_row$child_fk_cols[[1]][[1]])
        NULL
      }

      # --- Column classification ---
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
          is_low_cardinality <- vapply(seq_along(df), function(i) {
            col <- df[[i]]
            if (is.numeric(col)) {
              length(unique(col[is.finite(col)])) <= 10
            } else {
              length(unique(col)) <= 10
            }
          }, logical(1))
          names(is_low_cardinality) <- names(df)

          is_dimension <- (!is_numeric & !is_date) |
            (is_numeric & is_low_cardinality)
          is_range_dim <- is_numeric & !is_low_cardinality
          is_date_dim <- is_date

          # Exclude key columns from dimensions
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

          col_labels <- vapply(names(df), function(cn) {
            lbl <- attr(df[[cn]], "label")
            if (is.null(lbl)) "" else lbl
          }, character(1))

          result[[tbl_name]] <- list(
            dimensions = as.list(names(df)[is_dimension]),
            range_dimensions = as.list(names(df)[is_range_dim]),
            date_dimensions = as.list(names(df)[is_date_dim]),
            labels = col_labels
          )
        }
        result
      })

      # --- State ---
      as_rv <- function(x, default = x) {
        if (inherits(x, "reactiveVal")) x else shiny::reactiveVal(default)
      }
      r_active_dims <- as_rv(active_dims)
      r_filters <- as_rv(filters)
      r_range_filters <- as_rv(range_filters)

      # --- Add/remove filter dimensions from JS ---
      shiny::observeEvent(input$add_filter, {
        req <- input$add_filter
        tbl <- req$table
        dim <- req$dim
        active <- r_active_dims()
        current <- active[[tbl]] %||% character()
        if (!dim %in% current) {
          active[[tbl]] <- c(current, dim)
          r_active_dims(active)
        }
      })

      shiny::observeEvent(input$remove_filter, {
        req <- input$remove_filter
        tbl <- req$table
        dim <- req$dim
        active <- r_active_dims()
        current <- active[[tbl]] %||% character()
        active[[tbl]] <- setdiff(current, dim)
        if (length(active[[tbl]]) == 0) active[[tbl]] <- NULL
        r_active_dims(active)
        # Also clear any filter on this dim
        flt <- r_filters()
        if (!is.null(flt[[tbl]])) {
          flt[[tbl]][[dim]] <- NULL
          if (length(flt[[tbl]]) == 0) flt[[tbl]] <- NULL
          r_filters(flt)
        }
        rflt <- r_range_filters()
        if (!is.null(rflt[[tbl]])) {
          rflt[[tbl]][[dim]] <- NULL
          if (length(rflt[[tbl]]) == 0) rflt[[tbl]] <- NULL
          r_range_filters(rflt)
        }
      })

      shiny::observeEvent(input$clear_filters, {
        r_active_dims(list())
        r_filters(list())
        r_range_filters(list())
      })

      # --- Build lookups and send to JS ---
      shiny::observe({
        info <- dm_info()
        active <- r_active_dims()
        col_info <- column_info_per_table()
        t_start <- proc.time()[3]

        # Build lookups only if there are active dims
        lookup_info <- NULL
        has_dims <- any(vapply(active, length, integer(1)) > 0)
        if (has_dims) {
          lookup_info <- build_crossfilter_lookups(
            info$tables, active, info$pks, info$fks
          )
        }

        if (!is.null(lookup_info)) {
          # Encode NA sentinels and serialize as JSON strings (fast path)
          encoded_lookups <- lapply(lookup_info$lookups, function(df) {
            for (cn in names(df)) {
              col <- df[[cn]]
              if (is.character(col) || is.factor(col)) {
                col <- as.character(col)
                col[is.na(col)] <- CROSSFILTER_NA
                col[col == ""] <- CROSSFILTER_EMPTY
                df[[cn]] <- col
              }
            }
            # toJSON returns a "json" class; Shiny embeds it verbatim
            # (json_verbatim = TRUE). JS receives a parsed columnar object.
            jsonlite::toJSON(df, dataframe = "columns")
          })

          # Force arrays for length-1 vectors (avoid JSON scalar unboxing)
          safe_active <- lapply(active, as.list)

          t_elapsed <- round((proc.time()[3] - t_start) * 1000)
          sizes <- vapply(encoded_lookups, nchar, numeric(1))
          message(sprintf(
            "[js-crossfilter] R prep: %dms | lookups: %s",
            t_elapsed,
            paste(names(sizes), round(sizes / 1024), "KB", sep = "=", collapse = ", ")
          ))

          session$sendCustomMessage("js-crossfilter-data", list(
            id = ns("crossfilter_input"),
            lookups = encoded_lookups,
            dim_source = lookup_info$dim_source,
            parent_key = lookup_info$parent_key,
            parent_table = lookup_info$parent_table,
            child_fk_cols = lookup_info$child_fk_cols,
            column_info = col_info,
            all_columns = col_info,
            active_dims = safe_active
          ))
        } else {
          # No active dims — send empty state with column catalog
          safe_active <- lapply(active, as.list)
          session$sendCustomMessage("js-crossfilter-data", list(
            id = ns("crossfilter_input"),
            lookups = list(),
            dim_source = list(),
            parent_key = NULL,
            parent_table = NULL,
            child_fk_cols = list(),
            column_info = col_info,
            all_columns = col_info,
            active_dims = safe_active
          ))
        }
      })

      # --- JS -> R: receive filter state ---
      shiny::observeEvent(input$crossfilter_input, {
        state <- input$crossfilter_input
        r_filters(state$cat_filters %||% list())
        r_range_filters(state$rng_filters %||% list())
      })

      # --- Expression: dm::dm_filter() ---
      list(
        expr = shiny::reactive({
          cat_filters <- r_filters()
          rng_filters <- r_range_filters()

          if (length(cat_filters) == 0 && length(rng_filters) == 0) {
            return(quote(identity(data)))
          }

          table_conditions <- list()
          all_tables <- unique(c(names(cat_filters), names(rng_filters)))

          for (tbl in all_tables) {
            conditions <- list()

            tbl_cat <- cat_filters[[tbl]]
            if (!is.null(tbl_cat)) {
              for (dim in names(tbl_cat)) {
                val <- tbl_cat[[dim]]
                if (length(val) == 1) {
                  conditions <- c(conditions, list(
                    call("==", as.name(dim), val)
                  ))
                } else {
                  conditions <- c(conditions, list(
                    call("%in%", as.name(dim), val)
                  ))
                }
              }
            }

            tbl_rng <- rng_filters[[tbl]]
            if (!is.null(tbl_rng)) {
              info <- dm_info()
              tbl_df <- info$tables[[tbl]]
              for (dim in names(tbl_rng)) {
                rng <- tbl_rng[[dim]]
                if (!is.null(rng) && length(rng) == 2) {
                  is_date_col <- dim %in% names(tbl_df) &&
                    inherits(tbl_df[[dim]], c("Date", "POSIXct", "POSIXlt"))
                  if (is_date_col) {
                    lo_val <- call(
                      "as.Date",
                      as.character(as.Date(rng[1], origin = "1970-01-01"))
                    )
                    hi_val <- call(
                      "as.Date",
                      as.character(as.Date(rng[2], origin = "1970-01-01"))
                    )
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
                combined <- Reduce(
                  function(a, b) call("&", a, b),
                  conditions
                )
              }
              table_conditions[[tbl]] <- combined
            }
          }

          if (length(table_conditions) == 0) {
            return(quote(identity(data)))
          }

          expr <- call("dm_filter", quote(data))
          for (tbl in names(table_conditions)) {
            expr[[tbl]] <- table_conditions[[tbl]]
          }
          expr[[1]] <- quote(dm::dm_filter)
          expr
        }),
        state = list(
          active_dims = r_active_dims,
          filters = r_filters,
          range_filters = r_range_filters
        )
      )
    })
  }
}


# -- UI ----------------------------------------------------------------------

js_crossfilter_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    js_crossfilter_deps(),
    shiny::div(
      id = ns("crossfilter_input"),
      class = "js-crossfilter-container"
    )
  )
}

js_crossfilter_deps <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "crossfilter2",
      version = "1.5.4",
      src = system.file("js", package = "blockr.dm"),
      script = "crossfilter2.min.js"
    ),
    htmltools::htmlDependency(
      name = "js-crossfilter-block-js",
      version = utils::packageVersion("blockr.dm"),
      src = system.file("js", package = "blockr.dm"),
      script = "js-crossfilter-block.js"
    ),
    htmltools::htmlDependency(
      name = "js-crossfilter-block-css",
      version = utils::packageVersion("blockr.dm"),
      src = system.file("css", package = "blockr.dm"),
      stylesheet = "js-crossfilter-block.css"
    )
  )
}
