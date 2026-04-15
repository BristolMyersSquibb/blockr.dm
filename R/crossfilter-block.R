#' Crossfilter Block
#'
#' Client-side crossfilter using crossfilter2.js. R builds lookup tables and
#' ships them to the browser; all filtering, counting, and UI updates happen
#' in JavaScript for instant responsiveness. Accepts either a data frame or
#' a [dm][dm::dm()] object.
#'
#' @param active_dims Named list of per-table active filter columns.
#'   E.g., `list(adsl = c("SEX", "AGE"), adae = c("AESEV"))`
#' @param filters Named list of per-table categorical filters.
#'   E.g., `list(adae = list(AESEV = c("SEVERE")))`
#' @param range_filters Named list of per-table range filters.
#'   E.g., `list(adsl = list(AGE = c(40, 80)))`
#' @param measure Measure column as `"table.column"` string, or `NULL`
#'   for row counts. E.g., `"orders.amount"`.
#' @param agg_func Aggregation function: `"sum"` or `"mean"`. Only used
#'   when `measure` is set.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A blockr transform block with client-side crossfiltering
#'
#' @export
new_crossfilter_block <- function(
  active_dims = list(),
  filters = list(),
  range_filters = list(),
  measure = NULL,
  agg_func = NULL,
  ...
) {
  blockr.core::new_transform_block(
    server = crossfilter_server(
      active_dims, filters, range_filters, measure, agg_func
    ),
    ui = crossfilter_ui,
    allow_empty_state = c(
      "active_dims", "filters", "range_filters", "measure", "agg_func"
    ),
    external_ctrl = TRUE,
    class = "crossfilter_block",
    ...
  )
}

# Internal helper that builds the deprecation-banner UI wrapper shared
# by the legacy js_crossfilter_block and dm_crossfilter_block constructors.
deprecated_crossfilter_banner <- function(legacy_name) {
  shiny::div(
    class = "alert alert-warning blockr-deprecated-banner",
    role = "alert",
    style = paste0(
      "margin: 8px 0; padding: 10px 12px; border-radius: 4px; ",
      "background-color: #fff3cd; color: #856404; ",
      "border: 1px solid #ffeeba; font-size: 0.9rem;"
    ),
    shiny::tags$strong("Deprecated block: "),
    "this is the legacy ",
    shiny::tags$code(legacy_name),
    ". Use ",
    shiny::tags$code("crossfilter_block"),
    " instead \u2014 same functionality, canonical name."
  )
}

#' Legacy constructor for the crossfilter block (deprecated)
#'
#' `new_js_crossfilter_block()` is the pre-rename alias kept so saved boards
#' that reference the `js_crossfilter_block` UID continue to deserialize.
#' All new code should use [new_crossfilter_block()] directly. This wrapper
#' emits a [.Deprecated()] warning at construction time and tags the
#' produced block with a `js_crossfilter_block` subclass so the served
#' Shiny UI renders a visible deprecation banner above the normal block
#' output.
#'
#' @inheritParams new_crossfilter_block
#' @return A crossfilter block carrying a `js_crossfilter_block` subclass.
#' @export
new_js_crossfilter_block <- function(
  active_dims = list(),
  filters = list(),
  range_filters = list(),
  measure = NULL,
  agg_func = NULL,
  ...
) {
  .Deprecated(
    new = "new_crossfilter_block",
    package = "blockr.dm",
    msg = paste0(
      "'new_js_crossfilter_block' is deprecated and will be removed in a ",
      "future release. Use 'new_crossfilter_block' instead \u2014 it has the ",
      "same signature and behaviour under the canonical name."
    )
  )
  blk <- new_crossfilter_block(
    active_dims = active_dims,
    filters = filters,
    range_filters = range_filters,
    measure = measure,
    agg_func = agg_func,
    ...
  )
  class(blk) <- c("js_crossfilter_block", class(blk))
  blk
}

#' @method block_ui js_crossfilter_block
#' @export
block_ui.js_crossfilter_block <- function(id, x, ...) {
  shiny::tagList(
    deprecated_crossfilter_banner("js_crossfilter_block"),
    NextMethod()
  )
}

#' Legacy dm-only crossfilter constructor (deprecated)
#'
#' `new_dm_crossfilter_block()` is the pre-unification alias kept so saved
#' boards that reference the `dm_crossfilter_block` UID continue to
#' deserialize. The block now delegates to [new_crossfilter_block()] —
#' which accepts both data frames and dm objects — and emits a
#' [.Deprecated()] warning at construction time. Produced blocks carry
#' a `dm_crossfilter_block` subclass so the served Shiny UI renders a
#' visible deprecation banner above the normal block output.
#'
#' @inheritParams new_crossfilter_block
#' @return A crossfilter block carrying a `dm_crossfilter_block` subclass.
#' @export
new_dm_crossfilter_block <- function(
  active_dims = list(),
  filters = list(),
  range_filters = list(),
  measure = NULL,
  agg_func = NULL,
  ...
) {
  .Deprecated(
    new = "new_crossfilter_block",
    package = "blockr.dm",
    msg = paste0(
      "'new_dm_crossfilter_block' is deprecated and will be removed in a ",
      "future release. Use 'new_crossfilter_block' instead \u2014 it handles ",
      "both dm and data.frame inputs under the canonical name."
    )
  )
  blk <- new_crossfilter_block(
    active_dims = active_dims,
    filters = filters,
    range_filters = range_filters,
    measure = measure,
    agg_func = agg_func,
    ...
  )
  class(blk) <- c("dm_crossfilter_block", class(blk))
  blk
}

#' @method block_ui dm_crossfilter_block
#' @export
block_ui.dm_crossfilter_block <- function(id, x, ...) {
  shiny::tagList(
    deprecated_crossfilter_banner("dm_crossfilter_block"),
    NextMethod()
  )
}

#' @method block_output crossfilter_block
#' @export
block_output.crossfilter_block <- function(x, result, session) {
  if (inherits(result, "dm")) {
    return(block_output.dm_block(x, result, session))
  }
  # Data frame or other: use blockr.extra's dynamic renderer if available,
  # otherwise a simple HTML table preview
  render_fn <- tryCatch(
    get("render_dynamic_output", asNamespace("blockr.extra")),
    error = function(e) NULL
  )
  if (!is.null(render_fn)) {
    render_fn(result, x, session)
  } else {
    shiny::renderUI({
      if (!is.data.frame(result)) return(NULL)
      dat <- utils::head(result, 100)
      shiny::tags$div(
        style = "max-height: 400px; overflow: auto;",
        shiny::tags$table(
          class = "table table-sm table-striped",
          shiny::tags$thead(shiny::tags$tr(
            lapply(names(dat), function(n) shiny::tags$th(n))
          )),
          shiny::tags$tbody(
            lapply(seq_len(nrow(dat)), function(i) {
              shiny::tags$tr(lapply(dat[i, ], function(v) {
                shiny::tags$td(as.character(v))
              }))
            })
          )
        )
      )
    })
  }
}

#' @method block_ui crossfilter_block
#' @export
block_ui.crossfilter_block <- function(id, x, ...) {
  shiny::uiOutput(shiny::NS(id, "result"))
}

#' @method block_render_trigger crossfilter_block
#' @export
block_render_trigger.crossfilter_block <- function(
  x, session = blockr.core::get_session()
) {
  NULL
}


# -- Server ------------------------------------------------------------------

crossfilter_server <- function(active_dims, filters, range_filters,
                               measure, agg_func) {
  function(id, data) {
    shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # --- Coerce input: accept data.frame or dm ---
      r_input_is_df <- shiny::reactiveVal(FALSE)

      dm_data <- shiny::reactive({
        d <- data()
        if (is.data.frame(d)) {
          r_input_is_df(TRUE)
          dm::dm(.tbl = d)
        } else {
          shiny::req(inherits(d, "dm"))
          tbl_names <- names(dm::dm_get_tables(d))
          r_input_is_df(length(tbl_names) == 1)
          d
        }
      })

      # --- dm info: extract tables, PKs, FKs ---
      dm_info <- shiny::reactive({
        dm_obj <- dm_data()

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
            measures = as.list(names(df)[is_numeric & !is_low_cardinality]),
            labels = col_labels
          )
        }
        result
      })

      # --- State ---
      r_active_dims <- shiny::reactiveVal(active_dims)
      r_filters <- shiny::reactiveVal(filters)
      r_range_filters <- shiny::reactiveVal(range_filters)
      r_measure <- shiny::reactiveVal(measure %||% ".count")
      r_agg_func <- shiny::reactiveVal(agg_func %||% "sum")

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

      shiny::observeEvent(input$measure_switch, {
        val <- input$measure_switch
        if (!is.null(val)) r_measure(val)
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$agg_func_switch, {
        val <- input$agg_func_switch
        if (!is.null(val)) r_agg_func(val)
      }, ignoreInit = TRUE)

      # --- Build lookups and send to JS ---
      shiny::observe({
        info <- dm_info()
        active <- r_active_dims()
        col_info <- column_info_per_table()
        cur_measure <- r_measure()
        cur_agg_func <- r_agg_func()
        t_start <- proc.time()[3]

        # Build lookups only if there are active dims
        lookup_info <- NULL
        has_dims <- any(vapply(active, length, integer(1)) > 0)
        if (has_dims) {
          measure_col <- if (cur_measure != ".count") cur_measure else NULL
          # Try standard single-parent lookup first
          lookup_info <- build_crossfilter_lookups(
            info$tables, active, info$pks, info$fks, measure_col
          )
          # Check if all active dims are covered
          if (!is.null(lookup_info)) {
            all_dims <- unlist(active, use.names = FALSE)
            covered <- names(lookup_info$dim_source)
            if (!all(all_dims %in% covered)) {
              # Standard builder missed some dims — use flat fallback
              lookup_info <- NULL
            }
          }
          # Fallback: use dm_flatten_to_tbl for multi-parent schemas
          if (is.null(lookup_info)) {
            lookup_info <- build_lookups_flat(
              dm_data(), active, measure_col
            )
          }
          # Last resort: no FKs at all — independent per-table lookups
          if (is.null(lookup_info)) {
            lookup_info <- build_lookups_independent(
              info$tables, active, measure_col
            )
          }
        }

        if (!is.null(lookup_info)) {
          # Encode NA sentinels and serialize as columnar JSON (fast path)
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
            paste(
              names(sizes), round(sizes / 1024), "KB",
              sep = "=", collapse = ", "
            )
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
            active_dims = safe_active,
            measure = cur_measure,
            agg_func = cur_agg_func
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
                rng <- unlist(tbl_rng[[dim]])
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

          # Single-table input (data.frame or 1-table dm): use dplyr::filter
          if (r_input_is_df()) {
            # Combine all conditions (from the single table)
            all_conds <- unname(table_conditions)
            combined <- if (length(all_conds) == 1) {
              all_conds[[1]]
            } else {
              Reduce(function(a, b) call("&", a, b), all_conds)
            }
            return(as.call(list(quote(dplyr::filter), quote(data), combined)))
          }

          # Multi-table dm: use dm::dm_filter
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
          range_filters = r_range_filters,
          measure = r_measure,
          agg_func = r_agg_func
        )
      )
    })
  }
}


# -- Flat lookup builder (multi-parent / snowflake schemas) ------------------

#' Build crossfilter lookups using dm_flatten_to_tbl for arbitrary schemas.
#' Falls back to a single flat table when the standard star-schema builder
#' returns NULL.
#' @keywords internal
build_lookups_flat <- function(dm_obj, active_dims, measure_col = NULL) {
  if (!inherits(dm_obj, "dm")) return(NULL)

  table_names <- names(dm::dm_get_tables(dm_obj))
  pks <- dm::dm_get_all_pks(dm_obj)
  fks <- dm::dm_get_all_fks(dm_obj)

  # Need at least one FK to flatten
  if (nrow(fks) == 0) return(NULL)

  # Find the "fact table" — the table with most outgoing FKs
  fk_counts <- vapply(table_names, function(tbl) {
    sum(fks$child_table == tbl)
  }, integer(1))
  fact_table <- names(which.max(fk_counts))
  if (is.null(fact_table) || fk_counts[fact_table] == 0) return(NULL)

  # Flatten the dm starting from the fact table.
  # Non-recursive joins direct parents; if that misses dims, try recursive.
  flat <- tryCatch(
    dm::dm_flatten_to_tbl(dm_obj, !!rlang::sym(fact_table),
                          .join = dplyr::left_join),
    error = function(e) NULL
  )
  # If some active dims are missing, try recursive flatten
  if (!is.null(flat)) {
    all_dims <- unlist(active_dims, use.names = FALSE)
    missing <- setdiff(all_dims, names(flat))
    if (length(missing) > 0) {
      flat_r <- tryCatch(
        dm::dm_flatten_to_tbl(dm_obj, !!rlang::sym(fact_table),
                              .recursive = TRUE, .join = dplyr::left_join),
        error = function(e) NULL
      )
      if (!is.null(flat_r)) flat <- flat_r
    }
  }
  if (is.null(flat) || nrow(flat) == 0) return(NULL)

  # Select only the columns we need: active dims + key + measure
  all_dims <- unlist(active_dims, use.names = FALSE)
  keep_cols <- intersect(
    unique(c(all_dims, if (!is.null(measure_col)) {
      sub("^[^.]+\\.", "", measure_col)
    })),
    names(flat)
  )
  if (length(keep_cols) == 0) return(NULL)

  # Find a key column (PK of fact table or first available PK)
  fact_pk <- NULL
  pk_row <- pks[pks$table == fact_table, ]
  if (nrow(pk_row) > 0) fact_pk <- pk_row$pk_col[[1]][[1]]

  if (!is.null(fact_pk) && fact_pk %in% names(flat)) {
    keep_cols <- unique(c(fact_pk, keep_cols))
  }

  lookup <- flat[, keep_cols, drop = FALSE]

  # Build dim_source: map each dim to its original table
  dim_source <- list()
  for (tbl in names(active_dims)) {
    for (d in active_dims[[tbl]]) {
      dim_source[[d]] <- tbl
    }
  }

  # Determine parent table (table with PK, no outgoing FKs)
  parent_candidates <- setdiff(
    pks$table,
    fks$child_table
  )
  parent_table <- if (length(parent_candidates) > 0) {
    parent_candidates[1]
  } else {
    pks$table[1]
  }
  if (!is.null(fact_pk)) {
    parent_key <- fact_pk
  } else {
    pk_row <- pks[pks$table == parent_table, ]
    parent_key <- if (nrow(pk_row) > 0) {
      pk_row$pk_col[[1]][[1]]
    } else {
      NULL
    }
  }

  list(
    lookups = stats::setNames(list(lookup), fact_table),
    dim_source = dim_source,
    parent_key = parent_key,
    child_fk_cols = stats::setNames(parent_key, fact_table),
    parent_table = parent_table,
    child_tables = fact_table
  )
}


# -- Independent lookup builder (no FK relationships) -----------------------

#' Build per-table lookups when there are no FK relationships.
#' Each table gets its own lookup with only its active dims. No cross-table
#' filtering is possible (no shared key), but within-table filtering works.
#' @keywords internal
build_lookups_independent <- function(
  tables, active_dims, measure_col = NULL
) {
  lookups <- list()
  dim_source <- list()

  for (tbl in names(active_dims)) {
    dims <- active_dims[[tbl]]
    if (length(dims) == 0) next
    df <- tables[[tbl]]
    if (is.null(df) || !is.data.frame(df)) next

    # Select active dims + measure column
    keep <- intersect(dims, names(df))
    if (!is.null(measure_col)) {
      mc <- sub("^[^.]+\\.", "", measure_col)
      if (mc %in% names(df)) keep <- unique(c(keep, mc))
    }
    if (length(keep) == 0) next

    lookups[[tbl]] <- df[, keep, drop = FALSE]
    for (d in dims) {
      if (d %in% names(df)) dim_source[[d]] <- tbl
    }
  }

  if (length(lookups) == 0) return(NULL)

  # No parent/child structure — each table is independent.
  # Use empty strings for FK cols (no cross-table join possible).
  child_fk_cols <- as.list(stats::setNames(
    rep("", length(lookups)),
    names(lookups)
  ))

  list(
    lookups = lookups,
    dim_source = dim_source,
    parent_key = "",
    child_fk_cols = child_fk_cols,
    parent_table = "",
    child_tables = names(lookups)
  )
}


# -- UI ----------------------------------------------------------------------

crossfilter_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    crossfilter_deps(),
    shiny::div(
      id = ns("crossfilter_input"),
      class = "js-crossfilter-container"
    )
  )
}

crossfilter_deps <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "crossfilter2",
      version = "1.5.4",
      src = system.file("js", package = "blockr.dm"),
      script = "crossfilter2.min.js"
    ),
    htmltools::htmlDependency(
      name = "crossfilter-block-js",
      version = utils::packageVersion("blockr.dm"),
      src = system.file("js", package = "blockr.dm"),
      script = "crossfilter-block.js"
    ),
    htmltools::htmlDependency(
      name = "crossfilter-block-css",
      version = utils::packageVersion("blockr.dm"),
      src = system.file("css", package = "blockr.dm"),
      stylesheet = "crossfilter-block.css"
    )
  )
}
