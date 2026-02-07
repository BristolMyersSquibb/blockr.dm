# --- Crossfilter Backend Implementations ---
#
# Three interchangeable backends for the dm crossfilter hot path:
#   1. dplyr     — pure dplyr, always available (default fallback)
#   2. duckdb    — raw SQL via DBI/duckdb (fastest, ~2.6x vs dplyr)
#   3. duckplyr  — dplyr verbs on duckdb tibbles (for comparison/debugging)
#
# Each backend exports three functions:
#   *_crossfilter_data()   — filtered data for a table (exclude-own-dim pattern)
#   *_crossfilter_agg()    — aggregated counts for a categorical dimension
#   *_crossfilter_counts() — row counts per table after all filters


# ============================================================================
# Shared helpers
# ============================================================================

#' Apply categorical + range filters to a data frame (dplyr-compatible)
#'
#' Uses `.data[[dim]]` pronoun — works with standard dplyr.
#' @param df A data frame
#' @param cat_f Named list of character vectors (categorical filters)
#' @param rng_f Named list of numeric(2) vectors (range filters)
#' @param exclude_dim Optional dimension name to skip
#' @return Filtered data frame
#' @keywords internal
apply_crossfilter_filters <- function(df, cat_f, rng_f, exclude_dim = NULL) {
  if (!is.null(exclude_dim)) {
    cat_f[[exclude_dim]] <- NULL
    rng_f[[exclude_dim]] <- NULL
  }
  for (dim in names(cat_f)) {
    val <- cat_f[[dim]]
    if (!is.null(val) && length(val) > 0 && dim %in% names(df)) {
      df <- dplyr::filter(df, .data[[dim]] %in% val)
    }
  }
  for (dim in names(rng_f)) {
    rng <- rng_f[[dim]]
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

#' Apply categorical + range filters (duckplyr-compatible)
#'
#' Uses `!!rlang::sym(dim)` instead of `.data[[dim]]` and avoids `between()`
#' to prevent silent fallback to dplyr in duckplyr.
#' Avoids `df[[dim]]` column extraction which would materialize the lazy table.
#' @inheritParams apply_crossfilter_filters
#' @param col_classes Named character vector mapping column names to their class
#'   (e.g. "Date", "numeric"). Used instead of `df[[dim]]` to avoid
#'   materializing duckplyr lazy tables. If NULL, range filters are treated
#'   as numeric.
#' @keywords internal
apply_crossfilter_filters_sym <- function(df, cat_f, rng_f, exclude_dim = NULL,
                                          col_classes = NULL) {
  if (!is.null(exclude_dim)) {
    cat_f[[exclude_dim]] <- NULL
    rng_f[[exclude_dim]] <- NULL
  }
  for (dim in names(cat_f)) {
    val <- cat_f[[dim]]
    if (!is.null(val) && length(val) > 0 && dim %in% names(df)) {
      df <- dplyr::filter(df, !!rlang::sym(dim) %in% val)
    }
  }
  for (dim in names(rng_f)) {
    rng <- rng_f[[dim]]
    if (!is.null(rng) && length(rng) == 2 && dim %in% names(df)) {
      is_date <- !is.null(col_classes) &&
        !is.null(col_classes[[dim]]) &&
        col_classes[[dim]] %in% c("Date", "POSIXct", "POSIXlt")
      if (is_date) {
        rng_lo <- as.Date(rng[1], origin = "1970-01-01")
        rng_hi <- as.Date(rng[2], origin = "1970-01-01")
        df <- dplyr::filter(df, !!rlang::sym(dim) >= rng_lo & !!rlang::sym(dim) <= rng_hi)
      } else {
        lo <- rng[1]
        hi <- rng[2]
        df <- dplyr::filter(df, !!rlang::sym(dim) >= lo & !!rlang::sym(dim) <= hi)
      }
    }
  }
  df
}


# ============================================================================
# Backend 1: dplyr (extracts current inline logic)
# ============================================================================

#' Compute crossfilter-filtered data for a table (dplyr backend)
#'
#' @param tables Named list of data frames
#' @param key_col_fn Function: tbl_name -> key column name (or NULL)
#' @param tbl_name Target table name
#' @param exclude_dim Dimension to exclude from own-table filters
#' @param cat_filters Named list of per-table categorical filters
#' @param rng_filters Named list of per-table range filters
#' @return Filtered data frame
#' @keywords internal
dplyr_crossfilter_data <- function(tables, key_col_fn, tbl_name, exclude_dim,
                                   cat_filters, rng_filters) {
  df <- tables[[tbl_name]]
  key_col <- key_col_fn(tbl_name)

  # Own table's key set (excluding the target dimension)
  cat_f <- cat_filters[[tbl_name]] %||% list()
  rng_f <- rng_filters[[tbl_name]] %||% list()
  own_df <- apply_crossfilter_filters(df, cat_f, rng_f, exclude_dim = exclude_dim)
  own_keys <- if (!is.null(key_col)) unique(own_df[[key_col]]) else NULL

  # Intersect with all other tables' key sets (full filters)
  for (other_tbl in names(tables)) {
    if (other_tbl == tbl_name) next
    other_key_col <- key_col_fn(other_tbl)
    if (is.null(other_key_col)) next
    other_cat <- cat_filters[[other_tbl]] %||% list()
    other_rng <- rng_filters[[other_tbl]] %||% list()
    other_df <- apply_crossfilter_filters(tables[[other_tbl]], other_cat, other_rng)
    other_keys <- unique(other_df[[other_key_col]])
    if (!is.null(own_keys) && !is.null(other_keys)) {
      own_keys <- intersect(own_keys, other_keys)
    }
  }

  # Filter to allowed keys
  if (!is.null(key_col) && !is.null(own_keys)) {
    df <- dplyr::filter(df, .data[[key_col]] %in% own_keys)
  }

  # Apply own-table filters (excluding the target dimension)
  apply_crossfilter_filters(df, cat_f, rng_f, exclude_dim = exclude_dim)
}

#' Compute aggregated counts for a categorical dimension (dplyr backend)
#'
#' Pushes aggregation down: filters -> group_by -> count, avoiding full
#' materialization of the filtered table.
#' @inheritParams dplyr_crossfilter_data
#' @param dim The categorical dimension column name
#' @return Data frame with columns: dim, .count
#' @keywords internal
dplyr_crossfilter_agg <- function(tables, key_col_fn, tbl_name, dim,
                                  cat_filters, rng_filters) {
  df <- dplyr_crossfilter_data(tables, key_col_fn, tbl_name, dim,
                               cat_filters, rng_filters)
  if (nrow(df) == 0) return(data.frame(x = character(0), .count = integer(0),
                                        stringsAsFactors = FALSE))
  agg <- dplyr::summarise(df, .count = dplyr::n(),
                           .by = dplyr::all_of(dim))
  agg <- dplyr::mutate(agg, !!dim := as.character(.data[[dim]]))
  dplyr::arrange(agg, dplyr::desc(.data[[".count"]]))
}

#' Compute filtered row counts per table (dplyr backend)
#'
#' @inheritParams dplyr_crossfilter_data
#' @param table_names Character vector of table names to count
#' @return List with total, filtered, n_tables
#' @keywords internal
dplyr_crossfilter_counts <- function(tables, key_col_fn, table_names,
                                     cat_filters, rng_filters) {
  total <- 0L
  filtered <- 0L
  for (tbl_name in table_names) {
    df <- tables[[tbl_name]]
    total <- total + nrow(df)

    key_col <- key_col_fn(tbl_name)
    cat_f <- cat_filters[[tbl_name]] %||% list()
    rng_f <- rng_filters[[tbl_name]] %||% list()

    result_df <- apply_crossfilter_filters(df, cat_f, rng_f)

    if (!is.null(key_col)) {
      own_keys <- unique(result_df[[key_col]])
      for (other_tbl in table_names) {
        if (other_tbl == tbl_name) next
        other_key_col <- key_col_fn(other_tbl)
        if (is.null(other_key_col)) next
        other_cat <- cat_filters[[other_tbl]] %||% list()
        other_rng <- rng_filters[[other_tbl]] %||% list()
        other_df <- apply_crossfilter_filters(tables[[other_tbl]], other_cat, other_rng)
        other_keys <- unique(other_df[[other_key_col]])
        if (!is.null(other_keys)) {
          own_keys <- intersect(own_keys, other_keys)
        }
      }
      result_df <- dplyr::filter(result_df, .data[[key_col]] %in% own_keys)
    }

    filtered <- filtered + nrow(result_df)
  }
  list(total = total, filtered = filtered, n_tables = length(table_names))
}


# ============================================================================
# Backend 2: DuckDB raw SQL (fastest — single query string per operation)
# ============================================================================

#' Build WHERE clause fragments for crossfilter SQL
#'
#' @param tbl_name Table name to build clauses for
#' @param cat_filters Named list of per-table categorical filters
#' @param rng_filters Named list of per-table range filters
#' @param exclude_dim Optional dimension to skip
#' @param tables Named list of data frames (used for date column detection)
#' @return Character vector of WHERE clause fragments (without WHERE keyword)
#' @keywords internal
build_crossfilter_where <- function(tbl_name, cat_filters, rng_filters,
                                    exclude_dim = NULL, tables = NULL) {
  clauses <- character()

  cat_f <- cat_filters[[tbl_name]] %||% list()
  rng_f <- rng_filters[[tbl_name]] %||% list()

  if (!is.null(exclude_dim)) {
    cat_f[[exclude_dim]] <- NULL
    rng_f[[exclude_dim]] <- NULL
  }

  for (dim in names(cat_f)) {
    vals <- cat_f[[dim]]
    if (!is.null(vals) && length(vals) > 0) {
      escaped <- gsub("'", "''", vals)
      in_list <- paste0("'", escaped, "'", collapse = ", ")
      clauses <- c(clauses, sprintf('"%s" IN (%s)', dim, in_list))
    }
  }

  for (dim in names(rng_f)) {
    rng <- rng_f[[dim]]
    if (!is.null(rng) && length(rng) == 2) {
      is_date <- FALSE
      if (!is.null(tables) && !is.null(tables[[tbl_name]])) {
        col <- tables[[tbl_name]][[dim]]
        is_date <- inherits(col, c("Date", "POSIXct", "POSIXlt"))
      }
      if (is_date) {
        lo <- as.character(as.Date(rng[1], origin = "1970-01-01"))
        hi <- as.character(as.Date(rng[2], origin = "1970-01-01"))
        clauses <- c(clauses, sprintf('"%s" BETWEEN DATE \'%s\' AND DATE \'%s\'',
                                       dim, lo, hi))
      } else {
        clauses <- c(clauses, sprintf('"%s" BETWEEN %s AND %s',
                                       dim, rng[1], rng[2]))
      }
    }
  }

  clauses
}

#' Build a full WHERE string from clause fragments
#' @param clauses Character vector of WHERE clause fragments
#' @return Single WHERE string (empty string if no clauses)
#' @keywords internal
where_string <- function(clauses) {
  if (length(clauses) == 0) return("")
  paste0(" WHERE ", paste(clauses, collapse = " AND "))
}

#' Compute crossfilter-filtered data for a table (DuckDB SQL backend)
#'
#' Sends a single SQL query: SELECT * FROM tbl WHERE ... AND key IN (SELECT DISTINCT ...)
#' @param con DBI connection to DuckDB
#' @param tables Named list of data frames (for date detection in WHERE builder)
#' @param table_names Character vector of all table names
#' @param key_col_fn Function: tbl_name -> key column name (or NULL)
#' @param tbl_name Target table name
#' @param exclude_dim Dimension to exclude from own-table filters
#' @param cat_filters Named list of per-table categorical filters
#' @param rng_filters Named list of per-table range filters
#' @return Filtered data frame
#' @keywords internal
duckdb_crossfilter_data <- function(con, tables, table_names, key_col_fn,
                                    tbl_name, exclude_dim,
                                    cat_filters, rng_filters) {
  key_col <- key_col_fn(tbl_name)
  own_where <- build_crossfilter_where(tbl_name, cat_filters, rng_filters,
                                        exclude_dim, tables)

  # Build subqueries for other tables' key sets
  key_subqueries <- character()
  for (other_tbl in table_names) {
    if (other_tbl == tbl_name) next
    other_key <- key_col_fn(other_tbl)
    if (is.null(other_key)) next
    other_clauses <- build_crossfilter_where(other_tbl, cat_filters, rng_filters,
                                              tables = tables)
    key_subqueries <- c(key_subqueries,
      sprintf('SELECT DISTINCT "%s" FROM "%s"%s',
              other_key, other_tbl, where_string(other_clauses)))
  }

  # Combine: own WHERE + key IN (intersect of other tables)
  all_clauses <- own_where
  if (!is.null(key_col) && length(key_subqueries) > 0) {
    for (sq in key_subqueries) {
      all_clauses <- c(all_clauses, sprintf('"%s" IN (%s)', key_col, sq))
    }
  }

  sql <- sprintf('SELECT * FROM "%s"%s', tbl_name, where_string(all_clauses))
  DBI::dbGetQuery(con, sql)
}

#' Compute aggregated counts for a categorical dimension (DuckDB SQL backend)
#'
#' Pushes aggregation into SQL: SELECT dim, COUNT(*) ... GROUP BY dim
#' @inheritParams duckdb_crossfilter_data
#' @param dim The categorical dimension column name
#' @return Data frame with columns: dim, .count
#' @keywords internal
duckdb_crossfilter_agg <- function(con, tables, table_names, key_col_fn,
                                   tbl_name, dim,
                                   cat_filters, rng_filters) {
  key_col <- key_col_fn(tbl_name)
  own_where <- build_crossfilter_where(tbl_name, cat_filters, rng_filters,
                                        exclude_dim = dim, tables = tables)

  # Build subqueries for other tables' key sets
  key_subqueries <- character()
  for (other_tbl in table_names) {
    if (other_tbl == tbl_name) next
    other_key <- key_col_fn(other_tbl)
    if (is.null(other_key)) next
    other_clauses <- build_crossfilter_where(other_tbl, cat_filters, rng_filters,
                                              tables = tables)
    key_subqueries <- c(key_subqueries,
      sprintf('SELECT DISTINCT "%s" FROM "%s"%s',
              other_key, other_tbl, where_string(other_clauses)))
  }

  all_clauses <- own_where
  if (!is.null(key_col) && length(key_subqueries) > 0) {
    for (sq in key_subqueries) {
      all_clauses <- c(all_clauses, sprintf('"%s" IN (%s)', key_col, sq))
    }
  }

  sql <- sprintf(
    'SELECT CAST("%s" AS VARCHAR) AS "%s", COUNT(*) AS ".count" FROM "%s"%s GROUP BY "%s" ORDER BY ".count" DESC',
    dim, dim, tbl_name, where_string(all_clauses), dim
  )
  result <- DBI::dbGetQuery(con, sql)
  # Ensure integer count column
  result[[".count"]] <- as.integer(result[[".count"]])
  result
}

#' Compute filtered row counts per table (DuckDB SQL backend)
#'
#' Uses COUNT(*) queries instead of materializing full tables.
#' @inheritParams duckdb_crossfilter_data
#' @return List with total, filtered, n_tables
#' @keywords internal
duckdb_crossfilter_counts <- function(con, tables, table_names, key_col_fn,
                                      cat_filters, rng_filters) {
  total <- 0L
  filtered <- 0L

  for (tbl_name in table_names) {
    # Total count
    total_sql <- sprintf('SELECT COUNT(*) FROM "%s"', tbl_name)
    total <- total + DBI::dbGetQuery(con, total_sql)[[1]]

    # Filtered count
    key_col <- key_col_fn(tbl_name)
    own_clauses <- build_crossfilter_where(tbl_name, cat_filters, rng_filters,
                                            tables = tables)

    key_subqueries <- character()
    for (other_tbl in table_names) {
      if (other_tbl == tbl_name) next
      other_key <- key_col_fn(other_tbl)
      if (is.null(other_key)) next
      other_clauses <- build_crossfilter_where(other_tbl, cat_filters, rng_filters,
                                                tables = tables)
      key_subqueries <- c(key_subqueries,
        sprintf('SELECT DISTINCT "%s" FROM "%s"%s',
                other_key, other_tbl, where_string(other_clauses)))
    }

    all_clauses <- own_clauses
    if (!is.null(key_col) && length(key_subqueries) > 0) {
      for (sq in key_subqueries) {
        all_clauses <- c(all_clauses, sprintf('"%s" IN (%s)', key_col, sq))
      }
    }

    count_sql <- sprintf('SELECT COUNT(*) FROM "%s"%s',
                          tbl_name, where_string(all_clauses))
    filtered <- filtered + DBI::dbGetQuery(con, count_sql)[[1]]
  }

  list(total = total, filtered = filtered, n_tables = length(table_names))
}


# ============================================================================
# Backend 3: duckplyr (dplyr verbs on DuckDB tibbles)
# ============================================================================

#' Build a lazy crossfilter query for a table (duckplyr internal)
#'
#' Returns a lazy duckplyr tibble with all crossfilter logic applied but
#' NOT materialized. Callers decide when to collect.
#' @param duck_tables Named list of duckplyr data frames
#' @param col_classes Named list of per-table column class vectors
#' @param key_col_fn Function: tbl_name -> key column name (or NULL)
#' @param tbl_name Target table name
#' @param exclude_dim Dimension to exclude from own-table filters
#' @param cat_filters Named list of per-table categorical filters
#' @param rng_filters Named list of per-table range filters
#' @return Lazy duckplyr tibble (not yet materialized)
#' @keywords internal
duckplyr_crossfilter_lazy <- function(duck_tables, col_classes, key_col_fn,
                                      tbl_name, exclude_dim,
                                      cat_filters, rng_filters) {
  df <- duck_tables[[tbl_name]]
  key_col <- key_col_fn(tbl_name)

  cat_f <- cat_filters[[tbl_name]] %||% list()
  rng_f <- rng_filters[[tbl_name]] %||% list()
  own_filtered <- apply_crossfilter_filters_sym(
    df, cat_f, rng_f, exclude_dim = exclude_dim,
    col_classes = col_classes[[tbl_name]]
  )

  # Collect key sets from other tables, intersect in R, semi_join once.
  # Faster than chaining 7 semi_joins (DuckDB planner overhead on complex plans).
  # Uses semi_join with as_duckdb_tibble instead of %in% (>100 values fallback).
  if (!is.null(key_col)) {
    allowed_keys <- NULL
    for (other_tbl in names(duck_tables)) {
      if (other_tbl == tbl_name) next
      other_key <- key_col_fn(other_tbl)
      if (is.null(other_key)) next
      other_cat <- cat_filters[[other_tbl]] %||% list()
      other_rng <- rng_filters[[other_tbl]] %||% list()
      other_filtered <- apply_crossfilter_filters_sym(
        duck_tables[[other_tbl]], other_cat, other_rng,
        col_classes = col_classes[[other_tbl]]
      )
      other_key_vals <- dplyr::pull(
        dplyr::distinct(other_filtered, !!rlang::sym(other_key))
      )
      if (is.null(allowed_keys)) {
        allowed_keys <- other_key_vals
      } else {
        allowed_keys <- intersect(allowed_keys, other_key_vals)
      }
    }
    if (!is.null(allowed_keys)) {
      keys_tbl <- duckplyr::as_duckdb_tibble(
        stats::setNames(data.frame(allowed_keys, stringsAsFactors = FALSE),
                         key_col)
      )
      own_filtered <- dplyr::semi_join(own_filtered, keys_tbl, by = key_col)
    }
  }

  own_filtered
}

#' Compute crossfilter-filtered data for a table (duckplyr backend)
#'
#' @param duck_tables Named list of duckplyr data frames
#' @param col_classes Named list of per-table column class vectors
#' @param key_col_fn Function: tbl_name -> key column name (or NULL)
#' @param tbl_name Target table name
#' @param exclude_dim Dimension to exclude from own-table filters
#' @param cat_filters Named list of per-table categorical filters
#' @param rng_filters Named list of per-table range filters
#' @return Filtered data frame (materialized)
#' @keywords internal
duckplyr_crossfilter_data <- function(duck_tables, col_classes, key_col_fn,
                                      tbl_name, exclude_dim,
                                      cat_filters, rng_filters) {
  lazy <- duckplyr_crossfilter_lazy(duck_tables, col_classes, key_col_fn,
                                     tbl_name, exclude_dim,
                                     cat_filters, rng_filters)
  as.data.frame(lazy)
}

#' Compute aggregated counts for a categorical dimension (duckplyr backend)
#'
#' Keeps the query lazy through the GROUP BY, only materializes the small
#' aggregated result.
#' @inheritParams duckplyr_crossfilter_data
#' @param dim The categorical dimension column name
#' @return Data frame with columns: dim, .count
#' @keywords internal
duckplyr_crossfilter_agg <- function(duck_tables, col_classes, key_col_fn,
                                     tbl_name, dim,
                                     cat_filters, rng_filters) {
  lazy <- duckplyr_crossfilter_lazy(duck_tables, col_classes, key_col_fn,
                                     tbl_name, dim,
                                     cat_filters, rng_filters)
  # Use .by (not group_by) — duckplyr requires .by to avoid fallback
  # Cast to character AFTER materialization — as.character() causes fallback
  agg <- lazy |>
    dplyr::summarise(.count = dplyr::n(), .by = dplyr::all_of(dim)) |>
    dplyr::arrange(dplyr::desc(!!rlang::sym(".count")))
  agg <- as.data.frame(agg)
  agg[[dim]] <- as.character(agg[[dim]])
  agg
}

#' Compute filtered row counts per table (duckplyr backend)
#'
#' Computes filtered key sets per table once, intersects in R (cheap on small
#' key vectors), then counts per table with a single filter. Avoids rebuilding
#' 7 semi_join chains per table.
#' @inheritParams duckplyr_crossfilter_data
#' @param table_names Character vector of table names to count
#' @return List with total, filtered, n_tables
#' @keywords internal
duckplyr_crossfilter_counts <- function(duck_tables, col_classes, key_col_fn,
                                        table_names,
                                        cat_filters, rng_filters) {
  total <- 0L
  filtered <- 0L

  # Step 1: compute filtered key set per table (one query each)
  key_sets <- list()
  for (tbl_name in table_names) {
    total <- total + nrow(duck_tables[[tbl_name]])
    key_col <- key_col_fn(tbl_name)
    if (is.null(key_col)) next
    cat_f <- cat_filters[[tbl_name]] %||% list()
    rng_f <- rng_filters[[tbl_name]] %||% list()
    filt <- apply_crossfilter_filters_sym(duck_tables[[tbl_name]], cat_f, rng_f,
                                          col_classes = col_classes[[tbl_name]])
    key_sets[[tbl_name]] <- dplyr::pull(dplyr::distinct(filt, !!rlang::sym(key_col)))
  }

  # Step 2: intersect all key sets in R (fast — small vectors)
  all_keys <- Reduce(intersect, key_sets)

  # Step 3: create keys duckplyr tibble ONCE (as_duckdb_tibble outside the loop)
  keys_tbl <- NULL
  if (!is.null(all_keys)) {
    keys_tbl <- duckplyr::as_duckdb_tibble(
      data.frame(.key = all_keys, stringsAsFactors = FALSE)
    )
  }

  # Step 4: count per table with the global key set
  for (tbl_name in table_names) {
    key_col <- key_col_fn(tbl_name)
    cat_f <- cat_filters[[tbl_name]] %||% list()
    rng_f <- rng_filters[[tbl_name]] %||% list()
    filt <- apply_crossfilter_filters_sym(duck_tables[[tbl_name]], cat_f, rng_f,
                                          col_classes = col_classes[[tbl_name]])
    if (!is.null(key_col) && !is.null(keys_tbl)) {
      renamed_keys <- dplyr::rename(keys_tbl, !!key_col := ".key")
      filt <- dplyr::semi_join(filt, renamed_keys, by = key_col)
    }
    count_df <- dplyr::summarise(filt, n = dplyr::n())
    filtered <- filtered + as.data.frame(count_df)$n
  }
  list(total = total, filtered = filtered, n_tables = length(table_names))
}

#' Build column class map from a list of data frames
#'
#' Pre-computes the class of each column so that duckplyr filters can detect
#' Date columns without materializing the lazy tibble via `df[[dim]]`.
#' @param tables Named list of data frames
#' @return Named list of named character vectors
#' @keywords internal
build_col_classes <- function(tables) {
  lapply(tables, function(df) {
    vapply(df, function(col) class(col)[1], character(1))
  })
}
