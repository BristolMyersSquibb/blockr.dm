# --- Crossfilter Backend Implementations ---

# Sentinel value representing R's NA in the crossfilter pipeline.
# JS has no R-style NA, and `%in%` can't match NA — this sentinel flows through
# Shiny.setInputValue and back, letting us distinguish real NA from literal "NA".
CROSSFILTER_NA <- "__NA__"

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
      has_na <- CROSSFILTER_NA %in% val
      non_na_vals <- setdiff(val, CROSSFILTER_NA)
      if (has_na && length(non_na_vals) > 0) {
        df <- dplyr::filter(df, is.na(.data[[dim]]) | .data[[dim]] %in% non_na_vals)
      } else if (has_na) {
        df <- dplyr::filter(df, is.na(.data[[dim]]))
      } else {
        df <- dplyr::filter(df, .data[[dim]] %in% non_na_vals)
      }
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
      has_na <- CROSSFILTER_NA %in% val
      non_na_vals <- setdiff(val, CROSSFILTER_NA)
      if (has_na && length(non_na_vals) > 0) {
        df <- dplyr::filter(df, is.na(!!rlang::sym(dim)) | !!rlang::sym(dim) %in% non_na_vals)
      } else if (has_na) {
        df <- dplyr::filter(df, is.na(!!rlang::sym(dim)))
      } else {
        df <- dplyr::filter(df, !!rlang::sym(dim) %in% non_na_vals)
      }
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
  agg[[dim]][is.na(agg[[dim]])] <- CROSSFILTER_NA
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

# ============================================================================
# Backend 4: dm (dm::dm_filter for FK-aware filter propagation)
# ============================================================================

#' Build per-table filter expressions for dm::dm_filter
#'
#' Constructs a named list of rlang quosures suitable for splicing into
#' `dm::dm_filter()`. Each element is a single combined expression for one table.
#' @param cat_filters Named list of per-table categorical filters
#' @param rng_filters Named list of per-table range filters
#' @param tables Named list of data frames (for date column detection)
#' @param exclude_tbl Table name whose `exclude_dim` should be skipped
#' @param exclude_dim Dimension to exclude from `exclude_tbl`'s conditions
#' @return Named list of expressions (one per table that has active filters)
#' @keywords internal
build_dm_filter_exprs <- function(cat_filters, rng_filters, tables,
                                  exclude_tbl = NULL, exclude_dim = NULL) {
  all_tables <- unique(c(names(cat_filters), names(rng_filters)))
  result <- list()

  for (tbl in all_tables) {
    cat_f <- cat_filters[[tbl]] %||% list()
    rng_f <- rng_filters[[tbl]] %||% list()

    # Exclude the target dimension from the target table
    if (!is.null(exclude_tbl) && tbl == exclude_tbl && !is.null(exclude_dim)) {
      cat_f[[exclude_dim]] <- NULL
      rng_f[[exclude_dim]] <- NULL
    }

    conditions <- list()

    for (dim in names(cat_f)) {
      val <- cat_f[[dim]]
      if (!is.null(val) && length(val) > 0) {
        has_na <- CROSSFILTER_NA %in% val
        non_na_vals <- setdiff(val, CROSSFILTER_NA)
        if (has_na && length(non_na_vals) > 0) {
          conditions <- c(conditions, list(
            rlang::expr(is.na(!!rlang::sym(dim)) | !!rlang::sym(dim) %in% !!non_na_vals)
          ))
        } else if (has_na) {
          conditions <- c(conditions, list(rlang::expr(is.na(!!rlang::sym(dim)))))
        } else {
          conditions <- c(conditions, list(
            rlang::expr(!!rlang::sym(dim) %in% !!non_na_vals)
          ))
        }
      }
    }

    for (dim in names(rng_f)) {
      rng <- rng_f[[dim]]
      if (!is.null(rng) && length(rng) == 2) {
        is_date <- FALSE
        if (!is.null(tables) && !is.null(tables[[tbl]])) {
          col <- tables[[tbl]][[dim]]
          is_date <- inherits(col, c("Date", "POSIXct", "POSIXlt"))
        }
        if (is_date) {
          rng_lo <- as.Date(rng[1], origin = "1970-01-01")
          rng_hi <- as.Date(rng[2], origin = "1970-01-01")
          conditions <- c(conditions, list(
            rlang::expr(!!rlang::sym(dim) >= !!rng_lo & !!rlang::sym(dim) <= !!rng_hi)
          ))
        } else {
          lo <- rng[1]
          hi <- rng[2]
          conditions <- c(conditions, list(
            rlang::expr(!!rlang::sym(dim) >= !!lo & !!rlang::sym(dim) <= !!hi)
          ))
        }
      }
    }

    if (length(conditions) > 0) {
      if (length(conditions) == 1) {
        result[[tbl]] <- conditions[[1]]
      } else {
        result[[tbl]] <- Reduce(function(a, b) rlang::expr(!!a & !!b), conditions)
      }
    }
  }
  result
}

#' Compute crossfilter-filtered data for a table (dm backend)
#'
#' Uses `dm::dm_filter()` to propagate filters via FK relationships,
#' then extracts and returns the target table.
#' @param dm_obj A dm object
#' @param tbl_name Target table name
#' @param exclude_dim Dimension to exclude from own-table filters
#' @param cat_filters Named list of per-table categorical filters
#' @param rng_filters Named list of per-table range filters
#' @return Filtered data frame
#' @keywords internal
dm_crossfilter_data <- function(dm_obj, tbl_name, exclude_dim,
                                cat_filters, rng_filters) {
  tables <- stats::setNames(
    lapply(names(dm::dm_get_tables(dm_obj)), function(nm) dm_obj[[nm]]),
    names(dm::dm_get_tables(dm_obj))
  )
  filter_exprs <- build_dm_filter_exprs(
    cat_filters, rng_filters, tables,
    exclude_tbl = tbl_name, exclude_dim = exclude_dim
  )

  if (length(filter_exprs) > 0) {
    dm_call <- rlang::expr(dm::dm_filter(dm_obj, !!!filter_exprs))
    dm_filtered <- rlang::eval_tidy(dm_call)
  } else {
    dm_filtered <- dm_obj
  }

  dplyr::collect(dm_filtered[[tbl_name]])
}

#' Compute aggregated counts for a categorical dimension (dm backend)
#'
#' @inheritParams dm_crossfilter_data
#' @param dim The categorical dimension column name
#' @return Data frame with columns: dim, .count
#' @keywords internal
dm_crossfilter_agg <- function(dm_obj, tbl_name, dim,
                               cat_filters, rng_filters) {
  df <- dm_crossfilter_data(dm_obj, tbl_name, dim, cat_filters, rng_filters)
  if (nrow(df) == 0) {
    return(data.frame(x = character(0), .count = integer(0),
                      stringsAsFactors = FALSE))
  }
  agg <- dplyr::summarise(df, .count = dplyr::n(),
                           .by = dplyr::all_of(dim))
  agg <- dplyr::mutate(agg, !!dim := as.character(.data[[dim]]))
  dplyr::arrange(agg, dplyr::desc(.data[[".count"]]))
}

#' Compute filtered row counts per table (dm backend)
#'
#' @inheritParams dm_crossfilter_data
#' @param table_names Character vector of table names to count
#' @return List with total, filtered, n_tables
#' @keywords internal
dm_crossfilter_counts <- function(dm_obj, table_names,
                                  cat_filters, rng_filters) {
  tables <- stats::setNames(
    lapply(table_names, function(nm) dm_obj[[nm]]),
    table_names
  )
  filter_exprs <- build_dm_filter_exprs(cat_filters, rng_filters, tables)

  if (length(filter_exprs) > 0) {
    dm_call <- rlang::expr(dm::dm_filter(dm_obj, !!!filter_exprs))
    dm_filtered <- rlang::eval_tidy(dm_call)
  } else {
    dm_filtered <- dm_obj
  }

  total <- 0L
  filtered <- 0L
  for (tbl_name in table_names) {
    total <- total + nrow(tables[[tbl_name]])
    filtered <- filtered + nrow(dplyr::collect(dm_filtered[[tbl_name]]))
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


# ============================================================================
# Backend 5: Lookup (precomputed per-child join tables)
# ============================================================================

#' Flatten per-table filters to column-level for lookup operations
#'
#' Converts nested per-table filter lists (e.g., `list(adsl = list(SEX = "F"))`)
#' to flat column-level lists (e.g., `list(SEX = "F")`), keeping only columns
#' present in the lookup table.
#'
#' @param cat_filters Named list of per-table categorical filters
#' @param rng_filters Named list of per-table range filters
#' @param lookup_columns Character vector of column names in the lookup table
#' @return List with `cat` (flat categorical) and `rng` (flat range) filter lists
#' @keywords internal
flatten_filters_for_lookup <- function(cat_filters, rng_filters, lookup_columns) {
  flat_cat <- list()
  flat_rng <- list()
  for (tbl in names(cat_filters)) {
    for (dim in names(cat_filters[[tbl]])) {
      if (dim %in% lookup_columns) {
        flat_cat[[dim]] <- cat_filters[[tbl]][[dim]]
      }
    }
  }
  for (tbl in names(rng_filters)) {
    for (dim in names(rng_filters[[tbl]])) {
      if (dim %in% lookup_columns) {
        flat_rng[[dim]] <- rng_filters[[tbl]][[dim]]
      }
    }
  }
  list(cat = flat_cat, rng = flat_rng)
}

#' Build precomputed lookup tables for crossfilter operations
#'
#' For a star-schema dm (one parent table with PK, N child tables with FKs),
#' precomputes one flat lookup per child by joining each child with the parent.
#' This eliminates O(N x T) semi-joins per filter interaction --- only cheap
#' column filtering on flat tables remains.
#'
#' @param tables Named list of data frames
#' @param active_dims Named list: tbl_name -> character vector of active dim columns
#' @param pks PK tibble from dm::dm_get_all_pks()
#' @param fks FK tibble from dm::dm_get_all_fks()
#' @param measure_col Optional measure spec (".count" or "table.column")
#' @return List with lookups, dim_source, parent_key, child_fk_cols,
#'   parent_table, child_tables; or NULL if topology doesn't support lookup
#' @keywords internal
build_crossfilter_lookups <- function(tables, active_dims, pks, fks,
                                       measure_col = NULL) {
  if (nrow(fks) == 0 || nrow(pks) == 0) return(NULL)

  # No active dims means no filtering needed
  has_any_dims <- any(vapply(active_dims, length, integer(1)) > 0)
  if (!has_any_dims) return(NULL)

  # Identify parent (PK table) and children (FK tables)
  parent_table <- pks$table[1]
  parent_key <- pks$pk_col[[1]][[1]]
  parent_df <- tables[[parent_table]]
  if (is.null(parent_df)) return(NULL)

  child_fk_rows <- fks[fks$parent_table == parent_table, ]
  if (nrow(child_fk_rows) == 0) return(NULL)

  # Parse measure spec using table-name-aware matching
  measure_tbl <- NULL
  measure_cn <- NULL
  if (!is.null(measure_col) && measure_col != ".count") {
    for (tbl in names(tables)) {
      prefix <- paste0(tbl, ".")
      if (startsWith(measure_col, prefix)) {
        measure_tbl <- tbl
        measure_cn <- substr(measure_col, nchar(prefix) + 1, nchar(measure_col))
        break
      }
    }
  }

  parent_dims <- active_dims[[parent_table]] %||% character()

  lookups <- list()
  dim_source <- list()
  child_fk_col_map <- character()

  for (d in parent_dims) dim_source[[d]] <- parent_table

  for (i in seq_len(nrow(child_fk_rows))) {
    child_tbl <- child_fk_rows$child_table[i]
    child_fk_col <- child_fk_rows$child_fk_cols[[i]][[1]]
    child_df <- tables[[child_tbl]]
    if (is.null(child_df)) next

    child_dims <- active_dims[[child_tbl]] %||% character()
    for (d in child_dims) dim_source[[d]] <- child_tbl
    child_fk_col_map[[child_tbl]] <- child_fk_col

    # Columns from child: FK + child dims + measure (if applicable)
    child_select <- unique(c(child_fk_col, child_dims))
    if (!is.null(measure_tbl) && measure_tbl == child_tbl && !is.null(measure_cn)) {
      child_select <- unique(c(child_select, measure_cn))
    }
    child_select <- intersect(child_select, names(child_df))

    # Columns from parent: PK + parent dims + measure (if applicable)
    parent_select <- unique(c(parent_key, parent_dims))
    if (!is.null(measure_tbl) && measure_tbl == parent_table && !is.null(measure_cn)) {
      parent_select <- unique(c(parent_select, measure_cn))
    }
    parent_select <- intersect(parent_select, names(parent_df))

    # Remove conflicting non-key columns (same name in both tables)
    child_non_key <- setdiff(child_select, child_fk_col)
    parent_non_key <- setdiff(parent_select, parent_key)
    conflicts <- intersect(child_non_key, parent_non_key)
    if (length(conflicts) > 0) {
      parent_select <- setdiff(parent_select, conflicts)
    }

    child_sub <- child_df[, child_select, drop = FALSE]
    parent_sub <- unique(parent_df[, parent_select, drop = FALSE])

    by_spec <- stats::setNames(parent_key, child_fk_col)
    lookups[[child_tbl]] <- dplyr::left_join(child_sub, parent_sub, by = by_spec)
  }

  if (length(lookups) == 0) return(NULL)

  list(
    lookups = lookups,
    dim_source = dim_source,
    parent_key = parent_key,
    child_fk_cols = child_fk_col_map,
    parent_table = parent_table,
    child_tables = child_fk_rows$child_table
  )
}

#' Compute crossfilter-filtered data using precomputed lookups
#'
#' @param lookup_info Result from build_crossfilter_lookups()
#' @param tbl_name Target table name
#' @param exclude_dim Dimension to exclude from filtering (exclude-own pattern)
#' @param cat_filters Named list of per-table categorical filters
#' @param rng_filters Named list of per-table range filters
#' @return Filtered data frame from the lookup, or NULL on failure
#' @keywords internal
lookup_crossfilter_data <- function(lookup_info, tbl_name, exclude_dim,
                                     cat_filters, rng_filters) {
  lookups <- lookup_info$lookups
  child_fk_cols <- lookup_info$child_fk_cols
  child_tables <- lookup_info$child_tables
  parent_table <- lookup_info$parent_table

  # Choose primary lookup
  if (tbl_name %in% child_tables) {
    primary_child <- tbl_name
  } else if (tbl_name == parent_table && length(child_tables) > 0) {
    primary_child <- child_tables[1]
  } else {
    return(NULL)
  }

  primary_lookup <- lookups[[primary_child]]
  if (is.null(primary_lookup)) return(NULL)
  primary_key_col <- child_fk_cols[[primary_child]]

  # Flatten and apply filters (excluding target dim)
  flat <- flatten_filters_for_lookup(cat_filters, rng_filters,
                                      names(primary_lookup))
  filtered <- apply_crossfilter_filters(primary_lookup, flat$cat, flat$rng,
                                         exclude_dim = exclude_dim)

  # Sibling key intersection (also exclude own dim for correct crossfilter)
  for (other_child in child_tables) {
    if (other_child == primary_child) next
    other_lookup <- lookups[[other_child]]
    if (is.null(other_lookup)) next
    other_key_col <- child_fk_cols[[other_child]]

    other_flat <- flatten_filters_for_lookup(cat_filters, rng_filters,
                                              names(other_lookup))
    other_filtered <- apply_crossfilter_filters(other_lookup, other_flat$cat,
                                                 other_flat$rng,
                                                 exclude_dim = exclude_dim)
    other_keys <- unique(other_filtered[[other_key_col]])
    filtered <- dplyr::filter(filtered, .data[[primary_key_col]] %in% other_keys)
  }

  # For parent table requests, deduplicate to parent granularity
  if (tbl_name == parent_table) {
    filtered <- filtered[!duplicated(filtered[[primary_key_col]]), , drop = FALSE]
  }

  filtered
}

#' Compute aggregated counts for a categorical dimension using lookups
#'
#' Granularity-aware: parent dims use DISTINCT key counts (subject-level),
#' child dims use row counts (event-level).
#'
#' @inheritParams lookup_crossfilter_data
#' @param dim The categorical dimension column name
#' @return Data frame with columns: dim, .count
#' @keywords internal
lookup_crossfilter_agg <- function(lookup_info, tbl_name, dim,
                                    cat_filters, rng_filters) {
  lookups <- lookup_info$lookups
  dim_source <- lookup_info$dim_source
  child_fk_cols <- lookup_info$child_fk_cols
  child_tables <- lookup_info$child_tables
  parent_table <- lookup_info$parent_table

  source_table <- dim_source[[dim]]
  if (is.null(source_table)) return(NULL)
  dim_is_parent <- (source_table == parent_table)

  # Choose primary lookup
  if (!dim_is_parent && source_table %in% child_tables) {
    primary_child <- source_table
  } else if (length(child_tables) > 0) {
    primary_child <- child_tables[1]
  } else {
    return(NULL)
  }

  primary_lookup <- lookups[[primary_child]]
  if (is.null(primary_lookup)) return(NULL)
  if (!dim %in% names(primary_lookup)) return(NULL)
  primary_key_col <- child_fk_cols[[primary_child]]

  # Flatten and apply filters (excluding target dim)
  flat <- flatten_filters_for_lookup(cat_filters, rng_filters,
                                      names(primary_lookup))
  filtered <- apply_crossfilter_filters(primary_lookup, flat$cat, flat$rng,
                                         exclude_dim = dim)

  # Sibling key intersection (also exclude own dim for correct crossfilter)
  for (other_child in child_tables) {
    if (other_child == primary_child) next
    other_lookup <- lookups[[other_child]]
    if (is.null(other_lookup)) next
    other_key_col <- child_fk_cols[[other_child]]

    other_flat <- flatten_filters_for_lookup(cat_filters, rng_filters,
                                              names(other_lookup))
    other_filtered <- apply_crossfilter_filters(other_lookup, other_flat$cat,
                                                 other_flat$rng,
                                                 exclude_dim = dim)
    other_keys <- unique(other_filtered[[other_key_col]])
    filtered <- dplyr::filter(filtered, .data[[primary_key_col]] %in% other_keys)
  }

  if (nrow(filtered) == 0) {
    return(data.frame(x = character(0), .count = integer(0),
                      stringsAsFactors = FALSE))
  }

  # Granularity-aware counting
  if (dim_is_parent) {
    # Parent dim: count distinct keys per dim value (subject-level)
    distinct_rows <- unique(filtered[, c(primary_key_col, dim), drop = FALSE])
    agg <- dplyr::summarise(distinct_rows, .count = dplyr::n(),
                             .by = dplyr::all_of(dim))
  } else {
    # Child dim: count rows (event-level)
    agg <- dplyr::summarise(filtered, .count = dplyr::n(),
                             .by = dplyr::all_of(dim))
  }

  agg <- dplyr::mutate(agg, !!dim := as.character(.data[[dim]]))
  agg[[dim]][is.na(agg[[dim]])] <- CROSSFILTER_NA
  dplyr::arrange(agg, dplyr::desc(.data[[".count"]]))
}

#' Compute filtered row counts using precomputed lookups
#'
#' Computes the global allowed key set from all child lookups (with all filters
#' applied), then counts rows per original table.
#'
#' @param lookup_info Result from build_crossfilter_lookups()
#' @param tables Named list of original data frames
#' @param table_names Character vector of table names to count
#' @param cat_filters Named list of per-table categorical filters
#' @param rng_filters Named list of per-table range filters
#' @return List with total, filtered, n_tables
#' @keywords internal
lookup_crossfilter_counts <- function(lookup_info, tables, table_names,
                                       cat_filters, rng_filters) {
  lookups <- lookup_info$lookups
  child_fk_cols <- lookup_info$child_fk_cols
  parent_table <- lookup_info$parent_table
  child_tables <- lookup_info$child_tables
  parent_key <- lookup_info$parent_key

  # Step 1: compute filtered key set per child from lookups
  key_sets <- list()
  for (child in child_tables) {
    lookup <- lookups[[child]]
    if (is.null(lookup)) next
    key_col <- child_fk_cols[[child]]
    flat <- flatten_filters_for_lookup(cat_filters, rng_filters, names(lookup))
    filt <- apply_crossfilter_filters(lookup, flat$cat, flat$rng)
    key_sets[[child]] <- unique(filt[[key_col]])
  }

  # Step 2: intersect all key sets
  allowed_keys <- if (length(key_sets) > 0) Reduce(intersect, key_sets) else NULL

  # Step 3: count per original table
  total <- 0L
  filtered <- 0L

  for (tbl_name in table_names) {
    df <- tables[[tbl_name]]
    total <- total + nrow(df)

    # Determine key column for this table
    if (tbl_name == parent_table) {
      key_col <- parent_key
    } else if (tbl_name %in% child_tables) {
      key_col <- child_fk_cols[[tbl_name]]
    } else {
      key_col <- NULL
    }

    # Apply own-table filters
    cat_f <- cat_filters[[tbl_name]] %||% list()
    rng_f <- rng_filters[[tbl_name]] %||% list()
    result_df <- apply_crossfilter_filters(df, cat_f, rng_f)

    # Filter to allowed keys
    if (!is.null(key_col) && !is.null(allowed_keys)) {
      result_df <- dplyr::filter(result_df, .data[[key_col]] %in% allowed_keys)
    }

    filtered <- filtered + nrow(result_df)
  }

  list(total = total, filtered = filtered, n_tables = length(table_names))
}
