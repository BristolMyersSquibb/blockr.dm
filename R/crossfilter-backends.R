# --- Crossfilter Backend Implementations ---

# Sentinel value representing R's NA in the crossfilter pipeline.
# JS has no R-style NA, and `%in%` can't match NA -- this
# sentinel flows through Shiny.setInputValue and back, letting
# us distinguish real NA from literal "NA".
CROSSFILTER_NA <- "__NA__" # nolint object_name_linter
CROSSFILTER_EMPTY <- "__EMPTY__" # nolint object_name_linter

# Lookup-based crossfilter backend for the dm crossfilter hot path.
# Pre-joins each child table with the parent to create flat lookup tables,
# so filtering is just cheap column operations on pre-joined data.


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
      has_empty <- CROSSFILTER_EMPTY %in% val
      non_sentinel_vals <- setdiff(val, c(CROSSFILTER_NA, CROSSFILTER_EMPTY))
      # Build match vector: include empty string as literal for %in%
      match_vals <- non_sentinel_vals
      if (has_empty) match_vals <- c(match_vals, "")
      if (has_na && length(match_vals) > 0) {
        df <- dplyr::filter(
          df, is.na(.data[[dim]]) | .data[[dim]] %in% match_vals
        )
      } else if (has_na) {
        df <- dplyr::filter(df, is.na(.data[[dim]]))
      } else if (length(match_vals) > 0) {
        df <- dplyr::filter(df, .data[[dim]] %in% match_vals)
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


# ============================================================================
# Lookup backend (precomputed per-child join tables)
# ============================================================================

#' Flatten per-table filters to column-level for lookup operations
#'
#' Converts nested per-table filter lists (e.g., `list(adsl = list(SEX = "F"))`)
#' to flat column-level lists (e.g., `list(SEX = "F")`), keeping only columns
#' present in the lookup table.
#'
#' @param cat_filters Named list of per-table categorical filters
#' @param rng_filters Named list of per-table range filters
#' @param lookup_columns Character vector of column names in
#'   the lookup table
#' @return List with `cat` (flat categorical) and `rng`
#'   (flat range) filter lists
#' @keywords internal
flatten_filters_for_lookup <- function(
  cat_filters, rng_filters, lookup_columns
) {
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
#' @param active_dims Named list: tbl_name -> character vector
#'   of active dim columns
#' @param pks PK tibble from dm::dm_get_all_pks()
#' @param fks FK tibble from dm::dm_get_all_fks()
#' @param measure_col Optional measure spec (".count" or "table.column")
#' @return List with lookups, dim_source, parent_key, child_fk_cols,
#'   parent_table, child_tables; or NULL if topology doesn't support lookup
#' @keywords internal
build_crossfilter_lookups <- function(
  tables, active_dims, pks, fks, measure_col = NULL
) {
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
    if (
      !is.null(measure_tbl) && measure_tbl == child_tbl &&
        !is.null(measure_cn)
    ) {
      child_select <- unique(c(child_select, measure_cn))
    }
    child_select <- intersect(child_select, names(child_df))

    # Columns from parent: PK + parent dims + measure
    parent_select <- unique(c(parent_key, parent_dims))
    if (
      !is.null(measure_tbl) &&
        measure_tbl == parent_table &&
        !is.null(measure_cn)
    ) {
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
    lookups[[child_tbl]] <- dplyr::left_join(
      child_sub, parent_sub, by = by_spec
    )
  }

  if (length(lookups) == 0) return(NULL)

  # Emit a parent-keyed lookup so parent-dim cards (e.g. ARM, SEX) count over
  # the full set of parent records rather than getting hosted on a child
  # instance — which would undercount any parent that lacks rows in that
  # child (e.g. subjects with no AEs missing from an adae-hosted ARM card).
  if (length(parent_dims) > 0) {
    parent_select <- unique(c(parent_key, parent_dims))
    if (
      !is.null(measure_tbl) && measure_tbl == parent_table &&
        !is.null(measure_cn)
    ) {
      parent_select <- unique(c(parent_select, measure_cn))
    }
    parent_select <- intersect(parent_select, names(parent_df))
    if (parent_key %in% parent_select && length(parent_select) > 1) {
      lookups[[parent_table]] <- unique(
        parent_df[, parent_select, drop = FALSE]
      )
      child_fk_col_map[[parent_table]] <- parent_key
    }
  }

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
lookup_crossfilter_data <- function(
  lookup_info, tbl_name, exclude_dim, cat_filters, rng_filters
) {
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
  flat <- flatten_filters_for_lookup(
    cat_filters, rng_filters, names(primary_lookup)
  )
  filtered <- apply_crossfilter_filters(
    primary_lookup, flat$cat, flat$rng,
    exclude_dim = exclude_dim
  )

  # Sibling key intersection (exclude own dim for crossfilter)
  for (other_child in child_tables) {
    if (other_child == primary_child) next
    other_lookup <- lookups[[other_child]]
    if (is.null(other_lookup)) next
    other_key_col <- child_fk_cols[[other_child]]

    other_flat <- flatten_filters_for_lookup(
      cat_filters, rng_filters, names(other_lookup)
    )
    other_filtered <- apply_crossfilter_filters(
      other_lookup, other_flat$cat, other_flat$rng,
      exclude_dim = exclude_dim
    )
    other_keys <- unique(other_filtered[[other_key_col]])
    filtered <- dplyr::filter(
      filtered, .data[[primary_key_col]] %in% other_keys
    )
  }

  # For parent table requests, deduplicate to parent granularity
  if (tbl_name == parent_table) {
    filtered <- filtered[
      !duplicated(filtered[[primary_key_col]]), , drop = FALSE
    ]
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
lookup_crossfilter_agg <- function(
  lookup_info, tbl_name, dim, cat_filters, rng_filters
) {
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
  flat <- flatten_filters_for_lookup(
    cat_filters, rng_filters, names(primary_lookup)
  )
  filtered <- apply_crossfilter_filters(
    primary_lookup, flat$cat, flat$rng,
    exclude_dim = dim
  )

  # Sibling key intersection (exclude own dim for crossfilter)
  for (other_child in child_tables) {
    if (other_child == primary_child) next
    other_lookup <- lookups[[other_child]]
    if (is.null(other_lookup)) next
    other_key_col <- child_fk_cols[[other_child]]

    other_flat <- flatten_filters_for_lookup(
      cat_filters, rng_filters, names(other_lookup)
    )
    other_filtered <- apply_crossfilter_filters(
      other_lookup, other_flat$cat, other_flat$rng,
      exclude_dim = dim
    )
    other_keys <- unique(other_filtered[[other_key_col]])
    filtered <- dplyr::filter(
      filtered, .data[[primary_key_col]] %in% other_keys
    )
  }

  if (nrow(filtered) == 0) {
    return(data.frame(
      x = character(0), .count = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  # Granularity-aware counting
  if (dim_is_parent) {
    # Parent dim: count distinct keys per dim value (subject-level)
    distinct_rows <- unique(filtered[, c(primary_key_col, dim), drop = FALSE])
    agg <- dplyr::summarise(
      distinct_rows, .count = dplyr::n(),
      .by = dplyr::all_of(dim)
    )
  } else {
    # Child dim: count rows (event-level)
    agg <- dplyr::summarise(
      filtered, .count = dplyr::n(),
      .by = dplyr::all_of(dim)
    )
  }

  agg <- dplyr::mutate(agg, !!dim := as.character(.data[[dim]]))
  agg[[dim]][is.na(agg[[dim]])] <- CROSSFILTER_NA
  agg[[dim]][agg[[dim]] == ""] <- CROSSFILTER_EMPTY
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
lookup_crossfilter_counts <- function(
  lookup_info, tables, table_names, cat_filters, rng_filters
) {
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
  allowed_keys <- if (length(key_sets) > 0) {
    Reduce(intersect, key_sets)
  } else {
    NULL
  }

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
