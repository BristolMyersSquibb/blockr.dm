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


#' Dictionary-encode lookup tables for the wire
#'
#' Replaces every character/factor column in every lookup with 0-based
#' integer codes into a GLOBAL per-column levels vector, then serializes each
#' lookup as columnar JSON. Global (across lookups, per column name) is a
#' correctness requirement, not a size optimization: the client intersects
#' FK-code sets across lookups (`_syncSiblingKeys`), and parent dims are
#' joined into every child lookup as well as the parent lookup, so identical
#' values must encode to identical codes everywhere.
#'
#' NA/empty-string cells become the `CROSSFILTER_NA` / `CROSSFILTER_EMPTY`
#' sentinel BEFORE level collection, so sentinels are ordinary levels: codes
#' never contain NA, and the sentinels keep round-tripping to R as strings
#' through the client's filter state. Non-character columns (numeric,
#' logical, Date) pass through untouched and keep the `na = "null"`
#' serialization -- jsonlite's vector default would write NA as the string
#' `"NA"`, which mixes types in numeric columns and breaks crossfilter's
#' sort/binary-search (filterRange returns wrong rows). JSON null becomes JS
#' null which sorts predictably.
#'
#' Levels are sorted with a C-locale radix sort so re-encoding identical
#' input is byte-identical (supports the send-once identity guard and
#' snapshot tests). The client decodes only at its edges; display order does
#' not depend on level order.
#'
#' @param lookups Named list of data frames (the `lookups` element of any
#'   lookup builder's result)
#' @return List with `lookups` (named list of pre-serialized columnar
#'   `json` strings, one per input lookup) and `levels` (pre-serialized
#'   `json` object: column name -> array of level strings; `{}` when no
#'   column is encoded). Both are embedded verbatim by Shiny
#'   (`json_verbatim = TRUE`), so the client receives parsed objects.
#' @keywords internal
encode_crossfilter_payload <- function(lookups) {
  sentinelize <- function(col) {
    col <- as.character(col)
    col[is.na(col)] <- CROSSFILTER_NA
    col[col == ""] <- CROSSFILTER_EMPTY
    col
  }
  is_enc <- function(col) is.character(col) || is.factor(col)

  lvls <- structure(list(), names = character(0))
  for (df in lookups) {
    for (cn in names(df)) {
      if (is_enc(df[[cn]])) {
        lvls[[cn]] <- unique(c(lvls[[cn]], sentinelize(df[[cn]])))
      }
    }
  }
  lvls <- lapply(lvls, function(v) sort(v, method = "radix"))

  encoded <- lapply(lookups, function(df) {
    for (cn in names(df)) {
      if (is_enc(df[[cn]])) {
        df[[cn]] <- match(sentinelize(df[[cn]]), lvls[[cn]]) - 1L
      } else if (inherits(df[[cn]], "Date")) {
        # Ship dates as epoch-day ints, not ISO strings (~12 bytes/row -> ~5).
        # The client's toEpochDay() accepts both formats (number
        # short-circuit), and NA -> null -> NaN keeps the documented NA-drop
        # behavior. POSIXct is deliberately NOT converted: toEpochDay does
        # not floor, so datetimes carry fractional days, and a whole-day
        # conversion here would silently change range-filter semantics.
        df[[cn]] <- as.integer(unclass(df[[cn]]))
      }
    }
    jsonlite::toJSON(df, dataframe = "columns", na = "null")
  })

  list(lookups = encoded, levels = jsonlite::toJSON(lvls))
}


#' Compress the encoded lookups for the wire
#'
#' httpuv does not negotiate websocket permessage-deflate (verified against
#' the handshake: the 101 response carries no Sec-WebSocket-Extensions), so
#' Shiny custom messages travel uncompressed. Deflating the columnar JSON in
#' R and inflating with the browser's native DecompressionStream recovers
#' what the transport won't give us; the dictionary-encoded int arrays
#' compress extremely well, far outweighing base64's 4/3 inflation.
#'
#' Format trap: R's `memCompress(type = "gzip")` emits ZLIB framing
#' (RFC 1950, `0x78 0x9C` header), NOT gzip (RFC 1952, `0x1F 0x8B`). The
#' Compression Streams API calls that format `'deflate'`, so the payload's
#' `compression` field says "deflate" and the client inflates with
#' `DecompressionStream('deflate')` -- `'gzip'` there fails on the header.
#'
#' Gate: `options(blockr.dm.crossfilter_gzip = FALSE)` ships plain JSON for
#' clients without DecompressionStream (Safari < 16.4); the client accepts
#' both forms per message, keyed on the payload's `compression` field.
#'
#' @param encoded_lookups The `lookups` element of
#'   [encode_crossfilter_payload()]'s result (named list of `json` strings)
#' @return Named list of single-line base64 strings of deflated JSON
#' @keywords internal
compress_crossfilter_lookups <- function(encoded_lookups) {
  lapply(encoded_lookups, function(js) {
    b64 <- jsonlite::base64_enc(
      memCompress(charToRaw(as.character(js)), "gzip")
    )
    # atob() rejects embedded newlines
    gsub("[\r\n]", "", b64)
  })
}


# ============================================================================
# Shared helpers
# ============================================================================

#' Build the filter condition for one categorical dimension
#'
#' The JS UI publishes selected levels as label STRINGS inside a JSON array,
#' and Shiny decodes incoming messages with `simplifyVector = FALSE`, so
#' `["F"]` reaches R as `list("F")` — never an atomic vector. Pasting that
#' straight into a call produced `SEX == list("F")`: base R coerces the list
#' when the condition is evaluated on a plain data frame, so it looked like it
#' worked, but the generated code is wrong to read and a lazy (DuckDB/dbplyr)
#' backend cannot translate it at all.
#'
#' The sentinels get the same treatment: `CROSSFILTER_NA` means "the missings",
#' which is `is.na(col)`, not a literal `"__NA__"` that matches nothing — the
#' same reading [apply_crossfilter_filters()] uses for the bar counts, so the
#' expression mirrors the numbers the UI shows.
#'
#' @param dim Column name
#' @param val Selected levels, as a list or character vector of label strings
#' @param col The column itself, used to cast the literals back to its type;
#'   `NULL` when the column is not available
#' @return A call, or `NULL` when nothing is selected
#' @keywords internal
crossfilter_cat_condition <- function(dim, val, col = NULL) {
  val <- as.character(unlist(val, use.names = FALSE))
  if (length(val) == 0) return(NULL)

  has_na <- CROSSFILTER_NA %in% val
  lits <- setdiff(val, CROSSFILTER_NA)
  lits[lits == CROSSFILTER_EMPTY] <- ""
  lits <- cast_to_column_type(lits, col)

  sym <- as.name(dim)
  cond <- if (length(lits) == 1) {
    call("==", sym, lits)
  } else if (length(lits) > 1) {
    call("%in%", sym, lits)
  }

  if (has_na) {
    na_cond <- call("is.na", sym)
    cond <- if (is.null(cond)) na_cond else call("|", cond, na_cond)
  }
  cond
}

#' Cast label strings back to the column's own type
#'
#' Low-cardinality numeric columns are dimensions too, and their levels come
#' back from the browser as strings. `AVISITN == "3"` happens to work in base
#' R and fails on a database backend, so cast where the column tells us to.
#' Anything that does not survive the cast (an empty-string level in a numeric
#' column, say) keeps the character form.
#'
#' @param x Character vector of label strings
#' @param col The target column, or `NULL`
#' @return `x`, cast to `col`'s type where possible
#' @keywords internal
cast_to_column_type <- function(x, col) {
  if (is.null(col) || is.character(col) || is.factor(col)) return(x)
  cast <- if (is.logical(col)) {
    as.logical
  } else if (is.integer(col)) {
    as.integer
  } else if (is.numeric(col)) {
    as.numeric
  } else {
    return(x)
  }
  out <- suppressWarnings(cast(x))
  if (anyNA(out)) x else out
}

#' Apply categorical + range filters to a data frame (dplyr-compatible)
#'
#' Uses `.data[[dim]]` pronoun — works with standard dplyr.
#' @param df A data frame
#' @param cat_f Named list of character vectors (categorical filters)
#' @param rng_f Named list of numeric(2) vectors (range filters)
#' @param exclude_dim Optional dimension name to skip
#' @return Filtered data frame
#' @importFrom rlang .data :=
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
    # as.list(): this ends up in a sendCustomMessage payload, and Shiny's
    # jsonlite serialization (keep_vec_names = TRUE) is deprecated for
    # named vectors — a future jsonlite would encode them as arrays,
    # dropping the table keys the JS relies on (childFkCols[table]).
    # A named list yields the same keyed-object JSON, warning-free.
    child_fk_cols = as.list(child_fk_col_map),
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
