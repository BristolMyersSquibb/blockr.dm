# Argument metadata helpers for blockr.dm blocks.
#
# Returns named character vectors with `examples` and `prompt` attributes
# that the MCP server surfaces as typed parameter documentation.

#' @noRd
dm_read_arguments <- function() {
  structure(
    c(
      path = paste0(
        "Character. Path to a file or directory to read. Supported: ",
        ".xlsx / .xls (one sheet per table), .zip of per-table files, or a ",
        "directory containing per-table CSV / Parquet / Feather files."
      ),
      selected_tables = paste0(
        "Optional character vector restricting which tables to load. null ",
        "or unset loads every table found at `path`."
      )
    ),
    examples = list(
      path = "/data/trial-01.xlsx",
      selected_tables = list("adsl", "adae", "adlbc")
    ),
    prompt = paste(
      "Reads tabular data from disk into a dm (data model) object. Use as",
      "the entry point when working with multi-table datasets from Excel /",
      "ZIP / directories. The output is a dm that downstream dm_* blocks",
      "can filter, select, join, and flatten."
    )
  )
}

#' @noRd
dm_write_arguments <- function() {
  structure(
    c(
      directory = "Character. Output directory path.",
      filename = paste0(
        "Character. Base filename (no extension). Extension is derived ",
        "from `format`. For Excel, one file; for CSV / Parquet, one file per table."
      ),
      format = paste0(
        "Output format. One of \"excel\", \"csv\", or \"parquet\". ",
        "Default \"excel\"."
      ),
      auto_write = paste0(
        "Logical. TRUE writes on every upstream update; FALSE (default) ",
        "writes only on explicit action."
      )
    ),
    examples = list(
      directory = "/out/",
      filename = "trial-01-snapshot",
      format = "excel",
      auto_write = FALSE
    ),
    prompt = paste(
      "Write a dm object back to disk in Excel / CSV / Parquet form.",
      "Use as a terminal block to export a curated dm."
    )
  )
}

#' @noRd
dm_block_arguments <- function() {
  structure(
    c(
      infer_keys = paste0(
        "Logical. TRUE (default) auto-infers primary and foreign key ",
        "relationships from matching column names. FALSE leaves the dm ",
        "key-less (use dm_add_keys_block to set keys explicitly)."
      )
    ),
    examples = list(infer_keys = TRUE),
    prompt = paste(
      "Wraps a set of data frames into a dm (data model) object. Use when",
      "the upstream already provides multiple data frames — typically via",
      "bind_cols or multiple file loads — and you want to treat them as a",
      "related table collection. For CDISC/ADaM data prefer cdisc_dm_block",
      "which knows the USUBJID convention."
    )
  )
}

#' @noRd
cdisc_dm_arguments <- function() {
  structure(
    c(
      set_keys = paste0(
        "Logical. TRUE (default) sets USUBJID as primary key on ADSL and ",
        "foreign key on all other tables. Required for any cascading dm ",
        "filter to work."
      ),
      dedup_cols = paste0(
        "Logical. TRUE (default) removes duplicated subject-level columns ",
        "(AGE, SEX, ARM, etc.) from non-ADSL tables so they live only on ",
        "ADSL. Avoids column-name collisions in downstream joins."
      )
    ),
    examples = list(set_keys = TRUE, dedup_cols = TRUE),
    prompt = paste(
      "Turns a plain dm of CDISC/ADaM tables (adsl, adae, adlbc, ...) into",
      "a keyed dm where USUBJID is the primary key on ADSL and the foreign",
      "key on every other table. Always place this block immediately after",
      "loading CDISC data and before any filtering, joining, or crossfilter",
      "work. Without keys, downstream dm blocks can't cascade filters."
    )
  )
}

#' @noRd
dm_select_arguments <- function() {
  structure(
    c(
      tables = paste0(
        "Character vector of table names to keep in the dm. Tables not ",
        "listed are dropped. Relationships to dropped tables are removed."
      )
    ),
    examples = list(tables = list("adsl", "adae")),
    prompt = paste(
      "Narrows a dm to a subset of its tables. Use to trim large dm objects",
      "before flattening or displaying."
    )
  )
}

#' @noRd
dm_add_keys_arguments <- function() {
  structure(
    c(
      pk_table = "Character. Name of the table to receive the primary key.",
      pk_column = "Character. Column name on `pk_table` to mark as the primary key.",
      fk_tables = paste0(
        "Character vector of table names that should get a foreign key ",
        "pointing to `pk_table`."
      ),
      fk_column = paste0(
        "Character. Column name on each `fk_tables` entry to mark as the ",
        "foreign key. Must match `pk_column` in meaning, and usually in name."
      )
    ),
    examples = list(
      pk_table = "adsl",
      pk_column = "USUBJID",
      fk_tables = list("adae", "adlbc"),
      fk_column = "USUBJID"
    ),
    prompt = paste(
      "Explicitly sets a primary/foreign key relationship on a dm. Use when",
      "`dm_block(infer_keys=FALSE)` was used or when the relationships need",
      "manual correction. For CDISC data, prefer cdisc_dm_block which sets",
      "these automatically."
    )
  )
}

#' @noRd
dm_filter_arguments <- function() {
  structure(
    c(
      table = paste0(
        "Character. Name of the table within the dm where conditions are ",
        "applied. Rows removed from this table cascade to related tables ",
        "via the dm's FK relationships."
      ),
      state = paste0(
        "List with `conditions` and `operator`. Matches the state format ",
        "of blockr.dplyr::new_filter_block - condition types `values`, ",
        "`numeric`, and `expr`, combined with `&` or `|`."
      )
    ),
    examples = list(
      table = "adsl",
      state = list(
        conditions = list(list(
          type = "values", column = "ARM",
          values = list("Drug A"), mode = "include"
        )),
        operator = "&"
      )
    ),
    prompt = paste(
      "Filter a dm by one or more conditions on ONE selected table;",
      "matching rows cascade to all related tables via the dm's FK",
      "relationships. Reuses blockr.dplyr's type-aware filter UI",
      "(categorical multi-select, numeric operator+value, or free R",
      "expression) combined with & or |.",
      "\n\nDo NOT use blockr.dplyr::filter_block on a dm - it doesn't",
      "understand cascading."
    )
  )
}

#' @noRd
dm_pull_arguments <- function() {
  structure(
    c(
      table = "Character. Name of the table to extract as a data frame."
    ),
    examples = list(table = "adsl"),
    prompt = paste(
      "Extracts a single table from a dm as a plain data frame, dropping",
      "the dm wrapper. Use when a downstream block only accepts a data",
      "frame (e.g. blockr.dplyr blocks, most plot blocks) but the upstream",
      "is a dm."
    )
  )
}

#' @noRd
dm_flatten_arguments <- function() {
  structure(
    c(
      start_table = paste0(
        "Character. Table to flatten from — becomes the left side of every ",
        "join. Output row count equals this table's row count (for left joins)."
      ),
      include_tables = paste0(
        "Character vector. Tables to join into `start_table`. Empty vector ",
        "flattens the full dm by following all related tables."
      ),
      join_type = paste0(
        "Join type to use throughout. One of \"left\" (default), \"inner\", ",
        "\"full\", \"right\"."
      ),
      recursive = paste0(
        "Logical. TRUE (default) follows multi-hop relationships through ",
        "the FK graph. FALSE joins only direct neighbors of `start_table`."
      )
    ),
    examples = list(
      start_table = "adsl",
      include_tables = list("adae", "adlbc"),
      join_type = "left",
      recursive = TRUE
    ),
    prompt = paste(
      "Flattens a dm into a single data frame by chained joins starting",
      "from `start_table`. Use when you need a plain wide table for",
      "downstream plotting or modeling."
    )
  )
}

#' @noRd
dm_nested_view_arguments <- function() {
  structure(
    c(
      root_table = paste0(
        "Character vector. Root table(s) to render at the top level. Child ",
        "tables are shown as expandable rows via FK relationships."
      )
    ),
    examples = list(root_table = list("adsl")),
    prompt = paste(
      "Renders a dm as a nested expandable table. Use for interactive",
      "exploration of relationship structure. Purely a display block — it",
      "doesn't transform data."
    )
  )
}

#' @noRd
dm_temporal_join_arguments <- function() {
  structure(
    c(
      left_table = "Character. Name of the \"anchor\" table in the dm.",
      left_date = paste0(
        "Character. Date/datetime column on `left_table` that defines the ",
        "anchor time (e.g. treatment start)."
      ),
      right_table = paste0(
        "Character. Name of the \"event\" table in the dm being joined in."
      ),
      right_date = paste0(
        "Character. Date/datetime column on `right_table` whose offset from ",
        "`left_date` is checked against the window."
      ),
      window_days = paste0(
        "Integer. Size of the time window in days (default 7)."
      ),
      direction = paste0(
        "Window direction relative to `left_date`. One of \"after\" (default, ",
        "keep events within window_days AFTER anchor), \"before\", or ",
        "\"around\" (two-sided window)."
      )
    ),
    examples = list(
      left_table = "adsl",
      left_date = "TRTSDT",
      right_table = "adae",
      right_date = "ASTDT",
      window_days = 30,
      direction = "after"
    ),
    prompt = paste(
      "Temporal join between two tables within a dm, filtering the right",
      "table to rows whose date falls within `window_days` of the left",
      "date. Use for \"AE on treatment\" style windows: which events occur",
      "within N days of treatment start / end / visit.",
      "\n\nMap common requests:",
      "\n- \"AEs within 30 days of treatment start\" ->",
      "left_table=\"adsl\", left_date=\"TRTSDT\", right_table=\"adae\",",
      "right_date=\"ASTDT\", window_days=30, direction=\"after\"",
      "\n- \"concomitant meds within a week before baseline\" ->",
      "left_date=\"TRTSDT\", right_date=\"CMSTDT\", window_days=7,",
      "direction=\"before\""
    )
  )
}

#' @noRd
temporal_join_arguments <- function() {
  structure(
    c(
      by = paste0(
        "Character vector of column names to match on as the join key ",
        "(e.g. subject ID). Rows with matching key AND date-within-window ",
        "are joined."
      ),
      left_date = paste0(
        "Character. Date column on the LEFT (x) table that defines the ",
        "anchor time."
      ),
      right_date = paste0(
        "Character. Date column on the RIGHT (y) table whose offset from ",
        "`left_date` is checked against the window."
      ),
      window_days = "Integer. Window size in days. Default 7.",
      direction = paste0(
        "Window direction relative to `left_date`. One of \"after\" (default), ",
        "\"before\", \"around\"."
      )
    ),
    examples = list(
      by = list("USUBJID"),
      left_date = "TRTSDT",
      right_date = "ASTDT",
      window_days = 30,
      direction = "after"
    ),
    prompt = paste(
      "Plain two-table temporal join (no dm wrapper). Joins x and y on",
      "`by` AND filters to rows where y's `right_date` is within",
      "`window_days` of x's `left_date`. Use this when you have two plain",
      "data frames. For dm objects, use dm_temporal_join_block."
    )
  )
}

#' @noRd
crossfilter_arguments <- function() {
  structure(
    c(
      active_dims = paste0(
        "Per-table active filter columns. Object: table name -> array of ",
        "column names that are eligible for filtering. For a single data ",
        "frame input (not a dm), use \".tbl\" as the table name."
      ),
      filters = paste0(
        "Per-table categorical filters. Object: table name -> {column -> ",
        "array of kept values}. Values not listed are excluded."
      ),
      range_filters = paste0(
        "Per-table numeric/date range filters. Object: table name -> ",
        "{column -> [min, max]}."
      ),
      measure = paste0(
        "Aggregation measure as a \"table.column\" string, or null for row ",
        "counts. Used in linked summary views."
      ),
      agg_func = paste0(
        "Aggregation function applied to `measure`. One of \"sum\" or ",
        "\"mean\". Only used when measure is set."
      )
    ),
    examples = list(
      active_dims = list(
        adsl = list("SEX", "AGE", "ARM"),
        adae = list("AESEV")
      ),
      filters = list(adsl = list(SEX = list("F"))),
      range_filters = list(adsl = list(AGE = c(40, 60))),
      measure = NULL,
      agg_func = "sum"
    ),
    prompt = paste(
      "Interactive client-side crossfilter for data frames or dm objects.",
      "Renders a linked bar/histogram for each `active_dims` column;",
      "clicking a bar filters all downstream views. Categorical filters",
      "live in `filters`, numeric/date ranges in `range_filters`.",
      "\n\nUse whenever the user wants linked filtering across multiple",
      "views. Don't reimplement by chaining filter + join — this single",
      "block delivers the full brush/linked-filter UX."
    )
  )
}

#' @noRd
dm_example_arguments <- function() {
  structure(
    c(
      dataset = paste0(
        "Character. ID of the dm example dataset to load. Call ",
        "dm_example_choices() to see available options."
      )
    ),
    examples = list(dataset = "bi_star_schema"),
    prompt = paste(
      "Loads a pre-built dm from the blockr.dm example catalog. Use for",
      "demos and testing — no file paths needed."
    )
  )
}
