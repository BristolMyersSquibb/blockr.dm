# Argument metadata helpers for blockr.dm blocks.
#
# Each helper returns a `new_block_args()` object describing the block's
# constructor arguments (description + worked example + optional type
# descriptor). The free-text construction guidance that used to live in the
# legacy `prompt` attribute now lives in the `guidance` vector of the
# `register_blocks()` call in registry.R.

#' @noRd
dm_read_arguments <- function() {
  new_block_args(
    path = new_block_arg(
      paste0(
        "Character. Path to a file or directory to read. Supported: ",
        ".xlsx / .xls (one sheet per table), .zip of per-table files, or a ",
        "directory containing per-table CSV / Parquet / Feather files."
      ),
      example = "/data/trial-01.xlsx",
      type = arg_string()
    ),
    selected_tables = new_block_arg(
      paste0(
        "Optional character vector restricting which tables to load. null ",
        "or unset loads every table found at `path`."
      ),
      example = list("adsl", "adae", "adlbc"),
      type = arg_array(arg_string())
    )
  )
}

#' @noRd
dm_write_arguments <- function() {
  new_block_args(
    directory = new_block_arg(
      "Character. Output directory path.",
      example = "/out/",
      type = arg_string()
    ),
    filename = new_block_arg(
      paste0(
        "Character. Base filename (no extension). Extension is derived ",
        "from `format`. For Excel, one file; for CSV / Parquet, one file per table."
      ),
      example = "trial-01-snapshot",
      type = arg_string()
    ),
    format = new_block_arg(
      paste0(
        "Output format. One of \"excel\", \"csv\", or \"parquet\". ",
        "Default \"excel\"."
      ),
      example = "excel",
      type = arg_enum(c("excel", "csv", "parquet"))
    ),
    auto_write = new_block_arg(
      paste0(
        "Logical. TRUE writes on every upstream update; FALSE (default) ",
        "writes only on explicit action."
      ),
      example = FALSE,
      type = arg_boolean()
    )
  )
}

#' @noRd
dm_block_arguments <- function() {
  new_block_args(
    infer_keys = new_block_arg(
      paste0(
        "Logical. TRUE (default) auto-infers primary and foreign key ",
        "relationships from matching column names. FALSE leaves the dm ",
        "key-less (use dm_add_keys_block to set keys explicitly)."
      ),
      example = TRUE,
      type = arg_boolean()
    )
  )
}

#' @noRd
cdisc_dm_arguments <- function() {
  new_block_args(
    set_keys = new_block_arg(
      paste0(
        "Logical. TRUE (default) sets USUBJID as primary key on ADSL and ",
        "foreign key on all other tables. Required for any cascading dm ",
        "filter to work."
      ),
      example = TRUE,
      type = arg_boolean()
    ),
    dedup_cols = new_block_arg(
      paste0(
        "Logical. TRUE (default) removes duplicated subject-level columns ",
        "(AGE, SEX, ARM, etc.) from non-ADSL tables so they live only on ",
        "ADSL. Avoids column-name collisions in downstream joins."
      ),
      example = TRUE,
      type = arg_boolean()
    )
  )
}

#' @noRd
dm_select_arguments <- function() {
  new_block_args(
    tables = new_block_arg(
      paste0(
        "Character vector of table names to keep in the dm. Tables not ",
        "listed are dropped. Relationships to dropped tables are removed."
      ),
      example = list("adsl", "adae"),
      type = arg_array(arg_string())
    )
  )
}

#' @noRd
dm_add_keys_arguments <- function() {
  new_block_args(
    pk_table = new_block_arg(
      "Character. Name of the table to receive the primary key.",
      example = "adsl",
      type = arg_string()
    ),
    pk_column = new_block_arg(
      "Character. Column name on `pk_table` to mark as the primary key.",
      example = "USUBJID",
      type = arg_string()
    ),
    fk_tables = new_block_arg(
      paste0(
        "Character vector of table names that should get a foreign key ",
        "pointing to `pk_table`."
      ),
      example = list("adae", "adlbc"),
      type = arg_array(arg_string())
    ),
    fk_column = new_block_arg(
      paste0(
        "Character. Column name on each `fk_tables` entry to mark as the ",
        "foreign key. Must match `pk_column` in meaning, and usually in name."
      ),
      example = "USUBJID",
      type = arg_string()
    )
  )
}

#' @noRd
dm_filter_arguments <- function() {
  new_block_args(
    table = new_block_arg(
      paste0(
        "Character. Name of the table within the dm where conditions are ",
        "applied. Rows removed from this table cascade to related tables ",
        "via the dm's FK relationships."
      ),
      example = "adsl",
      type = arg_string()
    ),
    # `state` is an object wrapping a polymorphic-union conditions array
    # (values | numeric | expr). Every possible condition field is declared;
    # the non-universal ones are required = FALSE so the worked example
    # (which uses only the `values` subset) still conforms.
    state = new_block_arg(
      paste0(
        "List with `conditions` and `operator`. Matches the state format ",
        "of blockr.dplyr::new_filter_block - condition types `values`, ",
        "`numeric`, and `expr`, combined with `&` or `|`."
      ),
      example = list(
        conditions = list(list(
          type = "values", column = "ARM",
          values = list("Drug A"), mode = "include"
        )),
        operator = "&"
      ),
      type = arg_object(
        conditions = arg_array(
          arg_object(
            type = arg_enum(c("values", "numeric", "expr")),
            column = arg_string(required = FALSE),
            values = arg_array(arg_string(), required = FALSE),
            mode = arg_enum(c("include", "exclude"), required = FALSE),
            op = arg_enum(c(">", ">=", "<", "<="), required = FALSE),
            value = arg_number(required = FALSE),
            expr = arg_string(required = FALSE)
          )
        ),
        operator = arg_enum(c("&", "|"))
      )
    )
  )
}

#' @noRd
dm_filter_by_data_arguments <- function() {
  new_block_args(
    table = new_block_arg(
      paste0(
        "Character. Name of the dm table to filter. Rows whose `key_col` ",
        "value is absent from `by[[key_col]]` are dropped; matching rows ",
        "cascade to related tables via FKs. Default \"adsl\"."
      ),
      example = "adsl",
      type = arg_string()
    ),
    key_col = new_block_arg(
      paste0(
        "Character. Column used for the match. Must exist in both ",
        "`by` (the second input data frame) and `table` (the selected dm ",
        "table). Default \"USUBJID\"."
      ),
      example = "USUBJID",
      type = arg_string()
    ),
    distinct_only = new_block_arg(
      paste0(
        "Logical. TRUE (default) deduplicates `by[[key_col]]` before ",
        "matching. FALSE passes the raw column through."
      ),
      example = TRUE,
      type = arg_boolean()
    )
  )
}

#' @noRd
dm_pull_arguments <- function() {
  new_block_args(
    table = new_block_arg(
      "Character. Name of the table to extract as a data frame.",
      example = "adsl",
      type = arg_string()
    )
  )
}

#' @noRd
dm_flatten_arguments <- function() {
  new_block_args(
    start_table = new_block_arg(
      paste0(
        "Character. Table to flatten from \u2014 becomes the left side of every ",
        "join. Output row count equals this table's row count (for left joins)."
      ),
      example = "adsl",
      type = arg_string()
    ),
    include_tables = new_block_arg(
      paste0(
        "Character vector. Tables to join into `start_table`. Empty vector ",
        "flattens the full dm by following all related tables."
      ),
      example = list("adae", "adlbc"),
      type = arg_array(arg_string())
    ),
    join_type = new_block_arg(
      paste0(
        "Join type to use throughout. One of \"left\" (default), \"inner\", ",
        "\"full\", \"right\"."
      ),
      example = "left",
      type = arg_enum(c("left", "inner", "full", "right"))
    ),
    recursive = new_block_arg(
      paste0(
        "Logical. TRUE (default) follows multi-hop relationships through ",
        "the FK graph. FALSE joins only direct neighbors of `start_table`."
      ),
      example = TRUE,
      type = arg_boolean()
    )
  )
}

#' @noRd
dm_nested_view_arguments <- function() {
  new_block_args(
    root_table = new_block_arg(
      paste0(
        "Character vector. Root table(s) to render at the top level. Child ",
        "tables are shown as expandable rows via FK relationships."
      ),
      example = list("adsl"),
      type = arg_array(arg_string())
    )
  )
}

#' @noRd
dm_temporal_join_arguments <- function() {
  new_block_args(
    left_table = new_block_arg(
      "Character. Name of the \"anchor\" table in the dm.",
      example = "adsl",
      type = arg_string()
    ),
    left_date = new_block_arg(
      paste0(
        "Character. Date/datetime column on `left_table` that defines the ",
        "anchor time (e.g. treatment start)."
      ),
      example = "TRTSDT",
      type = arg_string()
    ),
    right_table = new_block_arg(
      paste0(
        "Character. Name of the \"event\" table in the dm being joined in."
      ),
      example = "adae",
      type = arg_string()
    ),
    right_date = new_block_arg(
      paste0(
        "Character. Date/datetime column on `right_table` whose offset from ",
        "`left_date` is checked against the window."
      ),
      example = "ASTDT",
      type = arg_string()
    ),
    window_days = new_block_arg(
      paste0(
        "Integer. Size of the time window in days (default 7)."
      ),
      example = 30,
      type = arg_integer()
    ),
    direction = new_block_arg(
      paste0(
        "Window direction relative to `left_date`. One of \"after\" (default, ",
        "keep events within window_days AFTER anchor), \"before\", or ",
        "\"around\" (two-sided window)."
      ),
      example = "after",
      type = arg_enum(c("after", "before", "around"))
    )
  )
}

#' @noRd
temporal_join_arguments <- function() {
  new_block_args(
    by = new_block_arg(
      paste0(
        "Character vector of column names to match on as the join key ",
        "(e.g. subject ID). Rows with matching key AND date-within-window ",
        "are joined."
      ),
      example = list("USUBJID"),
      type = arg_array(arg_string())
    ),
    left_date = new_block_arg(
      paste0(
        "Character. Date column on the LEFT (x) table that defines the ",
        "anchor time."
      ),
      example = "TRTSDT",
      type = arg_string()
    ),
    right_date = new_block_arg(
      paste0(
        "Character. Date column on the RIGHT (y) table whose offset from ",
        "`left_date` is checked against the window."
      ),
      example = "ASTDT",
      type = arg_string()
    ),
    window_days = new_block_arg(
      "Integer. Window size in days. Default 7.",
      example = 30,
      type = arg_integer()
    ),
    direction = new_block_arg(
      paste0(
        "Window direction relative to `left_date`. One of \"after\" (default), ",
        "\"before\", \"around\"."
      ),
      example = "after",
      type = arg_enum(c("after", "before", "around"))
    )
  )
}

#' @noRd
crossfilter_arguments <- function() {
  new_block_args(
    # arbitrary-key map (table name -> array of columns); the JSON-Schema
    # subset has no open-ended object so `type` is omitted and the consumer
    # infers the shape from the worked example.
    active_dims = new_block_arg(
      paste0(
        "Per-table active filter columns. Object: table name -> array of ",
        "column names that are eligible for filtering. For a single data ",
        "frame input (not a dm), use \".tbl\" as the table name."
      ),
      example = list(
        adsl = list("SEX", "AGE", "ARM"),
        adae = list("AESEV")
      )
    ),
    # arbitrary-key nested map (table -> {column -> kept values}); type omitted.
    filters = new_block_arg(
      paste0(
        "Per-table categorical filters. Object: table name -> {column -> ",
        "array of kept values}. Values not listed are excluded."
      ),
      example = list(adsl = list(SEX = list("F")))
    ),
    # arbitrary-key nested map (table -> {column -> [min, max]}); type omitted.
    range_filters = new_block_arg(
      paste0(
        "Per-table numeric/date range filters. Object: table name -> ",
        "{column -> [min, max]}."
      ),
      example = list(adsl = list(AGE = c(40, 60)))
    ),
    # scalar whose type varies (string | null); type omitted per guidance.
    measure = new_block_arg(
      paste0(
        "Aggregation measure as a \"table.column\" string, or null for row ",
        "counts. Used in linked summary views."
      ),
      example = NULL
    ),
    agg_func = new_block_arg(
      paste0(
        "Aggregation function applied to `measure`. One of \"sum\" or ",
        "\"mean\". Only used when measure is set."
      ),
      example = "sum",
      type = arg_enum(c("sum", "mean"))
    )
  )
}

#' @noRd
dm_example_arguments <- function() {
  new_block_args(
    # The set of valid dataset IDs depends on which data packages are
    # installed (see dm_example_choices()), so it is left as an open string
    # rather than a fixed enum.
    dataset = new_block_arg(
      paste0(
        "Character. ID of the dm example dataset to load. One of: ",
        "\"bi_star_schema\" (retail star schema), ",
        "\"safetydata_adam\" / \"pharmaverseadam\" / \"bms_adam\" ",
        "(CDISC ADaM clinical dm with adsl, adae, ...), ",
        "\"pharmaversesdtm\" (raw CDISC SDTM domains: dm, ae, vs, lb, cm), ",
        "\"nycflights13\", \"insurancedata\". For a CDISC ADaM / clinical ",
        "safety dm use \"safetydata_adam\" (NOT \"cdisc_adam\"). Availability ",
        "depends on installed data packages; call dm_example_choices() for ",
        "the live list."
      ),
      example = "bi_star_schema",
      type = arg_string()
    )
  )
}
