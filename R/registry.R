#' Register dm Blocks
#'
#' Registers the dm blocks with blockr.
#'
#' @export
#' @importFrom blockr.core register_blocks
register_dm_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_dm_read_block",
      "new_dm_write_block",
      "new_dm_block",
      "new_dm_select_block",
      "new_dm_add_keys_block",
      "new_dm_filter_block",
      "new_dm_pull_block",
      "new_dm_flatten_block",
      "new_dm_nested_view_block",
      "new_temporal_join_block",
      "new_crossfilter_block",
      "new_dm_crossfilter_block"
    ),
    name = c(
      "Read dm",
      "Write dm",
      "Create dm",
      "Select tables",
      "Add keys to dm",
      "Filter dm",
      "Pull table from dm",
      "Flatten dm",
      "Nested view",
      "Temporal join",
      "Crossfilter",
      "dm Crossfilter"
    ),
    description = c(
      "Read multiple tables from Excel, ZIP, or directory into a dm object",
      "Write dm object to Excel, ZIP, or directory",
      "Combine multiple data frames into a dm (data model) object",
      "Select a subset of tables to keep in a dm object",
      "Add primary and foreign key relationships to a dm object",
      "Filter a dm by condition in any table, cascading to related tables via foreign keys",
      "Extract a single table from a dm object as a data frame",
      "Flatten a dm into a single data frame by joining related tables",
      "Display dm as nested table with expandable child rows",
      "Join two tables and filter by time window between date columns",
      "Interactive crossfilter with categorical tables and numeric range sliders",
      "Cross-table crossfilter on a dm object with per-table filter panels"
    ),
    category = c(
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "structured"
    ),
    icon = c(
      "file-earmark-arrow-up",
      "file-earmark-arrow-down",
      "diagram-3",
      "check2-square",
      "key",
      "funnel",
      "box-arrow-up-right",
      "layers",
      "list-nested",
      "clock-history",
      "sliders",
      "sliders2"
    ),
    arguments = list(
      NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
      # crossfilter_block (pos 11):
      c(
        filters = "Categorical filters. Object: column name -> array of selected values (strings)",
        range_filters = "Range filters for numeric columns. Object: column name -> [min, max]",
        active_dims = "Active filter columns. Object with key \".tbl\" -> array of column names"
      ),
      # dm_crossfilter_block (pos 12):
      c(
        active_dims = "Per-table active filter columns. Object: table name -> array of column names",
        filters = "Per-table categorical filters. Object: table name -> {column -> array of values (strings)}",
        range_filters = "Per-table range filters. Object: table name -> {column -> [min, max]}",
        measure = "Aggregation measure as \"table.column\" string, or null for row counts"
      )
    ),
    examples = list(
      NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
      # crossfilter_block (pos 11):
      paste0(
        'Categorical: {"filters": {"Species": ["setosa", "virginica"]}}\n',
        'Numeric range: {"range_filters": {"Sepal.Length": [5, 7]}}\n',
        'Active dims: {"active_dims": {".tbl": ["Species", "Sepal.Width"]}}'
      ),
      # dm_crossfilter_block (pos 12):
      paste0(
        'Filter one table: {"filters": {"adsl": {"SEX": ["F"]}}}\n',
        'Range + categorical: {"filters": {"adae": {"AESEV": ["MILD", "SEVERE"]}}, ',
        '"range_filters": {"adsl": {"AGE": [40, 60]}}}\n',
        'Set active dims: {"active_dims": {"adsl": ["SEX", "AGE"], "adae": ["AESEV"]}}'
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
