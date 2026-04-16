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
      "new_cdisc_dm_block",
      "new_dm_select_block",
      "new_dm_add_keys_block",
      "new_dm_filter_block",
      "new_dm_filter_value_block",
      "new_dm_pull_block",
      "new_dm_flatten_block",
      "new_dm_nested_view_block",
      "new_temporal_join_block",
      "new_dm_temporal_join_block",
      "new_crossfilter_block",
      "new_dm_example_block"
    ),
    name = c(
      "Read dm",
      "Write dm",
      "Create dm",
      "CDISC dm",
      "Select tables",
      "Add keys",
      "Filter dm",
      "Filter dm by value",
      "Pull table",
      "Flatten dm",
      "Nested view",
      "Temporal join",
      "dm Temporal join",
      "Crossfilter",
      "DM Example"
    ),
    description = c(
      "Read tables from Excel, ZIP, or directory into dm",
      "Write dm object to Excel, ZIP, or directory",
      "Combine data frames into a dm (data model) object",
      paste(
        "Set CDISC keys (USUBJID PK/FK) and",
        "optionally deduplicate subject columns"
      ),
      "Select a subset of tables to keep in a dm",
      "Add primary and foreign key relationships to dm",
      paste(
        "Filter dm by condition in any table,",
        "cascading to related tables via FKs"
      ),
      paste(
        "Filter dm by selecting a table, column,",
        "and value from dropdowns"
      ),
      "Extract a single table from dm as a data frame",
      "Flatten dm into a single data frame by joining",
      "Display dm as nested table with expandable rows",
      paste(
        "Join two tables and filter by time window",
        "between date columns"
      ),
      paste(
        "Temporal join between two dm tables,",
        "filtering by time window"
      ),
      paste(
        "Client-side crossfilter for data frames",
        "and dm objects using crossfilter2.js"
      ),
      "Load a pre-built dm from example datasets"
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
      "structured",
      "structured",
      "structured",
      "structured"
    ),
    icon = c(
      "file-earmark-arrow-up",
      "file-earmark-arrow-down",
      "diagram-3",
      "patch-check",
      "check2-square",
      "key",
      "funnel",
      "input-cursor-text",
      "box-arrow-up-right",
      "layers",
      "list-nested",
      "clock-history",
      "clock-history",
      "lightning",
      "database"
    ),
    arguments = list(
      dm_read_arguments(),
      dm_write_arguments(),
      dm_block_arguments(),
      cdisc_dm_arguments(),
      dm_select_arguments(),
      dm_add_keys_arguments(),
      dm_filter_arguments(),
      dm_filter_value_arguments(),
      dm_pull_arguments(),
      dm_flatten_arguments(),
      dm_nested_view_arguments(),
      temporal_join_arguments(),
      dm_temporal_join_arguments(),
      crossfilter_arguments(),
      dm_example_arguments()
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
