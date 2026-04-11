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
      "new_dm_crossfilter_block",
      "new_js_crossfilter_block",
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
      "dm Crossfilter",
      "JS Crossfilter",
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
        "Interactive crossfilter with categorical",
        "tables and numeric range sliders"
      ),
      paste(
        "Cross-table crossfilter on dm with",
        "per-table filter panels"
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
      "sliders",
      "sliders2",
      "lightning",
      "database"
    ),
    arguments = list(
      NULL, NULL, NULL, NULL, NULL, NULL,
      NULL, NULL, NULL, NULL, NULL, NULL, NULL,
      # crossfilter_block (pos 13):
      structure(
        c(
          filters = paste(
            "Categorical filters. Object:",
            "column name -> array of selected",
            "values (strings)"
          ),
          range_filters = paste(
            "Range filters for numeric columns.",
            "Object: column name -> [min, max]"
          ),
          active_dims = paste(
            "Active filter columns. Object with",
            "key \".tbl\" -> array of column names"
          )
        ),
        examples = list(
          filters = list(
            Species = list("setosa", "virginica")
          ),
          range_filters = list(Sepal.Length = c(5, 7)),
          active_dims = list(
            .tbl = list("Species", "Sepal.Width")
          )
        )
      ),
      # dm_crossfilter_block (pos 14):
      structure(
        c(
          active_dims = paste(
            "Per-table active filter columns.",
            "Object: table name -> array of",
            "column names"
          ),
          filters = paste(
            "Per-table categorical filters.",
            "Object: table name ->",
            "{column -> array of values (strings)}"
          ),
          range_filters = paste(
            "Per-table range filters. Object:",
            "table name -> {column -> [min, max]}"
          ),
          measure = paste(
            "Aggregation measure as",
            "\"table.column\" string,",
            "or null for row counts"
          )
        ),
        examples = list(
          active_dims = list(
            adsl = list("SEX", "AGE"),
            adae = list("AESEV")
          ),
          filters = list(adsl = list(SEX = list("F"))),
          range_filters = list(
            adsl = list(AGE = c(40, 60))
          ),
          measure = NULL
        )
      ),
      # js_crossfilter_block (pos 15):
      structure(
        c(
          active_dims = paste(
            "Per-table active filter columns.",
            "Object: table name -> array of",
            "column names. For a single data",
            "frame use \".tbl\" as the table name."
          ),
          filters = paste(
            "Per-table categorical filters.",
            "Object: table name ->",
            "{column -> array of values}"
          ),
          range_filters = paste(
            "Per-table range filters. Object:",
            "table name -> {column -> [min, max]}"
          ),
          measure = paste(
            "Aggregation measure as",
            "\"table.column\" string,",
            "or null for row counts"
          ),
          agg_func = paste(
            "Aggregation function:",
            "\"sum\" or \"mean\".",
            "Only used when measure is set."
          )
        ),
        examples = list(
          active_dims = list(
            adsl = list("SEX", "AGE"),
            adae = list("AESEV")
          ),
          filters = list(adsl = list(SEX = list("F"))),
          range_filters = list(
            adsl = list(AGE = c(40, 60))
          ),
          measure = NULL,
          agg_func = "sum"
        )
      ),
      # dm_example_block (pos 16):
      structure(
        c(
          dataset = paste(
            "ID of the dm example dataset to",
            "load. Use dm_example_choices()",
            "to see available options."
          )
        ),
        examples = list(
          dataset = "bi_star_schema"
        )
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
