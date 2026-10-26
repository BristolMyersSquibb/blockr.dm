#' Register dm Blocks
#'
#' Registers the dm blocks with blockr.
#'
#' @export
#' @importFrom blockr.core register_blocks new_arg_specs new_arg_spec
#'   arg_string arg_number arg_integer arg_boolean arg_enum arg_array arg_object
register_dm_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_dm_block",
      "new_dm_select_block",
      "new_dm_add_keys_block",
      "new_dm_filter_block",
      "new_dm_pull_block",
      "new_dm_flatten_block",
      "new_crossfilter_block",
      "new_dm_example_block",
      "new_value_filter_block",
      "new_drill_filter_block"
    ),
    name = c(
      "Create dm",
      "Select tables",
      "Add keys",
      "Filter dm",
      "Pull table",
      "Flatten dm",
      "Crossfilter",
      "Example data model",
      "Value filter",
      "Drill filter"
    ),
    description = c(
      "Combine data frames into a dm (data model) object",
      "Select a subset of tables to keep in a dm",
      "Add primary and foreign key relationships to dm",
      paste(
        "Filter dm by condition in any table,",
        "cascading to related tables via FKs"
      ),
      "Extract a single table from dm as a data frame",
      "Flatten dm into a single data frame by joining",
      paste(
        "Client-side crossfilter for data frames",
        "and dm objects using crossfilter2.js"
      ),
      "Load a pre-built dm from example datasets",
      paste(
        "Minimal value filter for a data frame or dm.",
        "Columns hidden behind the gear; per-column single vs multi",
        "select. dm input cascades via FKs."
      ),
      paste(
        "The value filter a drill-down click writes into. One per board,",
        "beside blockr.viz's control bridge extension; senders find it by",
        "class and it resolves a claimed column to a dm table itself."
      )
    ),
    category = c(
      # A block that fetches data is "input", whatever shape it hands back.
      # "structured" is where you look for things that reshape a dm you
      # already have.
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "input",
      "structured",
      "structured"
    ),
    icon = c(
      "diagram-3",
      "check2-square",
      "key",
      "funnel",
      "box-arrow-up-right",
      "layers",
      "lightning",
      "database",
      "filter",
      "crosshair"
    ),
    guidance = c(
      # new_dm_block:
      paste(
        "Wraps a set of data frames into a dm (data model) object. Use when",
        "the upstream already provides multiple data frames \u2014 typically via",
        "bind_cols or multiple file loads \u2014 and you want to treat them as a",
        "related table collection."
      ),
      # new_dm_select_block:
      paste(
        "Narrows a dm to a subset of its tables. Use to trim large dm objects",
        "before flattening or displaying."
      ),
      # new_dm_add_keys_block:
      paste(
        "Explicitly sets a primary/foreign key relationship on a dm. Use when",
        "`dm_block(infer_keys=FALSE)` was used or when the relationships need",
        "manual correction."
      ),
      # new_dm_filter_block:
      paste(
        "Filter a dm by one or more conditions on ONE selected table;",
        "matching rows cascade to all related tables via the dm's FK",
        "relationships. Reuses blockr.dplyr's type-aware filter UI",
        "(categorical multi-select, numeric operator+value, or free R",
        "expression) combined with & or |.",
        "\n\nDo NOT use blockr.dplyr::filter_block on a dm - it doesn't",
        "understand cascading."
      ),
      # new_dm_pull_block:
      paste(
        "Extracts a single table from a dm as a plain data frame, dropping",
        "the dm wrapper. Use when a downstream block only accepts a data",
        "frame (e.g. blockr.dplyr blocks, most plot blocks) but the upstream",
        "is a dm."
      ),
      # new_dm_flatten_block:
      paste(
        "Flattens a dm into a single data frame by chained joins starting",
        "from `start_table`. Use when you need a plain wide table for",
        "downstream plotting or modeling."
      ),
      # new_crossfilter_block:
      paste(
        "Interactive client-side crossfilter for data frames or dm objects.",
        "Renders a linked bar/histogram for each `active_dims` column;",
        "clicking a bar filters all downstream views. Categorical filters",
        "live in `filters`, numeric/date ranges in `range_filters`.",
        "\n\nUse whenever the user wants linked filtering across multiple",
        "views. Don't reimplement by chaining filter + join \u2014 this single",
        "block delivers the full brush/linked-filter UX."
      ),
      # new_dm_example_block:
      paste(
        "Loads a pre-built dm from the blockr.dm example catalog. Use for",
        "demos and testing \u2014 no file paths needed."
      ),
      # new_value_filter_block:
      "",
      # new_drill_filter_block:
      paste(
        "The destination for cross-block drill-down claims. Put ONE on a",
        "board together with blockr.viz::new_ctrl_bridge_extension() \u2014 the",
        "two belong together: the bridge opens the control channel, this",
        "block is what the channel writes into. A drilling chart / table /",
        "tile then needs no ctrl_target, because senders resolve it by",
        "class. Unlike a plain value filter it also fills in WHICH dm table",
        "a claimed column belongs to, and reports a claim no table can take",
        "instead of dropping it. Use plain new_value_filter_block() for",
        "every other filtering job on the board."
      )
    ),
    arguments = list(
      dm_block_arguments(),
      dm_select_arguments(),
      dm_add_keys_arguments(),
      dm_filter_arguments(),
      dm_pull_arguments(),
      dm_flatten_arguments(),
      crossfilter_arguments(),
      dm_example_arguments(),
      NULL,
      NULL
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
