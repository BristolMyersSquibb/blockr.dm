# Argument metadata helpers for blockr.dm blocks.
#
# Each helper returns a `new_arg_specs()` object describing the block's
# constructor arguments (description + worked example + optional type
# descriptor). The free-text construction guidance that used to live in the
# legacy `prompt` attribute now lives in the `guidance` vector of the
# `register_blocks()` call in registry.R.

#' @noRd
dm_block_arguments <- function() {
  new_arg_specs(
    infer_keys = new_arg_spec(
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
dm_select_arguments <- function() {
  new_arg_specs(
    tables = new_arg_spec(
      paste0(
        "Character vector of table names to keep in the dm. Tables not ",
        "listed are dropped. Relationships to dropped tables are removed."
      ),
      example = list("customers", "orders"),
      type = arg_array(arg_string())
    )
  )
}

#' @noRd
dm_add_keys_arguments <- function() {
  new_arg_specs(
    pk_table = new_arg_spec(
      "Character. Name of the table to receive the primary key.",
      example = "customers",
      type = arg_string()
    ),
    pk_column = new_arg_spec(
      "Character. Column name on `pk_table` to mark as the primary key.",
      example = "customer_id",
      type = arg_string()
    ),
    fk_tables = new_arg_spec(
      paste0(
        "Character vector of table names that should get a foreign key ",
        "pointing to `pk_table`."
      ),
      example = list("orders"),
      type = arg_array(arg_string())
    ),
    fk_column = new_arg_spec(
      paste0(
        "Character. Column name on each `fk_tables` entry to mark as the ",
        "foreign key. Must match `pk_column` in meaning, and usually in name."
      ),
      example = "customer_id",
      type = arg_string()
    )
  )
}

#' @noRd
dm_filter_arguments <- function() {
  new_arg_specs(
    table = new_arg_spec(
      paste0(
        "Character. Name of the table within the dm where conditions are ",
        "applied. Rows removed from this table cascade to related tables ",
        "via the dm's FK relationships."
      ),
      example = "customers",
      type = arg_string()
    ),
    # `state` is an object wrapping a polymorphic-union conditions array
    # (values | numeric | expr). Every possible condition field is declared;
    # the non-universal ones are required = FALSE so the worked example
    # (which uses only the `values` subset) still conforms.
    state = new_arg_spec(
      paste0(
        "List with `conditions` and `operator`. Matches the state format ",
        "of blockr.dplyr::new_filter_block - condition types `values`, ",
        "`numeric`, and `expr`, combined with `&` or `|`."
      ),
      example = list(
        conditions = list(list(
          type = "values", column = "segment",
          values = list("Consumer"), mode = "include"
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
dm_pull_arguments <- function() {
  new_arg_specs(
    table = new_arg_spec(
      "Character. Name of the table to extract as a data frame.",
      example = "customers",
      type = arg_string()
    )
  )
}

#' @noRd
dm_flatten_arguments <- function() {
  new_arg_specs(
    start_table = new_arg_spec(
      paste0(
        "Character. Table to flatten from \u2014 becomes the left side of every ",
        "join. Output row count equals this table's row count (for left joins)."
      ),
      example = "orders",
      type = arg_string()
    ),
    include_tables = new_arg_spec(
      paste0(
        "Character vector. Tables to join into `start_table`. Empty vector ",
        "flattens the full dm by following all related tables."
      ),
      example = list("customers", "products"),
      type = arg_array(arg_string())
    ),
    join_type = new_arg_spec(
      paste0(
        "Join type to use throughout. One of \"left\" (default), \"inner\", ",
        "\"full\", \"right\"."
      ),
      example = "left",
      type = arg_enum(c("left", "inner", "full", "right"))
    ),
    recursive = new_arg_spec(
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
#' @noRd
crossfilter_arguments <- function() {
  new_arg_specs(
    # arbitrary-key map (table name -> array of columns); the JSON-Schema
    # subset has no open-ended object so `type` is omitted and the consumer
    # infers the shape from the worked example.
    active_dims = new_arg_spec(
      paste0(
        "Per-table active filter columns. Object: table name -> array of ",
        "column names that are eligible for filtering. For a single data ",
        "frame input (not a dm), use \".tbl\" as the table name."
      ),
      example = list(
        customers = list("segment", "city"),
        categories = list("category_name")
      )
    ),
    # arbitrary-key nested map (table -> {column -> kept values}); type omitted.
    filters = new_arg_spec(
      paste0(
        "Per-table categorical filters. Object: table name -> {column -> ",
        "array of kept values}. Values not listed are excluded."
      ),
      example = list(customers = list(segment = list("Consumer")))
    ),
    # arbitrary-key map (column -> {columns}); type omitted.
    subgroup = new_arg_spec(
      paste0(
        "A second column the board is split by, under `pinned`: tables nest ",
        "it under the group, charts facet by it. Same eligible columns as ",
        "`pinned`, minus the pinned one. Null for none."
      ),
      example = "city"
    ),
    groups = new_arg_spec(
      paste0(
        "Group definitions for the pinned column and the subgroup. Object: ",
        "column name -> {columns: array of {name, members: array of levels, ",
        "custom: boolean}, in table order}. A column with an empty name is ",
        "one level on its own; a named one pools its members. A level may ",
        "be in two columns. A level in no column is not shown. A column with ",
        "no entry shows every level on its own."
      ),
      example = list(segment = list(columns = list(
        list(name = "", members = list("Consumer"), custom = FALSE),
        list(
          name = "Corporate",
          members = list("Business", "Enterprise"),
          custom = FALSE
        ),
        list(name = "", members = list("Enterprise"), custom = FALSE)
      )))
    ),
    # arbitrary-key nested map (table -> {column -> [min, max]}); type omitted.
    range_filters = new_arg_spec(
      paste0(
        "Per-table numeric/date range filters. Object: table name -> ",
        "{column -> [min, max]}."
      ),
      example = list(products = list(unit_price = c(10, 100)))
    ),
    # scalar whose type varies (string | null); type omitted per guidance.
    measure = new_arg_spec(
      paste0(
        "Aggregation measure as a \"table.column\" string, or null for row ",
        "counts. Used in linked summary views."
      ),
      example = NULL
    ),
    agg_func = new_arg_spec(
      paste0(
        "Aggregation function applied to `measure`. One of \"sum\" or ",
        "\"mean\". Only used when measure is set."
      ),
      example = "sum",
      type = arg_enum(c("sum", "mean"))
    ),
    featured = new_arg_spec(
      paste0(
        "Columns worth showing up front: they appear as one-click chips ",
        "above the filter cards and supply the choices for `pinned`. Array ",
        "of column names, resolved to whichever table carries them."
      ),
      example = list("segment", "city", "category_name"),
      type = arg_array(arg_string())
    ),
    pinned = new_arg_spec(
      paste0(
        "Column kept in an always-open card at the top of the block, picked ",
        "through a select in that card's header. Must be one of `featured` ",
        "and live on the parent table. Null for no pinned card."
      ),
      example = "segment"
    )
  )
}

#' @noRd
dm_example_arguments <- function() {
  new_arg_specs(
    # The set of valid dataset IDs depends on which data packages are
    # installed (see dm_example_choices()), so it is left as an open string
    # rather than a fixed enum.
    dataset = new_arg_spec(
      paste0(
        "Character. ID of the dm example dataset to load. One of: ",
        "\"bi_star_schema\" (retail star schema), \"nycflights13\", ",
        "\"insurancedata\". Availability depends on installed data ",
        "packages; call dm_example_choices() for the live list."
      ),
      example = "bi_star_schema",
      type = arg_string()
    )
  )
}
