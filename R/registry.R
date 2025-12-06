#' Register dm Blocks
#'
#' Registers the dm blocks with blockr.
#'
#' @export
#' @importFrom blockr.core register_blocks
register_dm_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_dm_block",
      "new_dm_add_keys_block",
      "new_dm_filter_block",
      "new_dm_pluck_block",
      "new_dm_flatten_block",
      "new_dm_nested_view_block"
    ),
    name = c(
      "Create dm",
      "Add keys to dm",
      "Filter dm",
      "Pluck table from dm",
      "Flatten dm",
      "Nested view"
    ),
    description = c(
      "Combine multiple data frames into a dm (data model) object",
      "Add primary and foreign key relationships to a dm object",
      "Filter a dm by condition in any table, cascading to related tables via foreign keys",
      "Extract a single table from a dm object as a data frame",
      "Flatten a dm into a single data frame by joining related tables",
      "Display dm as nested table with expandable child rows"
    ),
    category = c(
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform"
    ),
    icon = c(
      "diagram-3",
      "key",
      "funnel",
      "box-arrow-up-right",
      "layers",
      "list-nested"
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
