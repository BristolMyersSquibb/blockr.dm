# Minimal demo: new_dm_filter_block with cascading FK filter.
#
# The filter is applied to one table; dm::dm_filter() semi-joins the
# condition through the foreign-key graph so related tables are trimmed too.
# The condition UI is the same type-aware filter as blockr.dplyr's
# new_filter_block (values / numeric / expr).

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dm")

library(blockr)

board <- new_dock_board(
  blocks = c(
    dm_data = new_dm_example_block(dataset = "nycflights13"),
    filtered = new_dm_filter_block(
      table = "flights",
      state = list(
        conditions = list(
          list(
            type = "values", column = "carrier",
            values = list("UA"), mode = "include"
          )
        ),
        operator = "&"
      )
    ),
    pulled = new_dm_pull_block(table = "flights")
  ),
  links = c(
    new_link(from = "dm_data", to = "filtered", input = "data"),
    new_link(from = "filtered", to = "pulled", input = "data")
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

serve(board)
