# value-filter-dm-demo.R — value_filter_block on a real multi-table dm.
#
# Uses the built-in dm_example_block (safetydata_adam: 10 ADaM tables) as the
# source — NOT a static block — so the dm cascade and lazy value loading are
# exercised on realistic, high-cardinality data.
#
# Serve on the forwarded port:
#   options(shiny.port = 3838, shiny.host = "0.0.0.0")
#   source("blockr.dm/dev/value-filter-dm-demo.R")

options(blockr.lazy_eval = FALSE, blockr.html_table_preview = TRUE)

suppressMessages({
  pkgload::load_all("blockr.core", quiet = TRUE)
  pkgload::load_all("blockr.dplyr", quiet = TRUE)
  pkgload::load_all("blockr.dm", quiet = TRUE)
  pkgload::load_all("blockr.extra", quiet = TRUE)
})

register_dm_blocks()

board <- new_board(
  blocks = c(
    src = new_dm_example_block(dataset = "safetydata_adam"),
    flt = new_value_filter_block(),
    out = new_dm_pull_block(table = "adsl")
  ),
  links = c(
    new_link("src", "flt", "data"),
    new_link("flt", "out", "data")
  )
)

serve(board)
