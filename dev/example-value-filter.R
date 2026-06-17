# value_filter_block tour — the same block on a data frame and on a dm.
#
# new_value_filter_block() filters by picking concrete values per column behind
# the gear (top right). Single-select always constrains (auto-picks the first
# value); multi-select passes through when empty. Column values load lazily
# (only when a dropdown is opened), so a 10-table ADaM dm doesn't ship every
# column's uniques at startup; high-cardinality dropdowns cap the rendered list
# with a "+N more — type to narrow" hint.
#
# The block accepts BOTH input shapes, and its output preview dispatches on the
# result type. This workflow shows both, from the same Safety ADaM source:
#   * flt_df  — a value filter on a DATA FRAME (one ADaM table, pulled out with
#               new_dm_pull_block). Output preview: a paginated table.
#   * flt_dm  — a value filter on the whole DM. The restriction cascades through
#               foreign keys via dm::dm_filter(); output preview: the interactive
#               Key-lines diagram with click-to-preview tables.
#
# Good things to try:
#   flt_df on adsl: ARM, SEX, AGEGR1
#   flt_dm on adsl/adae: ARM, then AEBODSYS — watch related tables narrow
#
# Run from the workspace root (inside or outside the dev container):
#   Rscript blockr.dm/dev/example-value-filter.R
# open the local URL serve() prints (or uncomment the options line to pin 3838).

options(blockr.html_table_preview = TRUE)
options(blockr.dock_is_locked = FALSE)
# options(shiny.port = 3838L, shiny.host = "0.0.0.0")  # uncomment to pin (devcontainer)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.dm")

stopifnot(requireNamespace("safetyData", quietly = TRUE))

board <- new_dock_board(
  blocks = c(
    data   = new_dm_example_block(dataset = "safetydata_adam"),
    adsl   = new_dm_pull_block(table = "adsl"),
    flt_df = new_value_filter_block(),
    flt_dm = new_value_filter_block()
  ),
  links = c(
    new_link(from = "data", to = "adsl",   input = "data"),
    new_link(from = "adsl", to = "flt_df", input = "data"),
    new_link(from = "data", to = "flt_dm", input = "data")
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
