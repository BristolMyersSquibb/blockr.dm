# Value-filter tour — the value_filter_block over a real dm.
#
# `new_value_filter_block()` filters a data frame or a dm by picking concrete
# values per column. Open the gear (top right of the block) to choose which
# columns to filter on; each column toggles between single-select (always
# constrains, auto-picks the first value) and multi-select (empty = passes
# through). For a dm the gear also gains a table selector, and the emitted
# filter goes through `dm::dm_filter()`, so a restriction cascades through
# foreign keys to related tables.
#
# Column values load lazily — the value list for a column is fetched only when
# you open its dropdown (so a 10-table ADaM dm doesn't ship every column's
# uniques at startup). High-cardinality dropdowns render a capped list with a
# "+N more — type to narrow" hint; typing searches the full list.
#
# The data is the Safety ADaM example dm (`new_dm_example_block`), so the gear
# search covers every ADaM column. Good things to add:
#   adsl: ARM, SEX, AGEGR1        single/multi value pickers
#   adae: AEBODSYS, AESEV         filter AEs; watch adsl narrow via the FK
#   adsl: STUDYID                 single-select, auto-picks the one study
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
pkgload::load_all("blockr.dm")

stopifnot(requireNamespace("safetyData", quietly = TRUE))

board <- new_dock_board(
  blocks = c(
    data = new_dm_example_block(dataset = "safetydata_adam"),
    flt  = new_value_filter_block()
  ),
  links = new_link(from = "data", to = "flt", input = "data"),
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
