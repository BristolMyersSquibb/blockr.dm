# dm filtering tour — value_filter_block and crossfilter_block side by side.
#
# Both blocks filter a `dm` and cascade the restriction through foreign keys
# (`dm::dm_filter()`), but offer different UX:
#
#   new_value_filter_block()  — pick concrete values per column behind the gear
#     (top right). Single-select always constrains (auto-picks the first value);
#     multi-select passes through when empty. For a dm the gear gains a table
#     selector. Column values load lazily (only when a dropdown is opened), so a
#     10-table ADaM dm doesn't ship every column's uniques at startup; high-
#     cardinality dropdowns cap the rendered list with a "+N more" hint.
#
#   new_crossfilter_block()   — ship the dm to the browser and filter it
#     client-side (crossfilter2): categorical columns become clickable bar
#     lists, numeric/date columns become range sliders with a density curve.
#
# Both accept a data frame or a dm, and their output preview dispatches on the
# result type: a dm renders the interactive Key-lines diagram with click-to-
# preview tables; a data frame renders a paginated table.
#
# The source is the Safety ADaM example dm (`new_dm_example_block`). Good things
# to try in either block:
#   adsl: ARM, SEX, AGEGR1        categorical pickers / bar lists
#   adsl: AGE, BMIBL              numeric (crossfilter shows a density slider)
#   adae: AEBODSYS, AESEV         filter AEs; watch adsl narrow via the FK
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
    flt  = new_value_filter_block(),
    cf   = new_crossfilter_block()
  ),
  links = c(
    new_link(from = "data", to = "flt", input = "data"),
    new_link(from = "data", to = "cf",  input = "data")
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
