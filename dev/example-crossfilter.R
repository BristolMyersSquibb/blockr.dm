# Cross-filter tour — the interactive crossfilter block over a real dm.
#
# `new_crossfilter_block()` ships a dm to the browser and filters it client-side
# (crossfilter2): categorical columns become clickable bar lists, numeric and
# date columns become range sliders with a density curve. Open the gear (top
# left of the block) to add/remove filter columns.
#
# The data is the Safety ADaM example dm (`new_dm_example_block`), so the gear
# search covers every ADaM column. Good things to add:
#   ARM, SEX, AGEGR1   categorical bar lists
#   AGE, BMIBL         numeric sliders (with density)
#   AENDT (in adae)    a DATE slider — 473 of 1191 AE end-dates are missing;
#                      the slider spans the real range and its density shows
#                      where the observations cluster. (This is the date-with-NA
#                      case the slider used to mishandle.)
#
# Run from the workspace root (inside or outside the dev container):
#   Rscript blockr.dm/dev/example-crossfilter.R
# open the local URL serve() prints (or uncomment the options line to pin 3838).

options(blockr.html_table_preview = TRUE)
options(blockr.dock_is_locked = FALSE)
# options(shiny.port = 3838L, shiny.host = "0.0.0.0")  # uncomment to pin (devcontainer)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.dm")

stopifnot(requireNamespace("safetyData", quietly = TRUE))

board <- new_dock_board(
  blocks = c(
    data = new_dm_example_block(dataset = "safetydata_adam"),
    cf   = new_crossfilter_block()
  ),
  links = new_link(from = "data", to = "cf", input = "data"),
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
