# ============================================================
# DM viewer demo — the "Key lines" data-model viewer
#
# A single `dm_example_block` whose built-in dataset switcher cycles
# through the bundled example data models (BI star schema, Safety /
# Pharmaverse / BMS ADaM, Insurance). Each renders with the Key-lines
# schematic: pick a table to preview its rows.
#
# Runs inside a dock + DAG board: the dock UI gives the block sidebar
# and gear offcanvas, and the DAG panel makes the data-model flow
# visible alongside the schematic.
#
# Run:  shiny::runApp(system.file("examples/dm-viewer-demo", package = "blockr.dm"))
#   or: Rscript inst/examples/dm-viewer-demo/app.R
# ============================================================

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")

# Use the restyled HTML table preview for the row view.
options(blockr.html_table_preview = TRUE)

board <- new_dock_board(
  blocks = c(
    data_model = new_dm_example_block(dataset = "bi_star_schema")
  ),
  extensions = new_dock_extensions(list(
    new_dag_extension()
  ))
)

serve(board, "dm-viewer-demo")
