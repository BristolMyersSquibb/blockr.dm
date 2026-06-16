# ============================================================
# DM viewer demo — the "Key lines" data-model viewer
#
# A single `dm_example_block` whose built-in dataset switcher cycles
# through the bundled example data models (BI star schema, Safety /
# Pharmaverse / BMS ADaM, Insurance). Each renders with the Key-lines
# schematic: pick a table to preview its rows.
#
# Run:  shiny::runApp(system.file("examples/dm-viewer-demo", package = "blockr.dm"))
#   or: Rscript inst/examples/dm-viewer-demo/app.R
# ============================================================

library(blockr.core)
library(blockr.dm)

# Use the restyled HTML table preview for the row view.
options(blockr.html_table_preview = TRUE)

serve(
  new_board(
    blocks = list(
      data_model = new_dm_example_block(dataset = "bi_star_schema")
    )
  )
)
