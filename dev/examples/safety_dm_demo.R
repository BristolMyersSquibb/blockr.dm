# DM Example Demo
#
# Demonstrates new_dm_example_block() as a zero-config data source.
# By default loads the built-in BI star schema. If safetyData is installed,
# the dropdown also offers Safety ADaM tables.

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dag")

library(blockr)

serve(
  new_dock_board(
    blocks = c(
      # One block gives you a ready-to-use dm
      example = new_dm_example_block(),

      # Interactive crossfilter
      crossfilter = new_dm_crossfilter_block()
    ),
    links = c(
      new_link("example", "crossfilter", "data")
    ),
    extensions = list(
      blockr.dag::new_dag_extension()
    )
  )
)
