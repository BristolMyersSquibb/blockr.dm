# Simple DM Example Demo
#
# Demonstrates new_dm_example_block() as a zero-config dm data source.
pkgload::load_all("g6R")
pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.session")


library(blockr)

board <-
  new_dock_board(
    blocks = c(
      example = new_dm_example_block()
    ),
    extensions = list(
      blockr.dag::new_dag_extension()
    )
  )

serve(board, "io_app", plugins = custom_plugins(manage_project()))
