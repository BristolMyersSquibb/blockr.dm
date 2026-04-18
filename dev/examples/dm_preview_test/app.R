# Minimal demo: click-to-preview on dm outputs.

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")

library(blockr)

options(blockr.html_table_preview = TRUE)

board <- new_dock_board(
  blocks = c(
    dm_data = new_dm_example_block(dataset = "nycflights13")
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

serve(board)
