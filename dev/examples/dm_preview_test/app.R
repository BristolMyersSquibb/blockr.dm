# Test app for dm table preview on click
library(blockr.core)
library(blockr.extra)

pkgload::load_all("blockr.dm")

options(blockr.html_table_preview = TRUE)

board <- new_board(
  blocks = c(
    dm_data = new_dm_example_block(dataset = "nycflights13")
  )
)

serve(board, "preview_test")
