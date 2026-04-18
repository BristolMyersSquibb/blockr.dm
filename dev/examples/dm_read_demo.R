# Minimal demo: new_dm_read_block reading a directory of parquet files.

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")

library(arrow)
library(blockr)

demo_dir <- file.path(tempdir(), "dm_read_demo")
dir.create(demo_dir, showWarnings = FALSE)
write_parquet(mtcars, file.path(demo_dir, "mtcars.parquet"))
write_parquet(iris, file.path(demo_dir, "iris.parquet"))
write_parquet(airquality, file.path(demo_dir, "airquality.parquet"))

board <- new_dock_board(
  blocks = c(
    dm_data = new_dm_read_block(path = demo_dir),
    table = new_dm_pull_block(table = "mtcars")
  ),
  links = new_link(from = "dm_data", to = "table", input = "data"),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

serve(board)
