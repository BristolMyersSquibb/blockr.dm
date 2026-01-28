# Demo: dm_read_block with directory of parquet files
pkgload::load_all()
library(arrow)
library(blockr)

# Create a temp directory with sample parquet files
demo_dir <- file.path(tempdir(), "dm_read_demo")
dir.create(demo_dir, showWarnings = FALSE)

# Write sample data as parquet files
write_parquet(mtcars, file.path(demo_dir, "mtcars.parquet"))
write_parquet(iris, file.path(demo_dir, "iris.parquet"))
write_parquet(airquality, file.path(demo_dir, "airquality.parquet"))
write_parquet(ChickWeight, file.path(demo_dir, "chickweight.parquet"))

cat("Created parquet files in:", demo_dir, "\n")
list.files(demo_dir)

# Run app: read from directory, pull one table
run_app(
 blocks = c(
    dm_data = new_dm_read_block(path = demo_dir),
    table = new_dm_pull_block(table = "mtcars")
  ),
  links = c(
    new_link("dm_data", "table", "data")
  )
)

# Clean up
unlink(demo_dir, recursive = TRUE)
