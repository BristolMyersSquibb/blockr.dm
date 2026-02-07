# Use Case 7: Large realistic ADaM data (efficiency testing)
#
# Reads bundled parquet files from inst/extdata/adam:
# ADSL (400 subjects, 55 cols), ADLB (8400 rows, 102 cols),
# ADCM (2000 rows, 83 cols), ADAE (2100 rows, 92 cols)
#
# Data generated with random.cdisc.data, labels preserved via parquet.

library(blockr)
library(blockr.dag)

pkgload::load_all("../blockr.dm")

adam_dir <- system.file("extdata", "adam", package = "blockr.dm")

run_app(
  blocks = c(
    dm_obj      = new_dm_read_block(path = adam_dir),
    crossfilter = new_dm_crossfilter_block(),
    result      = new_dm_pull_block(table = "adsl")
  ),
  links = c(
    new_link("dm_obj", "crossfilter", "data"),
    new_link("crossfilter", "result", "data")
  )
)
