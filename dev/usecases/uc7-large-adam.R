# Use Case 7: Large realistic ADaM data (efficiency testing)
#
# Reads bundled parquet files from inst/extdata/adam:
# ADSL (400 subjects, 55 cols), ADLB (8400 rows, 49 cols),
# ADCM (2000 rows, 30 cols), ADAE (2100 rows, 39 cols)
#
# Data generated with random.cdisc.data, normalized, labels preserved via parquet.
# dm_read_block reads the directory, dm_block infers PK/FK keys from USUBJID.

library(blockr)
library(blockr.dag)

pkgload::load_all("../blockr.dm")

adam_dir <- system.file("extdata", "adam", package = "blockr.dm")

run_app(
  blocks = c(
    dm_raw      = new_dm_read_block(path = adam_dir),
    dm_obj      = new_dm_block(infer_keys = TRUE),
    crossfilter = new_dm_crossfilter_block(),
    result      = new_dm_pull_block(table = "adsl")
  ),
  links = c(
    new_link("dm_raw", "dm_obj", "data"),
    new_link("dm_obj", "crossfilter", "data"),
    new_link("crossfilter", "result", "data")
  )
)
