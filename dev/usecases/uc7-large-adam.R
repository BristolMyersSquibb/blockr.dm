# Use Case 7: Realistic ADaM data (CDISC Pilot 01 from safetyData)
#
# Bundled parquet files from inst/extdata/adam (normalized):
# ADSL  (254 subjects, 48 cols) - Subject Level
# ADLBC (74,264 rows, 34 cols) - Laboratory Chemistry
# ADVS  (32,139 rows, 24 cols) - Vital Signs
# ADAE  (1,191 rows, 45 cols)  - Adverse Events
#
# Source: safetyData R package (CDISC Pilot 01 study)
# Labels preserved via parquet.

pkgload::load_all("../blockr.core")
pkgload::load_all("../blockr.dock")
pkgload::load_all("../blockr.dm")


library(blockr)
library(blockr.dag)


# Enable debug logging to trace double evaluation
options(blockr.log_level = "debug")

adam_dir <- system.file("extdata", "adam", package = "blockr.dm")

run_app(
  blocks = c(
    dm_raw      = new_dm_read_block(path = adam_dir),
    dm_obj      = new_dm_block(infer_keys = TRUE),
    result      = new_dm_pull_block(table = "adsl"),
    crossfilter = new_dm_crossfilter_block(
      active_dims = list(adsl = c("SEX", "AGE"), adae = c("AESEV"))
    )
  ),
  links = c(
    new_link("dm_raw", "dm_obj", "data"),
    new_link("dm_obj", "crossfilter", "data"),
    new_link("crossfilter", "result", "data")
  )
)
