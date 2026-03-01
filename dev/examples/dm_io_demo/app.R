# ADAM Workflow — Read, CDISC-keyed dm, Crossfilter, Write
#
# Demonstrates:
# - new_dm_read_block: Read Excel into dm (one sheet per table)
# - new_cdisc_dm_block: Auto-set USUBJID PK/FK + dedup subject columns
# - new_dm_crossfilter_block: Interactive crossfilter across tables
# - new_dm_write_block: Export keyed dm to Excel

library(blockr.core)
library(blockr.dag)
library(blockr.dock)
library(blockr.extra)
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.io")

options(
  blockr.html_table_preview = TRUE
)

# --- Prepare Excel file from safetyData ADAM tables ---
excel_path <- file.path(tempdir(), "adam_data.xlsx")
writexl::write_xlsx(
  list(
    adsl = safetyData::adam_adsl,
    adae = safetyData::adam_adae,
    adlbc = safetyData::adam_adlbc
  ),
  excel_path
)

# --- Build board ---
board <- new_dock_board(
  blocks = c(
    dm_read = new_dm_read_block(
      path = excel_path,
      selected_tables = c("adsl", "adae", "adlbc")
    ),
    dm_adam = new_cdisc_dm_block(dedup_cols = TRUE),
    dm_cf = new_dm_crossfilter_block(),
    dm_out = new_dm_write_block(format = "excel")
  ),
  links = c(
    new_link("dm_read", "dm_adam", "data"),
    new_link("dm_adam", "dm_cf", "data"),
    new_link("dm_cf", "dm_out", "data")
  ),
  extensions = list(
    new_dag_extension()
  )
)

serve(board)
