# dm IO Demo — Read + Write blocks with dock layout and DAG
#
# Demonstrates:
# - new_dm_read_block: Read Excel file into dm (cheap table discovery + Load button)
# - new_dm_write_block: Write dm to Excel / ZIP with download + server save
# - Dock layout with DAG visualization

library(blockr.core)
library(blockr.dag)
library(blockr.dock)
library(blockr.dm)
pkgload::load_all("blockr.dm")

# options(
#   shiny.port = 7860L,
#   shiny.host = "0.0.0.0",
#   shiny.launch.browser = FALSE
# )

# --- Create test Excel file with ADaM-style tables ---
test_dir <- file.path(tempdir(), "dm_io_demo")
dir.create(test_dir, showWarnings = FALSE)

adsl <- data.frame(
  USUBJID = paste0("SUBJ-", 1:5),
  AGE = c(45, 52, 38, 61, 29),
  SEX = c("M", "F", "M", "F", "M")
)

adae <- data.frame(
  USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-2", "SUBJ-4", "SUBJ-4", "SUBJ-4"),
  AETERM = c("Headache", "Nausea", "Fatigue", "Dizziness", "Headache", "Rash"),
  AESEV = c("MILD", "MODERATE", "MILD", "SEVERE", "MILD", "MODERATE")
)

adlb <- data.frame(
  USUBJID = rep(paste0("SUBJ-", 1:5), each = 2),
  PARAMCD = rep(c("NEUT", "WBC"), 5),
  AVAL = c(4.5, 8.2, 6.1, 7.5, 3.2, 6.8, 5.5, 9.1, 4.8, 7.2)
)

excel_path <- file.path(test_dir, "adam_data.xlsx")
writexl::write_xlsx(list(adsl = adsl, adae = adae, adlb = adlb), excel_path)

cat("Test Excel file created at:", excel_path, "\n")

# --- Build board ---
board <- new_dock_board(
  blocks = c(
    # Read dm from Excel (each sheet becomes a table)
    dm_input = new_dm_read_block(
      path = excel_path,
      selected_tables = c("adsl", "adae", "adlb")
    ),

    # Add keys (USUBJID links the tables)
    dm_keyed = new_dm_add_keys_block(infer_keys = TRUE),

    # Filter: only subjects with severe AEs
    dm_filtered = new_dm_filter_block(
      table = "adae",
      expr = "AESEV == 'SEVERE'"
    ),

    # Flatten filtered dm to single table
    flat_result = new_dm_flatten_block(start_table = "adae"),

    # Write dm back to file
    dm_output = new_dm_write_block(format = "excel")
  ),
  links = c(
    new_link("dm_input", "dm_keyed", "data"),
    new_link("dm_keyed", "dm_filtered", "data"),
    new_link("dm_filtered", "flat_result", "data"),
    new_link("dm_keyed", "dm_output", "data")
  ),
  extensions = list(
    new_dag_extension()
  )
)

serve(board)
