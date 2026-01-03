# blockr.dm ADaM Workflow Demo
#
# Demonstrates cross-table queries on ADaM-style pharmaceutical data:
# "For subjects with Neutrophil counts > 5, what are the adverse events?"
#
# Features:
# - Automatic key inference from column names
# - Nested view with expandable rows (click to see child tables)

library(blockr)
library(blockr.dag)
library(blockr.dm)

# Create test data simulating ADaM structure
adsl <- data.frame(
  USUBJID = paste0("SUBJ-", 1:5),
  AGE = c(45, 52, 38, 61, 29),
  SEX = c("M", "F", "M", "F", "M")
)

adlb <- data.frame(
  USUBJID = rep(paste0("SUBJ-", 1:5), each = 2),
  PARAMCD = rep(c("NEUT", "WBC"), 5),
  AVAL = c(4.5, 8.2, 6.1, 7.5, 3.2, 6.8, 5.5, 9.1, 4.8, 7.2)
)

adae <- data.frame(
  USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-2", "SUBJ-4", "SUBJ-4", "SUBJ-4"),
  AETERM = c("Headache", "Nausea", "Fatigue", "Dizziness", "Headache", "Rash"),
  AESEV = c("MILD", "MODERATE", "MILD", "SEVERE", "MILD", "MODERATE")
)

# Launch the workflow
run_app(
  blocks = c(
    # Load the three datasets
    adsl_data = new_static_block(data = adsl),
    adlb_data = new_static_block(data = adlb),
    adae_data = new_static_block(data = adae),

    # Combine into dm object with automatic key inference
    # Since USUBJID is unique in adsl but not in adlb/adae,
    # it automatically adds: PK(adsl.USUBJID) and FKs from adlb/adae
    dm_obj = new_dm_block(infer_keys = TRUE),

    # Filter by lab condition - cascades to related tables!
    filtered_dm = new_dm_filter_block(
      table = "adlb_data",
      expr = "PARAMCD == 'NEUT' & AVAL > 5"
    ),

    # Extract adverse events for filtered subjects
    ae_results = new_dm_pull_block(table = "adae_data"),

    # Flatten: join adae with adsl for filtered subjects
    flattened = new_dm_flatten_block(start_table = "adae_data"),

    # Nested view: click subjects to expand and see their AEs and labs
    nested = new_dm_nested_view_block(root_table = "adsl_data")
  ),
  links = c(
    # Connect datasets to dm block
    new_link("adsl_data", "dm_obj", "adsl_data"),
    new_link("adlb_data", "dm_obj", "adlb_data"),
    new_link("adae_data", "dm_obj", "adae_data"),

    # Filter the dm (no need for dm_keys blocks - keys are auto-inferred!)
    new_link("dm_obj", "filtered_dm", "data"),

    # Extract AE table
    new_link("filtered_dm", "ae_results", "data"),

    # Flatten filtered dm to single table
    new_link("filtered_dm", "flattened", "data"),

    # Nested view of the filtered dm
    new_link("filtered_dm", "nested", "data")
  ),
  extensions = list(
    new_dag_extension()
  )
)
