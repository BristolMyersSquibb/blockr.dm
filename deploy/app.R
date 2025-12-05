# blockr.dm ADaM Workflow Demo
#
# Demonstrates cross-table queries on ADaM-style pharmaceutical data:
# "For subjects with Neutrophil counts > 5, what are the adverse events?"

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

    # Combine into dm object - shows diagram with 3 unconnected tables
    dm_obj = new_dm_block(),

    # Add primary key (USUBJID in adsl) and foreign key from adlb
    dm_keys1 = new_dm_add_keys_block(
      pk_table = "adsl_data",
      pk_column = "USUBJID",
      fk_table = "adlb_data",
      fk_column = "USUBJID"
    ),

    # Add another foreign key (adae -> adsl)
    dm_keys2 = new_dm_add_keys_block(
      pk_table = "adsl_data",
      pk_column = "USUBJID",
      fk_table = "adae_data",
      fk_column = "USUBJID"
    ),

    # Filter by lab condition - cascades to related tables!
    filtered_dm = new_dm_filter_block(
      table = "adlb_data",
      expr = "PARAMCD == 'NEUT' & AVAL > 5"
    ),

    # Extract adverse events for filtered subjects
    ae_results = new_dm_pluck_block(table = "adae_data"),

    # Flatten: join adae with adsl for filtered subjects
    flattened = new_dm_flatten_block(start_table = "adae_data")
  ),
  links = c(
    # Connect datasets to dm block
    new_link("adsl_data", "dm_obj", "adsl_data"),
    new_link("adlb_data", "dm_obj", "adlb_data"),
    new_link("adae_data", "dm_obj", "adae_data"),

    # Add keys (chained)
    new_link("dm_obj", "dm_keys1", "data"),
    new_link("dm_keys1", "dm_keys2", "data"),

    # Filter the dm
    new_link("dm_keys2", "filtered_dm", "data"),

    # Extract AE table
    new_link("filtered_dm", "ae_results", "data"),

    # Flatten filtered dm to single table
    new_link("filtered_dm", "flattened", "data")
  ),
  extensions = list(
    new_dag_extension()
  )
)
