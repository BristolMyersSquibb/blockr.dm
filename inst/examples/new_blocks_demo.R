# New Blocks Demo
#
# This example demonstrates the newly added dm blocks:
# - new_dm_read_block: Read Excel/ZIP/directory into dm
# - new_dm_write_block: Write dm to Excel/ZIP/directory
# - new_dm_select_block: Select subset of tables
# - new_dm_flatten_block: Enhanced with include_tables and join_type
# - new_dm_block: Enhanced to support dm objects as inputs

library(blockr)
library(blockr.dag)
library(blockr.dm)

# ============================================================================
# Example 1: Read/Write Workflow
# ============================================================================
#
# Demonstrates reading multiple tables from a directory, processing them,
# and writing the result back to a file.
#
# Use case: "Read data from directory, filter, export for further analysis."

# Create temp directory with test data
temp_dir <- file.path(tempdir(), "dm_demo")
dir.create(temp_dir, showWarnings = FALSE)

write.csv(
 data.frame(USUBJID = paste0("SUBJ-", 1:3), AGE = c(45, 52, 38)),
 file.path(temp_dir, "adsl.csv"), row.names = FALSE
)
write.csv(
 data.frame(USUBJID = c("SUBJ-1", "SUBJ-2"), AETERM = c("Headache", "Nausea"),
            AESEV = c("MILD", "SEVERE")),
 file.path(temp_dir, "adae.csv"), row.names = FALSE
)

if (FALSE) { # Set to TRUE to run this example
 run_app(
   blocks = c(
     # Read all CSVs from directory into dm
     dm_input = new_dm_read_block(
       path = temp_dir,
       source = "path",
       infer_keys = TRUE
     ),

     # Filter to severe adverse events
     filtered = new_dm_filter_block(
       table = "adae",
       expr = "AESEV == 'SEVERE'"
     ),

     # Write filtered dm to Excel
     output = new_dm_write_block(format = "xlsx")
   ),
   links = c(
     new_link("dm_input", "filtered", "data"),
     new_link("filtered", "output", "data")
   ),
   extensions = list(new_dag_extension())
 )
}

# ============================================================================
# Example 2: Select Tables Workflow
# ============================================================================
#
# Demonstrates selecting a subset of tables from a large dm object.
#
# Use case: "I only need ADSL and ADAE for this analysis, drop the rest."

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

adcm <- data.frame(
  USUBJID = c("SUBJ-1", "SUBJ-2", "SUBJ-3", "SUBJ-4"),
  CMTRT = c("Aspirin", "Ibuprofen", "Acetaminophen", "Aspirin")
)

run_app(
  blocks = c(
    # Load all tables
    adsl_data = new_static_block(data = adsl),
    adlb_data = new_static_block(data = adlb),
    adae_data = new_static_block(data = adae),
    adcm_data = new_static_block(data = adcm),

    # Combine into dm (all 4 tables)
    full_dm = new_dm_block(infer_keys = TRUE),

    # Select only ADSL and ADAE tables (drop ADLB and ADCM)
    # This is useful when you have many tables but only need a few
    selected_dm = new_dm_select_block(tables = c("adsl_data", "adae_data")),

    # View the selected dm structure
    view_dm = new_dm_pluck_block(table = "adae_data")
  ),
  links = c(
    # Connect all 4 tables to the dm block
    new_link("adsl_data", "full_dm", "adsl_data"),
    new_link("adlb_data", "full_dm", "adlb_data"),
    new_link("adae_data", "full_dm", "adae_data"),
    new_link("adcm_data", "full_dm", "adcm_data"),

    # Select subset of tables
    new_link("full_dm", "selected_dm", "data"),

    # View result
    new_link("selected_dm", "view_dm", "data")
  ),
  extensions = list(new_dag_extension())
)

# ============================================================================
# Example 3: Enhanced Flatten with Include Tables and Join Type
# ============================================================================
#
# Demonstrates the new include_tables and join_type parameters.
#
# Use case: "Flatten ADAE with ADSL only (not ADLB), using inner join
#            so only subjects with AEs are included."

if (FALSE) { # Alternative example
  run_app(
    blocks = c(
      adsl_data = new_static_block(data = adsl),
      adlb_data = new_static_block(data = adlb),
      adae_data = new_static_block(data = adae),

      dm_obj = new_dm_block(infer_keys = TRUE),

      # Flatten with specific options:
      # - start from adae_data
      # - only include adsl_data (not adlb_data)
      # - use inner join (only subjects with AEs)
      flattened = new_dm_flatten_block(
        start_table = "adae_data",
        include_tables = c("adsl_data"),  # Only join with ADSL
        join_type = "inner",               # Inner join
        recursive = TRUE
      )
    ),
    links = c(
      new_link("adsl_data", "dm_obj", "adsl_data"),
      new_link("adlb_data", "dm_obj", "adlb_data"),
      new_link("adae_data", "dm_obj", "adae_data"),
      new_link("dm_obj", "flattened", "data")
    ),
    extensions = list(new_dag_extension())
  )
}

# ============================================================================
# Example 4: Combining dm Objects
# ============================================================================
#
# Demonstrates the enhanced new_dm_block that can accept both
# data frames AND existing dm objects as inputs.
#
# Use case: "Merge a pre-existing dm with additional tables."

if (FALSE) { # Example with pre-existing dm
  # Create a pre-existing dm object
  existing_dm <- dm::dm(
    subjects = data.frame(
      id = 1:3,
      name = c("Alice", "Bob", "Carol")
    ),
    visits = data.frame(
      subject_id = c(1, 1, 2, 3),
      visit_date = as.Date(c("2024-01-01", "2024-02-01", "2024-01-15", "2024-01-20"))
    )
  ) |>
    dm::dm_add_pk(subjects, id) |>
    dm::dm_add_fk(visits, subject_id, subjects)

  # New data to add
  labs <- data.frame(
    subject_id = c(1, 2, 2, 3),
    test = c("CBC", "CBC", "CMP", "CBC"),
    result = c(5.2, 4.8, 102, 5.1)
  )

  run_app(
    blocks = c(
      # Existing dm object
      existing = new_static_block(data = existing_dm),

      # New table to add
      labs_data = new_static_block(data = labs),

      # Combine: dm object + data frame = merged dm
      # The existing dm's tables and keys are preserved
      # The new table is added with auto-inferred keys
      merged_dm = new_dm_block(infer_keys = TRUE),

      # View the merged result
      view = new_dm_nested_view_block(root_table = "subjects")
    ),
    links = c(
      # Both dm and data frame can be inputs
      new_link("existing", "merged_dm", "existing"),
      new_link("labs_data", "merged_dm", "labs_data"),

      new_link("merged_dm", "view", "data")
    ),
    extensions = list(new_dag_extension())
  )
}

# ============================================================================
# Example 5: Full Pipeline Demo
# ============================================================================
#
# Complete workflow showing:
# 1. Create dm from data frames
# 2. Select subset of tables
# 3. Flatten with specific join settings
# 4. View as nested table

run_app(
  blocks = c(
    # Source data
    adsl_data = new_static_block(data = adsl),
    adlb_data = new_static_block(data = adlb),
    adae_data = new_static_block(data = adae),
    adcm_data = new_static_block(data = adcm),

    # Step 1: Create dm with all tables
    full_dm = new_dm_block(infer_keys = TRUE),

    # Step 2: Select only the tables we need
    selected = new_dm_select_block(tables = c("adsl_data", "adae_data", "adlb_data")),

    # Step 3a: Flatten to single table (AE-focused)
    # Start from ADAE, join with ADSL only, use left join
    flat_ae = new_dm_flatten_block(
      start_table = "adae_data",
      include_tables = c("adsl_data"),
      join_type = "left"
    ),

    # Step 3b: Alternatively, view as nested (interactive drill-down)
    nested = new_dm_nested_view_block(root_table = "adsl_data")
  ),
  links = c(
    # Connect source tables
    new_link("adsl_data", "full_dm", "adsl_data"),
    new_link("adlb_data", "full_dm", "adlb_data"),
    new_link("adae_data", "full_dm", "adae_data"),
    new_link("adcm_data", "full_dm", "adcm_data"),

    # Select subset
    new_link("full_dm", "selected", "data"),

    # Flatten (from selected dm)
    new_link("selected", "flat_ae", "data"),

    # Nested view (from selected dm)
    new_link("selected", "nested", "data")
  ),
  extensions = list(new_dag_extension())
)
