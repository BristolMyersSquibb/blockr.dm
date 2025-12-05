# ADaM Workflow Example
#
# This example demonstrates using blockr.dm to query across related
# pharmaceutical datasets (ADaM format).
#
# Use case: "For subjects with Neutrophil counts greater than X,
#            what are the corresponding adverse events?"

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

# Example workflow using blockr with DAG board (visual node-based interface)
#
# This demonstrates the full pipeline:
# 1. Load three ADaM tables (ADSL, ADLB, ADAE)
# 2. Combine into a dm object (shows interactive diagram)
# 3. Add primary/foreign key relationships (diagram shows connections)
# 4. Filter by lab condition (subjects with high neutrophils)
# 5. Extract the adverse events for those subjects (pluck)
# 6. Flatten to a single table with columns from both adae and adsl

run_app(
 blocks = c(
   # Load the three datasets using new_static_block
   adsl_data = new_static_block(data = adsl),
   adlb_data = new_static_block(data = adlb),
   adae_data = new_static_block(data = adae),

   # Combine into dm object - shows diagram with 3 unconnected tables
   dm_obj = new_dm_block(),

   # Add primary key (USUBJID in adsl) and foreign key from adlb
   # Diagram now shows arrow from adlb_data to adsl_data
   dm_keys1 = new_dm_add_keys_block(
     pk_table = "adsl_data",
     pk_column = "USUBJID",
     fk_table = "adlb_data",
     fk_column = "USUBJID"
   ),

   # Add another foreign key (adae -> adsl)
   # Diagram now shows both adlb and adae connected to adsl
   dm_keys2 = new_dm_add_keys_block(
     pk_table = "adsl_data",
     pk_column = "USUBJID",
     fk_table = "adae_data",
     fk_column = "USUBJID"
   ),

   # Filter by lab condition (e.g., high neutrophils)
   # PARAMCD == "NEUT" filters to neutrophil records
   # AVAL > 5 filters to values above threshold
   # This filter CASCADES to related tables via foreign keys!
   filtered_dm = new_dm_filter_block(
     table = "adlb_data",
     expr = "PARAMCD == 'NEUT' & AVAL > 5"
   ),

   # Extract the adverse events for filtered subjects
   # Returns a data frame with only AEs for subjects meeting the lab criteria
   ae_results = new_dm_pluck_block(table = "adae_data"),

   # Flatten the filtered dm: join adae with adsl to get a single table
   # Result has columns from both: USUBJID, AETERM, AESEV, AGE, SEX
   # Only includes subjects who met the filter criteria!
   flattened = new_dm_flatten_block(start_table = "adae_data")
 ),
 links = c(
   # Connect datasets to dm block using NAMED inputs
   # The input name becomes the table name in the dm object
   new_link("adsl_data", "dm_obj", "adsl_data"),
   new_link("adlb_data", "dm_obj", "adlb_data"),
   new_link("adae_data", "dm_obj", "adae_data"),

   # Add keys (chained: dm_obj -> dm_keys1 -> dm_keys2)
   new_link("dm_obj", "dm_keys1", "data"),
   new_link("dm_keys1", "dm_keys2", "data"),

   # Filter the dm
   new_link("dm_keys2", "filtered_dm", "data"),

   # Extract AE table (pluck returns data frame)
   new_link("filtered_dm", "ae_results", "data"),

   # Flatten the filtered dm to single table
   # This shows AEs with subject demographics for subjects with high neutrophils
   new_link("filtered_dm", "flattened", "data")
 ),
 extensions = list(
   new_dag_extension()
 )
)
