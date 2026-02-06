# Use Case 1: Population Selection
# "Show me the safety population, females over 65, in the Drug A arm"
#
# What this demonstrates:
# - Cascading filter via dm::dm_filter() and new_dm_filter_block()
# - Semi-join propagation: filtering ADSL automatically subsets child
#   tables (ADAE, ADLB) to matching subjects via foreign keys
# - Pull a downstream table (ADAE) that reflects the parent filter
#
# How it works:
# dm_filter() applies the filter expression to the specified table, then
# uses semi-joins along foreign key relationships to propagate the filter.
# Child tables (those with a FK pointing to the filtered table) are
# automatically subsetted to only rows matching the filtered parent.
# This is the core mechanism behind "cascading filters" in blockr.dm.
#
# Limitations:
# - Only one filter expression per dm_filter_block; chain multiple blocks
#   for compound cross-table filters
# - No visual crossfilter UI (click-to-filter) for multi-table data yet

library(blockr)
library(blockr.dag)
library(blockr.dm)

# --- Synthetic data ---

adsl <- data.frame(
  USUBJID = paste0("SUBJ-", 1:10),
  AGE = c(70, 55, 68, 72, 45, 80, 66, 58, 74, 63),
  SEX = c("F", "M", "F", "F", "M", "F", "M", "F", "F", "M"),
  TRT01A = c("Drug A", "Drug B", "Drug A", "Drug A", "Drug A",
             "Drug A", "Drug B", "Drug B", "Drug A", "Drug B"),
  SAFFL = c("Y", "Y", "Y", "Y", "Y", "Y", "Y", "N", "Y", "Y")
)

adae <- data.frame(
  USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-3", "SUBJ-4", "SUBJ-6",
              "SUBJ-6", "SUBJ-9", "SUBJ-2", "SUBJ-5"),
  AETERM = c("Headache", "Nausea", "Fatigue", "Dizziness", "Rash",
             "Cough", "Back pain", "Insomnia", "Vomiting"),
  AESEV = c("MILD", "MODERATE", "MILD", "SEVERE", "MILD",
            "MILD", "MODERATE", "MILD", "MODERATE")
)

adlb <- data.frame(
  USUBJID = rep(paste0("SUBJ-", 1:10), each = 2),
  PARAMCD = rep(c("ALT", "CREAT"), 10),
  AVAL = c(25, 0.9, 30, 1.1, 45, 1.3, 22, 0.8, 28, 1.0,
           55, 1.5, 35, 1.2, 20, 0.7, 40, 1.4, 32, 1.1)
)

# --- Workflow ---
#
# 1. Load ADSL, ADAE, ADLB as static blocks
# 2. Combine into a dm with infer_keys = TRUE
#    (USUBJID is unique in ADSL -> PK; non-unique in ADAE/ADLB -> FK)
# 3. Filter ADSL: safety population, females over 65, Drug A arm
#    -> This cascades to ADAE and ADLB via semi-join on USUBJID
# 4. Pull ADAE to see adverse events for the filtered population
#
# Expected filtered subjects: SUBJ-1 (F, 70, Drug A, Y),
#   SUBJ-4 (F, 72, Drug A, Y), SUBJ-6 (F, 80, Drug A, Y),
#   SUBJ-9 (F, 74, Drug A, Y)
# Expected ADAE rows: events for those 4 subjects only

run_app(
  blocks = c(
    adsl_data = new_static_block(data = adsl),
    adae_data = new_static_block(data = adae),
    adlb_data = new_static_block(data = adlb),

    dm_obj = new_dm_block(infer_keys = TRUE),

    # Cascading filter on ADSL — propagates to all child tables
    filtered_dm = new_dm_filter_block(
      table = "adsl_data",
      expr = "SAFFL == 'Y' & SEX == 'F' & AGE > 65 & TRT01A == 'Drug A'"
    ),

    # Pull the filtered ADAE table (only events for matching subjects)
    ae_results = new_dm_pull_block(table = "adae_data")
  ),
  links = c(
    new_link("adsl_data", "dm_obj", "adsl_data"),
    new_link("adae_data", "dm_obj", "adae_data"),
    new_link("adlb_data", "dm_obj", "adlb_data"),

    new_link("dm_obj", "filtered_dm", "data"),
    new_link("filtered_dm", "ae_results", "data")
  ),
  extensions = list(new_dag_extension())
)
