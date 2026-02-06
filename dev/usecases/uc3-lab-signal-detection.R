# Use Case 3: Lab Signal Detection
# "Show ALT distribution for patients with hepatotoxicity AEs"
#
# What this demonstrates:
# - Cross-table subject-level filtering: filter ADAE on a condition,
#   then use cascading filters to automatically subset ADLB to only
#   the subjects who match
# - Pull the filtered ADLB to get ALT values for downstream analysis
#   (e.g., plotting distributions)
#
# How it works:
# 1. Filter ADAE where AEDECOD == 'Hepatotoxicity'
# 2. dm_filter() cascades via semi-join on USUBJID to ADLB
#    (ADAE and ADLB both have FK -> ADSL, so filtering ADAE subjects
#     propagates through ADSL to ADLB)
# 3. Pull ADLB to get the filtered lab records
#
# Limitations:
# - No visual crossfilter UI for multi-table data (no click-to-filter
#   across tables)
# - The cascading filter works at the subject level (USUBJID), not at
#   the row level — you get ALL labs for matching subjects, not just
#   the temporally related ones
# - To further filter ADLB to just ALT records, you would need a second
#   filter block or filter the pulled data frame downstream

library(blockr)
library(blockr.dag)
library(blockr.dm)

# --- Synthetic data ---

adsl <- data.frame(
  USUBJID = paste0("SUBJ-", 1:8),
  AGE = c(52, 64, 45, 71, 38, 60, 55, 48),
  SEX = c("M", "F", "M", "F", "M", "F", "M", "F"),
  TRT01A = c("Drug A", "Drug A", "Drug B", "Drug A",
             "Drug B", "Drug A", "Drug B", "Drug A")
)

# AEs — some subjects have hepatotoxicity
adae <- data.frame(
  USUBJID = c("SUBJ-1", "SUBJ-2", "SUBJ-2", "SUBJ-3",
              "SUBJ-4", "SUBJ-5", "SUBJ-6", "SUBJ-8"),
  AEDECOD = c("Hepatotoxicity", "Hepatotoxicity", "Headache", "Nausea",
              "Hepatotoxicity", "Rash", "Fatigue", "Hepatotoxicity"),
  AESEV = c("SEVERE", "MODERATE", "MILD", "MILD",
            "SEVERE", "MILD", "MODERATE", "MODERATE")
)

# Lab data — ALT and AST for all subjects
adlb <- data.frame(
  USUBJID = rep(paste0("SUBJ-", 1:8), each = 4),
  PARAMCD = rep(c("ALT", "AST", "ALT", "AST"), 8),
  PARAM = rep(c("Alanine Aminotransferase", "Aspartate Aminotransferase",
                "Alanine Aminotransferase", "Aspartate Aminotransferase"), 8),
  VISIT = rep(c("Baseline", "Baseline", "Week 4", "Week 4"), 8),
  AVAL = c(
    25, 28, 120, 95,   # SUBJ-1: hepatotox — ALT spikes at Week 4
    30, 32, 85, 70,    # SUBJ-2: hepatotox — ALT elevated
    22, 25, 24, 23,    # SUBJ-3: normal
    28, 30, 150, 110,  # SUBJ-4: hepatotox — ALT very high
    20, 22, 21, 20,    # SUBJ-5: normal
    35, 38, 36, 35,    # SUBJ-6: normal
    24, 26, 25, 24,    # SUBJ-7: no AE, normal
    32, 34, 90, 75     # SUBJ-8: hepatotox — ALT elevated
  ),
  ANRHI = 40  # Upper limit of normal for ALT/AST
)

# --- Workflow ---
#
# 1. Load ADSL, ADAE, ADLB
# 2. Combine into dm (auto-inferred keys via USUBJID)
# 3. Filter ADAE: AEDECOD == 'Hepatotoxicity'
#    -> Cascades to ADLB: only labs for SUBJ-1, -2, -4, -8
# 4. Pull ADLB to get ALT values for the hepatotoxicity cohort
#
# The pulled ADLB will contain ALL lab records (ALT + AST, both visits)
# for subjects with hepatotoxicity AEs. To focus on ALT only, add a
# second filter block on ADLB or filter the data frame downstream.

run_app(
  blocks = c(
    adsl_data = new_static_block(data = adsl),
    adae_data = new_static_block(data = adae),
    adlb_data = new_static_block(data = adlb),

    dm_obj = new_dm_block(infer_keys = TRUE),

    # Filter ADAE to hepatotoxicity events
    # This cascades: ADLB is automatically subsetted to matching subjects
    filtered_dm = new_dm_filter_block(
      table = "adae_data",
      expr = "AEDECOD == 'Hepatotoxicity'"
    ),

    # Pull the filtered ADLB — contains labs for hepatotox subjects only
    lab_results = new_dm_pull_block(table = "adlb_data"),

    # Also flatten ADAE with ADSL for context
    flat_ae = new_dm_flatten_block(
      start_table = "adae_data",
      include_tables = "adsl_data",
      join_type = "left"
    )
  ),
  links = c(
    new_link("adsl_data", "dm_obj", "adsl_data"),
    new_link("adae_data", "dm_obj", "adae_data"),
    new_link("adlb_data", "dm_obj", "adlb_data"),

    new_link("dm_obj", "filtered_dm", "data"),
    new_link("filtered_dm", "lab_results", "data"),
    new_link("filtered_dm", "flat_ae", "data")
  ),
  extensions = list(new_dag_extension())
)
