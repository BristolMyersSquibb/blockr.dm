# Use Case 2: Adverse Event Investigation
# "Which patients had Grade 3+ neutropenia within 7 days of a serious AE?"
#
# What this demonstrates:
# - Temporal join via new_dm_temporal_join_block(): join ADAE and ADLB
#   within a time window to assess causal relationships
# - This is a blockr.dm-specific feature not available in teal
# - The temporal join block outputs a data frame with a `days_diff` column
#   showing the number of days between the AE and the lab measurement
#
# How it works:
# new_dm_temporal_join_block() takes a dm, joins two tables by their
# common key (USUBJID), computes days_diff = right_date - left_date,
# and filters to rows within the specified window. The `direction`
# parameter controls whether to look "after", "before", or "around"
# the anchor event.
#
# Limitations:
# - The temporal join returns a flat data frame, not a dm; downstream
#   dm operations are not possible on the result
# - Cannot chain multiple temporal joins in sequence
# - No built-in visualization of the temporal relationship (timeline)

library(blockr)
library(blockr.dag)
library(blockr.dm)

# --- Synthetic data ---

adsl <- data.frame(
  USUBJID = paste0("SUBJ-", 1:6),
  AGE = c(55, 62, 48, 70, 45, 58),
  SEX = c("M", "F", "M", "F", "M", "F"),
  TRT01A = c("Drug A", "Drug B", "Drug A", "Drug A", "Drug B", "Drug A")
)

# Adverse events with seriousness and severity
adae <- data.frame(
  USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-2", "SUBJ-3", "SUBJ-4",
              "SUBJ-4", "SUBJ-6"),
  AETERM = c("Febrile neutropenia", "Headache", "Nausea",
             "Neutropenia", "Rash", "Febrile neutropenia", "Fatigue"),
  AESEV = c("SEVERE", "MILD", "MODERATE",
            "SEVERE", "MILD", "SEVERE", "MILD"),
  AESER = c("Y", "N", "N", "Y", "N", "Y", "N"),
  ASTDT = as.Date(c(
    "2024-01-10", "2024-01-15",  # SUBJ-1
    "2024-01-12",                # SUBJ-2
    "2024-01-08",                # SUBJ-3
    "2024-01-20", "2024-01-25",  # SUBJ-4
    "2024-01-18"                 # SUBJ-6
  ))
)

# Lab data with neutrophil counts and toxicity grades
adlb <- data.frame(
  USUBJID = rep(paste0("SUBJ-", 1:6), each = 3),
  PARAMCD = rep(c("NEUT", "WBC", "NEUT"), 6),
  PARAM = rep(c("Neutrophils", "White Blood Cells", "Neutrophils"), 6),
  AVAL = c(
    0.8, 5.5, 0.5,   # SUBJ-1: low neutrophils
    3.2, 7.1, 3.0,   # SUBJ-2: normal
    0.9, 4.8, 0.3,   # SUBJ-3: low neutrophils
    1.5, 6.2, 1.2,   # SUBJ-4: borderline
    4.0, 8.0, 3.8,   # SUBJ-5: normal (no AE)
    2.5, 6.5, 2.2    # SUBJ-6: normal
  ),
  ATOXGR = c(
    "3", "0", "4",    # SUBJ-1: Grade 3 and 4 neutropenia
    "0", "0", "0",    # SUBJ-2: normal
    "3", "0", "4",    # SUBJ-3: Grade 3 and 4 neutropenia
    "2", "0", "2",    # SUBJ-4: Grade 2
    "0", "0", "0",    # SUBJ-5: normal
    "0", "0", "0"     # SUBJ-6: normal
  ),
  ADT = as.Date(c(
    "2024-01-08", "2024-01-08", "2024-01-12",  # SUBJ-1
    "2024-01-10", "2024-01-10", "2024-01-15",  # SUBJ-2
    "2024-01-06", "2024-01-06", "2024-01-10",  # SUBJ-3
    "2024-01-18", "2024-01-18", "2024-01-26",  # SUBJ-4
    "2024-01-05", "2024-01-05", "2024-01-12",  # SUBJ-5
    "2024-01-15", "2024-01-15", "2024-01-20"   # SUBJ-6
  ))
)

# --- Workflow ---
#
# 1. Load ADSL, ADAE, ADLB
# 2. Combine into dm with auto-inferred keys
# 3. Filter ADAE to serious AEs (AESER == 'Y')
# 4. Temporal join: for each serious AE, find labs within 7 days BEFORE
#    the AE start date (to see if neutropenia preceded or coincided)
#
# Expected: SUBJ-1's febrile neutropenia (Jan 10) matches NEUT on Jan 8
#   (2 days before), SUBJ-3's neutropenia (Jan 8) matches NEUT on Jan 6
#   (2 days before), SUBJ-4's febrile neutropenia (Jan 25) matches
#   NEUT on Jan 26 (1 day after — outside "before" window)

run_app(
  blocks = c(
    adsl_data = new_static_block(data = adsl),
    adae_data = new_static_block(data = adae),
    adlb_data = new_static_block(data = adlb),

    dm_obj = new_dm_block(infer_keys = TRUE),

    # Filter to serious adverse events only
    serious_dm = new_dm_filter_block(
      table = "adae_data",
      expr = "AESER == 'Y'"
    ),

    # Temporal join: find labs within 7 days BEFORE each serious AE
    # direction = "before" means: right_date is 0-7 days before left_date
    temporal_result = new_dm_temporal_join_block(
      left_table = "adae_data",
      left_date = "ASTDT",
      right_table = "adlb_data",
      right_date = "ADT",
      window_days = 7,
      direction = "before"
    )
  ),
  links = c(
    new_link("adsl_data", "dm_obj", "adsl_data"),
    new_link("adae_data", "dm_obj", "adae_data"),
    new_link("adlb_data", "dm_obj", "adlb_data"),

    new_link("dm_obj", "serious_dm", "data"),

    # Temporal join takes the filtered dm
    new_link("serious_dm", "temporal_result", "data")
  ),
  extensions = list(new_dag_extension())
)
