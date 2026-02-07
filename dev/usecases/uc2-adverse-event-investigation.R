# Use Case 2: Adverse Event Investigation
# "Which patients had Grade 3+ neutropenia within 7 days of a serious AE?"
#
# Approach: create a dm from ADSL + ADAE + ADLB, then dm crossfilter to
# filter serious AEs. Pull filtered AE and lab tables, then temporal_join
# to find labs within 7 days of each serious AE.
#
# The dm crossfilter replaces the old join+flat-crossfilter:
# - No manual left_join to enrich AEs with demographics
# - ADSL demographics filterable in their own panel
# - ADLB cascades automatically to matching subjects
#
# Teal doesn't support temporal joins at all -- users must pre-compute with
# admiral. Here we make it explicit with a dedicated temporal_join_block.

library(blockr)
library(blockr.dag)

pkgload::load_all("../blockr.dm")

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
# 1. Combine ADSL, ADAE, ADLB into a dm (auto-infers USUBJID PK/FK)
# 2. dm crossfilter — filter AESER=Y in adae panel for serious events;
#    ADSL and ADLB panels cascade to matching subjects
# 3. Pull filtered ADAE and ADLB tables
# 4. Temporal join: find labs within 7 days after each serious AE
#
# Expected: SUBJ-1 febrile neutropenia (Jan 10) matches NEUT on Jan 12
#   (2 days after), SUBJ-3 neutropenia (Jan 8) matches NEUT on Jan 10
#   (2 days after)

run_app(
  blocks = c(
    adsl_data   = new_static_block(data = adsl),
    adae_data   = new_static_block(data = adae),
    adlb_data   = new_static_block(data = adlb),

    # Combine into dm with auto-inferred keys
    dm_obj      = new_dm_block(),

    # dm crossfilter: filter serious AEs, demographics cascade
    crossfilter = new_dm_crossfilter_block(
      active_dims = list(
        adae_data = c("AESER", "AESEV"),
        adsl_data = c("SEX", "AGE"),
        adlb_data = c("PARAMCD", "ATOXGR")
      ),
      filters = list(adae_data = list(AESER = "Y"))
    ),

    # Pull filtered tables for temporal join
    pull_ae     = new_dm_pull_block(table = "adae_data"),
    pull_labs   = new_dm_pull_block(table = "adlb_data"),

    # Temporal join: find labs within 7 days after each serious AE
    temporal    = new_temporal_join_block(
      by = "USUBJID",
      left_date = "ASTDT",
      right_date = "ADT",
      window_days = 7,
      direction = "after"
    )
  ),
  links = c(
    # Combine into dm
    new_link("adsl_data", "dm_obj", "adsl_data"),
    new_link("adae_data", "dm_obj", "adae_data"),
    new_link("adlb_data", "dm_obj", "adlb_data"),

    # Crossfilter on dm
    new_link("dm_obj", "crossfilter", "data"),

    # Pull tables from filtered dm
    new_link("crossfilter", "pull_ae", "data"),
    new_link("crossfilter", "pull_labs", "data"),

    # Temporal join: filtered AEs (x) with filtered labs (y)
    new_link("pull_ae", "temporal", "x"),
    new_link("pull_labs", "temporal", "y")
  )
)
