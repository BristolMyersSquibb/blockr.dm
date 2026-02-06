# Use Case 2: Adverse Event Investigation
# "Which patients had Grade 3+ neutropenia within 7 days of a serious AE?"
#
# Approach: filter ADAE to serious events via crossfilter, enrich with ADSL
# demographics, then join ADLB with a time window using mutate + filter.
#
# Teal doesn't support temporal joins at all — users must pre-compute with
# admiral. Here we make it explicit: join AE and lab data, compute days_diff,
# filter to window. More steps but fully transparent. A dedicated
# temporal_join block (like blockr.dm has) could wrap this for convenience.

library(blockr)
library(blockr.dplyr)
library(blockr.bi)
library(blockr.dag)

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
# 1. Enrich ADAE with ADSL demographics via left_join
# 2. Crossfilter on enriched AEs — click AESER=Y to select serious events
# 3. Left join filtered serious AEs with ADLB on USUBJID (many-to-many)
# 4. Compute days_diff = ADT - ASTDT (lab date minus AE start date)
# 5. Filter to rows within 7-day window (days_diff >= 0 & days_diff <= 7)
#
# This is 5 blocks vs 1 dm temporal_join_block, but every step is visible.
# Expected: SUBJ-1 febrile neutropenia (Jan 10) matches NEUT on Jan 12
#   (2 days after), SUBJ-3 neutropenia (Jan 8) matches NEUT on Jan 10
#   (2 days after)

run_app(
  blocks = c(
    adsl_data   = new_static_block(data = adsl),
    adae_data   = new_static_block(data = adae),
    adlb_data   = new_static_block(data = adlb),

    # Enrich AEs with demographics
    ae_enriched = new_join_block(type = "left_join"),

    # Interactive filter: click AESER=Y for serious events
    ae_filter   = new_table_filter_block(),

    # Join filtered serious AEs with lab data (many-to-many on USUBJID)
    ae_lab_join = new_join_block(type = "left_join", by = "USUBJID"),

    # Compute days between lab measurement and AE start
    temporal    = new_mutate_expr_block(
      exprs = list(days_diff = "as.numeric(ADT - ASTDT)")
    ),

    # Keep only labs within 7 days after AE start
    window      = new_filter_expr_block(
      exprs = "days_diff >= 0 & days_diff <= 7"
    )
  ),
  links = c(
    # Enrich AEs with ADSL
    new_link("adae_data", "ae_enriched", "x"),
    new_link("adsl_data", "ae_enriched", "y"),

    # Crossfilter on enriched AEs
    new_link("ae_enriched", "ae_filter", "data"),

    # Join filtered AEs with labs
    new_link("ae_filter", "ae_lab_join", "x"),
    new_link("adlb_data", "ae_lab_join", "y"),

    # Compute time difference and filter to window
    new_link("ae_lab_join", "temporal", "data"),
    new_link("temporal", "window", "data")
  )
)
