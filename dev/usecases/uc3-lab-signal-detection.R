# Use Case 3: Lab Signal Detection
# "Show ALT distribution for patients with hepatotoxicity AEs"
#
# Approach: enrich ADAE with ADSL demographics via left_join, crossfilter
# to select hepatotoxicity, then semi_join to get matching ADLB records.
#
# The crossfilter on enriched ADAE lets you click Hepatotoxicity under
# AEDECOD. The semi_join propagates to ADLB — you get all lab records for
# hepatotoxicity subjects. This is the "filter child, get sibling" pattern.
# Note semi_join direction: ADLB is x (kept), filtered AE is y (match against).

library(blockr)
library(blockr.dplyr)
library(blockr.dag)

pkgload::load_all("../blockr.dm")

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
# 1. Enrich ADAE with ADSL demographics via left_join
# 2. Crossfilter on enriched AEs — click Hepatotoxicity under AEDECOD
# 3. Semi_join ADLB with filtered AEs to get labs for matching subjects
#
# The semi_join keeps all ADLB rows where USUBJID matches the filtered AEs.
# Expected hepatotoxicity subjects: SUBJ-1, -2, -4, -8
# Expected ADLB output: all lab records (ALT + AST, both visits) for those 4

run_app(
  blocks = c(
    adsl_data     = new_static_block(data = adsl),
    adae_data     = new_static_block(data = adae),
    adlb_data     = new_static_block(data = adlb),

    # Enrich AEs with demographics
    ae_enriched   = new_join_block(type = "left_join"),

    # Interactive filter: click Hepatotoxicity under AEDECOD
    ae_filter     = new_crossfilter_block(),

    # Propagate to labs: keep ADLB rows matching filtered AE subjects
    filtered_labs = new_join_block(type = "semi_join", by = "USUBJID")
  ),
  links = c(
    # Enrich AEs with ADSL
    new_link("adae_data", "ae_enriched", "x"),
    new_link("adsl_data", "ae_enriched", "y"),

    # Crossfilter on enriched AEs
    new_link("ae_enriched", "ae_filter", "data"),

    # Semi_join: ADLB (x, kept) where USUBJID matches filtered AEs (y)
    new_link("adlb_data", "filtered_labs", "x"),
    new_link("ae_filter", "filtered_labs", "y")
  )
)
