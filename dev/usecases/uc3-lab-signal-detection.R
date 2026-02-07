# Use Case 3: Lab Signal Detection
# "Show ALT distribution for patients with hepatotoxicity AEs"
#
# Approach: create a dm from ADSL + ADAE + ADLB, then dm crossfilter.
# Click Hepatotoxicity under AEDECOD in the ADAE panel — the ADLB panel
# automatically cascades to show only lab records for those subjects.
#
# The dm crossfilter replaces the old 3-block chain (join + crossfilter +
# semi_join). No manual joins needed — cascading via FK handles it all.

library(blockr)
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
# 1. Combine ADSL, ADAE, ADLB into a dm (auto-infers USUBJID PK/FK)
# 2. dm crossfilter — click Hepatotoxicity under AEDECOD in ADAE panel
#    -> ADLB panel cascades to show only labs for hepatotox subjects
#    -> ADSL panel cascades to show only matching subjects
# 3. Pull the filtered ADLB table for display
#
# Expected hepatotoxicity subjects: SUBJ-1, -2, -4, -8
# Expected ADLB output: all lab records (ALT + AST, both visits) for those 4

run_app(
  blocks = c(
    adsl_data   = new_static_block(data = adsl),
    adae_data   = new_static_block(data = adae),
    adlb_data   = new_static_block(data = adlb),

    # Combine into dm with auto-inferred keys
    dm_obj      = new_dm_block(),

    # dm crossfilter: filter by AE type, labs and demographics cascade
    crossfilter = new_dm_crossfilter_block(
      active_dims = list(
        adae_data = c("AEDECOD", "AESEV"),
        adlb_data = c("PARAMCD", "VISIT"),
        adsl_data = c("SEX", "TRT01A")
      ),
      filters = list(adae_data = list(AEDECOD = "Hepatotoxicity"))
    ),

    # Pull filtered labs
    result      = new_dm_pull_block(table = "adlb_data")
  ),
  links = c(
    # Combine into dm
    new_link("adsl_data", "dm_obj", "adsl_data"),
    new_link("adae_data", "dm_obj", "adae_data"),
    new_link("adlb_data", "dm_obj", "adlb_data"),

    # Crossfilter on dm
    new_link("dm_obj", "crossfilter", "data"),

    # Pull filtered labs
    new_link("crossfilter", "result", "data")
  )
)
