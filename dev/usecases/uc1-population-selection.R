# Use Case 1: Population Selection
# "Show me the safety population, females over 65, in the Drug A arm"
#
# Approach: create a dm from ADSL + ADAE, then dm crossfilter for cross-table
# filtering. Filter SAFFL=Y, SEX=F, TRT01A=Drug A, AGE>=65 in the ADSL panel.
# The ADAE panel cascades automatically — only AEs for matching subjects.
#
# The dm crossfilter replaces the old join+flat-crossfilter pattern:
# no manual left_join needed, and filters cascade bidirectionally.

library(blockr)
library(blockr.dag)

pkgload::load_all("../blockr.dm")

# --- Synthetic data ---

adsl <- data.frame(
  USUBJID = paste0("SUBJ-", 1:20),
  AGE = c(70, 55, 68, 72, 45, 80, 66, 58, 74, 63,
          42, 77, 51, 69, 83, 60, 47, 73, 56, 65),
  SEX = c("F", "M", "F", "F", "M", "F", "M", "F", "F", "M",
          "M", "F", "M", "F", "F", "M", "F", "M", "F", "M"),
  TRT01A = c("Drug A", "Drug B", "Drug A", "Drug A", "Drug A",
             "Drug A", "Drug B", "Drug B", "Drug A", "Drug B",
             "Drug A", "Drug A", "Drug B", "Drug A", "Drug B",
             "Drug A", "Drug B", "Drug A", "Drug A", "Drug B"),
  SAFFL = c("Y", "Y", "Y", "Y", "Y", "Y", "Y", "N", "Y", "Y",
            "Y", "Y", "Y", "Y", "N", "Y", "Y", "Y", "Y", "Y")
)

adae <- data.frame(
  USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-3", "SUBJ-4", "SUBJ-6",
              "SUBJ-6", "SUBJ-9", "SUBJ-2", "SUBJ-5", "SUBJ-12",
              "SUBJ-12", "SUBJ-14", "SUBJ-16", "SUBJ-18", "SUBJ-19"),
  AETERM = c("Headache", "Nausea", "Fatigue", "Dizziness", "Rash",
             "Cough", "Back pain", "Insomnia", "Vomiting", "Headache",
             "Arthralgia", "Pyrexia", "Cough", "Nausea", "Fatigue"),
  AESEV = c("MILD", "MODERATE", "MILD", "SEVERE", "MILD",
            "MILD", "MODERATE", "MILD", "MODERATE", "MILD",
            "SEVERE", "MILD", "MODERATE", "MILD", "MODERATE")
)

# --- Workflow ---
#
# 1. Load ADSL and ADAE as static blocks
# 2. Combine into a dm (auto-infers USUBJID PK/FK)
# 3. dm crossfilter — ADSL panel shows SAFFL, SEX, TRT01A, AGE;
#    ADAE panel shows AESEV. Filters cascade across tables.
# 4. Pull the filtered ADAE table for display
#
# Pre-applied: SAFFL=Y, SEX=F, TRT01A=Drug A, AGE>=65
# Expected: AEs for SUBJ-1 (F, 70), SUBJ-4 (F, 72),
#           SUBJ-6 (F, 80), SUBJ-9 (F, 74)

run_app(
  blocks = c(
    adsl_data   = new_static_block(data = adsl),
    adae_data   = new_static_block(data = adae),
    dm_obj      = new_dm_block(),
    crossfilter = new_dm_crossfilter_block(
      active_dims = list(
        adsl_data = c("SAFFL", "SEX", "TRT01A", "AGE"),
        adae_data = c("AESEV")
      ),
      filters = list(adsl_data = list(SAFFL = "Y", SEX = "F", TRT01A = "Drug A")),
      range_filters = list(adsl_data = list(AGE = c(65, 100)))
    ),
    result      = new_dm_pull_block(table = "adae_data")
  ),
  links = c(
    new_link("adsl_data", "dm_obj", "adsl_data"),
    new_link("adae_data", "dm_obj", "adae_data"),
    new_link("dm_obj", "crossfilter", "data"),
    new_link("crossfilter", "result", "data")
  )
)
