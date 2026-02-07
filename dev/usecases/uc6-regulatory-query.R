# Use Case 6: Regulatory Query Response (dm crossfilter, 3 tables)
# "For patients with elevated creatinine, what concomitant medications
#  were they on?"
#
# Approach: create a dm object from ADSL + ADLB + ADCM, then use dm crossfilter
# for cross-table filtering across all three tables. Filter PARAMCD=CREAT in
# the ADLB panel, use the AVAL range slider to set min=1.5, and the ADCM panel
# instantly shows only medications for matching subjects.
#
# A downstream dm_pull_block extracts the filtered ADCM table.

library(blockr)
library(blockr.dag)

pkgload::load_all("../blockr.dm")

# --- Synthetic data ---

adsl <- data.frame(
  USUBJID = paste0("SUBJ-", 1:8),
  AGE = c(65, 52, 70, 45, 58, 72, 48, 63),
  SEX = c("M", "F", "M", "F", "M", "F", "M", "F"),
  TRT01A = c("Drug A", "Drug B", "Drug A", "Drug B",
             "Drug A", "Drug A", "Drug B", "Drug A")
)

# Lab data with creatinine values
adlb <- data.frame(
  USUBJID = rep(paste0("SUBJ-", 1:8), each = 3),
  PARAMCD = rep(c("CREAT", "ALT", "AST"), 8),
  PARAM = rep(c("Creatinine", "Alanine Aminotransferase",
                "Aspartate Aminotransferase"), 8),
  AVAL = c(
    1.8, 30, 28,   # SUBJ-1: elevated creatinine
    0.9, 25, 22,   # SUBJ-2: normal
    2.1, 35, 32,   # SUBJ-3: elevated creatinine
    1.0, 22, 20,   # SUBJ-4: normal
    1.6, 40, 38,   # SUBJ-5: elevated creatinine
    1.2, 28, 25,   # SUBJ-6: normal
    0.8, 20, 18,   # SUBJ-7: normal
    1.9, 45, 42    # SUBJ-8: elevated creatinine
  )
)

# Concomitant medications
adcm <- data.frame(
  USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-2", "SUBJ-3", "SUBJ-3",
              "SUBJ-4", "SUBJ-5", "SUBJ-5", "SUBJ-6", "SUBJ-8"),
  CMTRT = c("Lisinopril", "Metformin", "Aspirin", "Atorvastatin",
            "Ibuprofen", "Omeprazole", "Amlodipine", "Metformin",
            "Aspirin", "Furosemide"),
  CMDECOD = c("LISINOPRIL", "METFORMIN", "ACETYLSALICYLIC ACID",
              "ATORVASTATIN", "IBUPROFEN", "OMEPRAZOLE",
              "AMLODIPINE", "METFORMIN", "ACETYLSALICYLIC ACID",
              "FUROSEMIDE"),
  CMCLAS = c("ACE inhibitor", "Antidiabetic", "Analgesic",
             "Lipid-lowering", "NSAID", "PPI",
             "Calcium channel blocker", "Antidiabetic",
             "Analgesic", "Diuretic")
)

# --- Workflow ---
#
# 1. Load ADSL, ADLB, ADCM as static blocks
# 2. Combine into a dm object (auto-infers USUBJID PK/FK)
# 3. dm crossfilter shows per-table panels for all 3 tables
#    Click CREAT under PARAMCD in ADLB panel, use AVAL slider min=1.5
#    -> ADCM panel updates to show only meds for subjects with elevated creatinine
# 4. Pull the filtered ADCM table for display
#
# Expected subjects with elevated creatinine (AVAL >= 1.5): SUBJ-1, -3, -5, -8
# Expected ADCM records:
#   SUBJ-1: Lisinopril, Metformin
#   SUBJ-3: Atorvastatin, Ibuprofen
#   SUBJ-5: Amlodipine, Metformin
#   SUBJ-8: Furosemide

run_app(
  blocks = c(
    adsl_data   = new_static_block(data = adsl),
    adlb_data   = new_static_block(data = adlb),
    adcm_data   = new_static_block(data = adcm),
    dm_obj      = new_dm_block(),
    crossfilter = new_dm_crossfilter_block(
      active_dims = list(
        adlb_data = c("PARAMCD", "AVAL"),
        adsl_data = c("SEX", "AGE"),
        adcm_data = c("CMCLAS")
      )
    ),
    result      = new_dm_pull_block(table = "adcm_data")
  ),
  links = c(
    new_link("adsl_data", "dm_obj", "adsl_data"),
    new_link("adlb_data", "dm_obj", "adlb_data"),
    new_link("adcm_data", "dm_obj", "adcm_data"),
    new_link("dm_obj", "crossfilter", "data"),
    new_link("crossfilter", "result", "data")
  )
)
