# Use Case 6: Regulatory Query Response
# "For patients with elevated creatinine, what concomitant medications
#  were they on?"
#
# Approach: enrich ADLB with ADSL demographics via left_join, crossfilter
# to select CREAT and use the AVAL range slider to set a threshold,
# then semi_join to get matching ADCM.
#
# The range slider on AVAL lets you interactively set "elevated" > 1.5.
# The crossfilter handles the PARAMCD selection. The semi_join propagates
# the subject filter to ADCM.

library(blockr)
library(blockr.dplyr)
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
# 1. Enrich ADLB with ADSL demographics via left_join
# 2. Crossfilter on enriched labs -- click CREAT under PARAMCD,
#    then use the AVAL range slider to set min=1.5 for "elevated"
# 3. Semi_join ADCM with filtered labs to get medications for matching subjects
#
# Expected subjects with elevated creatinine (AVAL >= 1.5): SUBJ-1, -3, -5, -8
# Expected ADCM records:
#   SUBJ-1: Lisinopril, Metformin
#   SUBJ-3: Atorvastatin, Ibuprofen
#   SUBJ-5: Amlodipine, Metformin
#   SUBJ-8: Furosemide

run_app(
  blocks = c(
    adsl_data     = new_static_block(data = adsl),
    adlb_data     = new_static_block(data = adlb),
    adcm_data     = new_static_block(data = adcm),

    # Enrich labs with demographics
    lab_enriched  = new_join_block(type = "left_join"),

    # Interactive filter: click CREAT, use AVAL slider to set >= 1.5
    # AVAL is auto-detected as a range dimension (>10 unique values)
    lab_filter    = new_crossfilter_block(),

    # Propagate to medications: keep ADCM rows matching filtered lab subjects
    filtered_meds = new_join_block(type = "semi_join", by = "USUBJID")
  ),
  links = c(
    # Enrich labs with ADSL
    new_link("adlb_data", "lab_enriched", "x"),
    new_link("adsl_data", "lab_enriched", "y"),

    # Crossfilter on enriched labs
    new_link("lab_enriched", "lab_filter", "data"),

    # Semi_join: ADCM (x, kept) where USUBJID matches filtered labs (y)
    new_link("adcm_data", "filtered_meds", "x"),
    new_link("lab_filter", "filtered_meds", "y")
  )
)
