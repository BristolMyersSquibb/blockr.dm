# Use Case 1: Population Selection
# "Show me the safety population, females over 65, in the Drug A arm"
#
# Approach: enrich ADAE with ADSL demographics via left_join, then
# crossfilter on the enriched table. No dm needed.
#
# In the crossfilter, click Y under SAFFL, F under SEX, Drug A under TRT01A.
# Use the AGE range slider to set min=65. The table updates instantly showing
# only AEs for matching subjects.
# A left_join enriches ADAE with ADSL columns, then crossfilter does the rest.

library(blockr)
library(blockr.dplyr)
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
# 2. Left join ADAE with ADSL on USUBJID to enrich AEs with demographics
# 3. Crossfilter on the enriched table — click SAFFL=Y, SEX=F, TRT01A=Drug A
#    and use the AGE range slider to set min=65
#
# Expected: clicking SAFFL=Y, SEX=F, AGE>=65, TRT01A=Drug A filters to
# AEs for SUBJ-1 (F, 70, Drug A), SUBJ-4 (F, 72, Drug A),
# SUBJ-6 (F, 80, Drug A), SUBJ-9 (F, 74, Drug A)

run_app(
  blocks = c(
    adsl_data   = new_static_block(data = adsl),
    adae_data   = new_static_block(data = adae),
    enriched    = new_join_block(type = "left_join"),
    crossfilter = new_crossfilter_block(
      dimensions = c("SAFFL", "SEX", "TRT01A", "AESEV")
    )
  ),
  links = c(
    new_link("adae_data", "enriched", "x"),
    new_link("adsl_data", "enriched", "y"),
    new_link("enriched", "crossfilter", "data")
  )
)
