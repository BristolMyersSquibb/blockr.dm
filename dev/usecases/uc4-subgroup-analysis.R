# Use Case 4: Subgroup Analysis
# "Compare efficacy between Japanese and non-Japanese patients with prior therapy"
#
# Approach: enrich ADTTE with ADSL demographics via left_join, then
# crossfilter on the flattened table. Click PTHFL=Y, then subgroup by RACE.
#
# Same as UC1 but targeting ADTTE. The flattened data has both demographics
# (RACE, ETHNIC) and TTE endpoints (AVAL, CNSR), so you can filter and
# group in one view.

library(blockr)
library(blockr.dplyr)
library(blockr.bi)
library(blockr.dag)

# --- Synthetic data ---

set.seed(42)

adsl <- data.frame(
  USUBJID = paste0("SUBJ-", 1:12),
  AGE = c(58, 65, 52, 70, 45, 62, 55, 48, 72, 60, 67, 50),
  SEX = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F"),
  RACE = c("ASIAN", "WHITE", "ASIAN", "ASIAN", "WHITE", "BLACK",
           "ASIAN", "WHITE", "ASIAN", "WHITE", "ASIAN", "BLACK"),
  ETHNIC = c("JAPANESE", "NOT REPORTED", "JAPANESE", "CHINESE",
             "NOT REPORTED", "NOT REPORTED", "JAPANESE", "NOT REPORTED",
             "JAPANESE", "NOT REPORTED", "KOREAN", "NOT REPORTED"),
  PTHFL = c("Y", "Y", "N", "Y", "Y", "N", "Y", "Y", "Y", "N", "Y", "Y"),
  TRT01A = rep(c("Drug A", "Drug B"), 6)
)

# Time-to-event data (e.g., overall survival)
adtte <- data.frame(
  USUBJID = paste0("SUBJ-", 1:12),
  PARAMCD = "OS",
  PARAM = "Overall Survival",
  AVAL = c(365, 280, 420, 310, 195, 450, 340, 260, 290, 380, 320, 230),
  CNSR = c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1),
  EVNTDESC = c("Death", "Censored", "Death", "Death", "Censored", "Death",
               "Censored", "Death", "Death", "Censored", "Death", "Censored")
)

# --- Workflow ---
#
# 1. Left join ADTTE with ADSL to enrich TTE data with demographics
# 2. Crossfilter on enriched table — click PTHFL=Y for prior therapy
# 3. The enriched view shows RACE, ETHNIC, AVAL, CNSR together
#
# Expected filtered subjects (PTHFL == 'Y'):
#   SUBJ-1 (Japanese), SUBJ-2 (White), SUBJ-4 (Chinese),
#   SUBJ-5 (White), SUBJ-7 (Japanese), SUBJ-8 (White),
#   SUBJ-9 (Japanese), SUBJ-11 (Korean), SUBJ-12 (Black)

run_app(
  blocks = c(
    adsl_data   = new_static_block(data = adsl),
    adtte_data  = new_static_block(data = adtte),
    enriched    = new_join_block(type = "left_join"),
    crossfilter = new_table_filter_block()
  ),
  links = c(
    new_link("adtte_data", "enriched", "x"),
    new_link("adsl_data", "enriched", "y"),
    new_link("enriched", "crossfilter", "data")
  )
)
