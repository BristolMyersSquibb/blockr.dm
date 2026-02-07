# Use Case 4: Subgroup Analysis
# "Compare efficacy between Japanese and non-Japanese patients with prior therapy"
#
# Approach: create a dm from ADSL + ADTTE, then dm crossfilter.
# Filter PTHFL=Y in the ADSL panel — the ADTTE panel cascades to show
# only TTE endpoints for subjects with prior therapy.
#
# The dm crossfilter replaces the old join+flat-crossfilter:
# no manual left_join needed, demographics and endpoints stay in
# separate panels, cascading via FK handles the linking.

library(blockr)
library(blockr.dag)

pkgload::load_all("../blockr.dm")

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
# 1. Combine ADSL and ADTTE into a dm (auto-infers USUBJID PK/FK)
# 2. dm crossfilter — filter PTHFL=Y in ADSL panel;
#    ADTTE panel cascades to matching subjects
# 3. Pull the filtered ADTTE table for display
#
# Expected filtered subjects (PTHFL == 'Y'):
#   SUBJ-1 (Japanese), SUBJ-2 (White), SUBJ-4 (Chinese),
#   SUBJ-5 (White), SUBJ-7 (Japanese), SUBJ-8 (White),
#   SUBJ-9 (Japanese), SUBJ-11 (Korean), SUBJ-12 (Black)

run_app(
  blocks = c(
    adsl_data   = new_static_block(data = adsl),
    adtte_data  = new_static_block(data = adtte),

    # Combine into dm with auto-inferred keys
    dm_obj      = new_dm_block(),

    # dm crossfilter: filter by prior therapy, race, ethnicity
    crossfilter = new_dm_crossfilter_block(
      active_dims = list(
        adsl_data = c("PTHFL", "RACE", "ETHNIC", "TRT01A"),
        adtte_data = c("EVNTDESC")
      ),
      filters = list(adsl_data = list(PTHFL = "Y"))
    ),

    # Pull filtered TTE data
    result      = new_dm_pull_block(table = "adtte_data")
  ),
  links = c(
    # Combine into dm
    new_link("adsl_data", "dm_obj", "adsl_data"),
    new_link("adtte_data", "dm_obj", "adtte_data"),

    # Crossfilter on dm
    new_link("dm_obj", "crossfilter", "data"),

    # Pull filtered TTE
    new_link("crossfilter", "result", "data")
  )
)
