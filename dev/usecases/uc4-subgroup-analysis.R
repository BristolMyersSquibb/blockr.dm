# Use Case 4: Subgroup Analysis
# "Compare efficacy between Japanese and non-Japanese patients with prior therapy"
#
# What this demonstrates:
# - Population subsetting: filter ADSL on RACE and prior therapy flag,
#   then cascade to a downstream efficacy table (ADTTE)
# - Same cascading mechanism as UC1, but targeting a different downstream
#   table (time-to-event instead of adverse events)
# - Pull ADTTE for downstream survival analysis (e.g., KM plot)
#
# How it works:
# Filter ADSL to patients with prior therapy (PTHFL == 'Y'), then
# cascade to ADTTE. The pulled ADTTE can be used with downstream
# blockr blocks or external analysis tools for Kaplan-Meier estimation.
#
# Limitations:
# - blockr.dm does not include built-in survival analysis or KM plots;
#   the pulled data frame would need to be analyzed externally or via
#   a custom blockr block
# - To compare subgroups (Japanese vs non-Japanese), you would need to
#   pull the full prior-therapy population and group downstream, or
#   run two parallel filtered pipelines

library(blockr)
library(blockr.dag)
library(blockr.dm)

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
# 1. Load ADSL and ADTTE
# 2. Combine into dm (USUBJID links them)
# 3. Filter ADSL: prior therapy patients only (PTHFL == 'Y')
#    -> Cascades to ADTTE
# 4. Pull ADTTE for the filtered subgroup
#
# The pulled ADTTE includes all prior-therapy patients. To compare
# Japanese vs non-Japanese, group the result by RACE/ETHNIC downstream.
#
# Expected filtered subjects (PTHFL == 'Y'):
#   SUBJ-1 (Japanese), SUBJ-2 (White), SUBJ-4 (Chinese),
#   SUBJ-5 (White), SUBJ-7 (Japanese), SUBJ-8 (White),
#   SUBJ-9 (Japanese), SUBJ-11 (Korean), SUBJ-12 (Black)

run_app(
  blocks = c(
    adsl_data = new_static_block(data = adsl),
    adtte_data = new_static_block(data = adtte),

    dm_obj = new_dm_block(infer_keys = TRUE),

    # Filter to prior therapy population
    filtered_dm = new_dm_filter_block(
      table = "adsl_data",
      expr = "PTHFL == 'Y'"
    ),

    # Pull ADTTE — only records for prior-therapy patients
    tte_results = new_dm_pull_block(table = "adtte_data"),

    # Also flatten ADTTE with ADSL to get demographics alongside TTE data
    # Useful for grouping by RACE/ETHNIC in downstream analysis
    flat_tte = new_dm_flatten_block(
      start_table = "adtte_data",
      include_tables = "adsl_data",
      join_type = "inner"
    )
  ),
  links = c(
    new_link("adsl_data", "dm_obj", "adsl_data"),
    new_link("adtte_data", "dm_obj", "adtte_data"),

    new_link("dm_obj", "filtered_dm", "data"),
    new_link("filtered_dm", "tte_results", "data"),
    new_link("filtered_dm", "flat_tte", "data")
  ),
  extensions = list(new_dag_extension())
)
