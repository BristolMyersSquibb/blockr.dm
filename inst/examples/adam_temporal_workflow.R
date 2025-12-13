# ADaM Temporal Workflow Example
#
# This example demonstrates a more complex query pattern:
# "For each adverse event, find lab values measured within X days,
#  to assess temporal patterns for causality assessment."
#
# This is a common safety analysis question in clinical trials.
# See notes/ae-lb-temporal-queries.md for background.

library(blockr)
library(blockr.dag)
library(blockr.dm)
library(dplyr)

# =============================================================================
# Create test data with DATES (simulating ADaM structure)
# =============================================================================

adsl <- data.frame(
  USUBJID = paste0("SUBJ-", 1:5),
  AGE = c(45, 52, 38, 61, 29),
  SEX = c("M", "F", "M", "F", "M"),
  TRTSDT = as.Date(c("2024-01-01", "2024-01-02", "2024-01-01",
                     "2024-01-03", "2024-01-02"))
)

# Lab data with dates
adlb <- data.frame(
  USUBJID = rep(paste0("SUBJ-", 1:5), each = 4),
  PARAMCD = rep(c("ALT", "AST"), 10),
  PARAM = rep(c("Alanine Aminotransferase", "Aspartate Aminotransferase"), 10),
  ADT = as.Date(c(
    # SUBJ-1: baseline + day 8, 15
    "2024-01-01", "2024-01-01", "2024-01-08", "2024-01-15",
    # SUBJ-2: baseline + day 10, 20
    "2024-01-02", "2024-01-02", "2024-01-12", "2024-01-22",
    # SUBJ-3: baseline + day 7, 14
    "2024-01-01", "2024-01-01", "2024-01-08", "2024-01-15",
    # SUBJ-4: baseline + day 5, 12
    "2024-01-03", "2024-01-03", "2024-01-08", "2024-01-15",
    # SUBJ-5: baseline + day 10, 20
    "2024-01-02", "2024-01-02", "2024-01-12", "2024-01-22"
  )),
  AVAL = c(
    # SUBJ-1: normal baseline, elevated after AE
    25, 28, 45, 30,
    # SUBJ-2: normal throughout
    22, 25, 24, 23,
    # SUBJ-3: normal throughout (no AE)
    20, 22, 21, 20,
    # SUBJ-4: elevated after AE
    28, 30, 55, 35,
    # SUBJ-5: normal throughout (no AE)
    24, 26, 25, 24
  ),
  ANRLO = 10,
  ANRHI = 40  # Normal range: 10-40 U/L
)

# Adverse events with start/end dates
adae <- data.frame(
  USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-2", "SUBJ-4", "SUBJ-4"),
  AESEQ = c(1, 2, 1, 1, 2),
  AETERM = c("DIARRHOEA", "HEADACHE", "FATIGUE", "NAUSEA", "RASH"),
  AESEV = c("MODERATE", "MILD", "MILD", "MODERATE", "MILD"),
  ASTDT = as.Date(c(
    "2024-01-05",  # SUBJ-1: Diarrhoea starts day 5
    "2024-01-10",  # SUBJ-1: Headache starts day 10
    "2024-01-08",  # SUBJ-2: Fatigue starts day 7
    "2024-01-04",  # SUBJ-4: Nausea starts day 2
    "2024-01-10"   # SUBJ-4: Rash starts day 8
  )),
  AENDT = as.Date(c(
    "2024-01-07",  # SUBJ-1: Diarrhoea ends day 7 (before lab on day 8)
    "2024-01-12",  # SUBJ-1: Headache ends day 12
    "2024-01-15",  # SUBJ-2: Fatigue ongoing
    "2024-01-06",  # SUBJ-4: Nausea ends day 4 (before lab on day 5)
    NA             # SUBJ-4: Rash ongoing
  ))
)

# =============================================================================
# The Question:
# "For each AE, find labs within 7 days to assess causality"
#
# Expected findings:
# - SUBJ-1: DIARRHOEA (Jan 5-7) -> Lab on Jan 8 shows ALT=45 (HIGH) <- 1 day after AE
# - SUBJ-4: NAUSEA (Jan 4-6) -> Lab on Jan 8 shows ALT=55 (HIGH) <- 2 days after AE
# =============================================================================

# =============================================================================
# APPROACH 1: What current blockr.dm CAN do
# Filter by subject (key-based), then manually inspect
# =============================================================================

cat("\n=== Current blockr.dm approach ===\n")
cat("Can filter to subjects WITH adverse events, but cannot do temporal join.\n\n")

# This works: filter to subjects who have AEs
# But it returns ALL labs for those subjects, not just temporally related ones

# =============================================================================
# APPROACH 2: What we NEED - temporal join (pure dplyr for now)
# =============================================================================

cat("=== Temporal join (dplyr) ===\n\n")

WINDOW_DAYS <- 7

ae_with_labs <- adae |>
  inner_join(
    adlb |> select(USUBJID, PARAMCD, ADT, AVAL, ANRHI),
    by = "USUBJID",
    relationship = "many-to-many"
  ) |>
  mutate(
    days_from_ae = as.numeric(ADT - ASTDT),
    abnormal = if_else(AVAL > ANRHI, "HIGH", "NORMAL")
  ) |>
  # Keep labs within window of AE START
  filter(days_from_ae >= 0, days_from_ae <= WINDOW_DAYS) |>
  select(
    USUBJID, AETERM, AESEV, AE_START = ASTDT, AE_END = AENDT,
    PARAMCD, LAB_DATE = ADT, days_from_ae, AVAL, abnormal
  ) |>
  arrange(USUBJID, AE_START, days_from_ae)

print(ae_with_labs)

cat("\n=== Causality Assessment Summary ===\n\n")

ae_with_labs |>
  filter(abnormal == "HIGH") |>
  mutate(
    narrative = paste0(
      USUBJID, ": ", AETERM, " (", AE_START, " to ", AE_END, ") -> ",
      PARAMCD, "=", AVAL, " on ", LAB_DATE, " (", days_from_ae, " days after AE start)"
    )
  ) |>
  pull(narrative) |>
  cat(sep = "\n")

# =============================================================================
# APPROACH 3: Using dm with zoom (keeps dm structure)
# =============================================================================

cat("\n\n=== Using dm with temporal join ===\n\n")

library(dm)

adam_dm <- dm(adsl = adsl, adlb = adlb, adae = adae) |>
  dm_add_pk(adsl, USUBJID) |>
  dm_add_pk(adae, c(USUBJID, AESEQ)) |>
  dm_add_fk(adlb, USUBJID, adsl) |>
  dm_add_fk(adae, USUBJID, adsl)

# Zoom to adae, join labs temporally, insert as new table
result_dm <- adam_dm |>
  dm_zoom_to(adae) |>
  left_join(adlb, by = "USUBJID", relationship = "many-to-many") |>
  mutate(
    days_from_ae = as.numeric(ADT - ASTDT),
    abnormal = if_else(AVAL > ANRHI, "HIGH", "NORMAL")
  ) |>
  filter(days_from_ae >= 0, days_from_ae <= WINDOW_DAYS) |>
  dm_insert_zoomed("ae_lab_temporal")

cat("dm structure with derived table:\n")
print(result_dm)

cat("\nDerived table (ae_lab_temporal):\n")
result_dm$ae_lab_temporal |>
  select(USUBJID, AETERM, ASTDT, PARAMCD, ADT, days_from_ae, AVAL, abnormal) |>
  print()

# =============================================================================
# APPROACH 4: Using new_dm_temporal_join_block (blockr.dm)
# =============================================================================
#
# The temporal join block:
# - Input: dm object (with multiple tables)
# - Output: data frame (joined result)
#
# It does:
# 1. Joins left and right tables by common key (USUBJID)
# 2. Calculates days_diff = right_date - left_date
# 3. Filters to rows within window based on direction
# 4. Returns the result as a data frame
#
# =============================================================================

# =============================================================================
# Run the visual blockr app with temporal join
# =============================================================================

if (interactive()) {
  run_app(
    blocks = c(
      # Load datasets
      adsl_data = new_static_block(data = adsl),
      adlb_data = new_static_block(data = adlb),
      adae_data = new_static_block(data = adae),

      # Combine into dm with auto-detected keys
      dm_obj = new_dm_block(infer_keys = TRUE),

      # Temporal join: find labs within 7 days after AE start
      # Returns a data frame directly - no pluck needed!
      result = new_dm_temporal_join_block(
        left_table = "adae_data",
        left_date = "ASTDT",
        right_table = "adlb_data",
        right_date = "ADT",
        window_days = 7,
        direction = "after"
      )
    ),
    links = c(
      # Connect datasets to dm
      new_link("adsl_data", "dm_obj", "adsl_data"),
      new_link("adlb_data", "dm_obj", "adlb_data"),
      new_link("adae_data", "dm_obj", "adae_data"),

      # Temporal join takes dm, outputs data frame
      new_link("dm_obj", "result", "data")
    ),
    extensions = list(new_dag_extension())
  )
}
