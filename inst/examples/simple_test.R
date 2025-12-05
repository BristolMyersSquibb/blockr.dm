# Simple test of blockr.dm
#
# Tests the core dm functionality without requiring external packages

library(dm)

# Create test data simulating ADaM structure
adsl <- data.frame(
  USUBJID = paste0("SUBJ-", 1:5),
  AGE = c(45, 52, 38, 61, 29),
  SEX = c("M", "F", "M", "F", "M"),
  stringsAsFactors = FALSE
)

adlb <- data.frame(
  USUBJID = rep(paste0("SUBJ-", 1:5), each = 2),
  PARAMCD = rep(c("NEUT", "WBC"), 5),
  AVAL = c(4.5, 8.2, 6.1, 7.5, 3.2, 6.8, 5.5, 9.1, 4.8, 7.2),
  stringsAsFactors = FALSE
)

adae <- data.frame(
  USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-2", "SUBJ-4", "SUBJ-4", "SUBJ-4"),
  AETERM = c("Headache", "Nausea", "Fatigue", "Dizziness", "Headache", "Rash"),
  AESEV = c("MILD", "MODERATE", "MILD", "SEVERE", "MILD", "MODERATE"),
  stringsAsFactors = FALSE
)

# Create dm object
dm_obj <- dm(adsl = adsl, adlb = adlb, adae = adae)
print(dm_obj)

# Add keys
dm_with_keys <- dm_obj |>
 dm_add_pk(adsl, USUBJID) |>
 dm_add_fk(adlb, USUBJID, adsl) |>
 dm_add_fk(adae, USUBJID, adsl)

print(dm_with_keys)

# Filter: subjects with high neutrophils (NEUT > 5)
dm_filtered <- dm_with_keys |>
 dm_filter(adlb = (PARAMCD == "NEUT" & AVAL > 5))

print(dm_filtered)

# Get the adverse events for filtered subjects
ae_for_high_neut <- dm_filtered |>
 pull_tbl(adae)

print("Adverse events for subjects with neutrophils > 5:")
print(ae_for_high_neut)

# Alternative: flatten to get all info in one table
flattened <- dm_filtered |>
 dm_flatten_to_tbl(adae, .recursive = TRUE)

print("Flattened data:")
print(flattened)
