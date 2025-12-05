# Test blockr.dm with blockr.core
#
# This script tests the blocks without running the full Shiny app

library(blockr.core)
devtools::load_all("/Users/christophsax/git/blockr/blockr.dm")

# Create test data
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

# Test creating blocks
cat("Creating dm block...\n")
dm_block <- new_dm_block()
print(class(dm_block))

cat("\nCreating dm_add_keys block...\n")
keys_block <- new_dm_add_keys_block(
  pk_table = "adsl",
  pk_column = "USUBJID",
  fk_table = "adlb",
  fk_column = "USUBJID"
)
print(class(keys_block))

cat("\nCreating dm_filter block...\n")
filter_block <- new_dm_filter_block(
  table = "adlb",
  expr = "PARAMCD == 'NEUT' & AVAL > 5"
)
print(class(filter_block))

cat("\nCreating dm_pluck block...\n")
pluck_block <- new_dm_pluck_block(table = "adae")
print(class(pluck_block))

cat("\nCreating dm_flatten block...\n")
flatten_block <- new_dm_flatten_block(start_table = "adae", recursive = TRUE)
print(class(flatten_block))

cat("\nAll blocks created successfully!\n")

# Check registered blocks
cat("\nRegistered dm blocks:\n")
registry <- blockr.core::available_blocks()
dm_blocks <- registry[grepl("dm", registry$constructor), ]
print(dm_blocks[, c("constructor", "name", "category")])
