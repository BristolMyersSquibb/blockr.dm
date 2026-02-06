# Use Case 5: Data Quality Exploration
# "Which sites have unusual patterns of missing lab data?"
#
# Approach: single-table crossfilter on ADLB — the ideal crossfilter use case.
# Click SITE-03 -> see which PARAMCDs and VISITs have high missing rates.
# Click CREAT -> see which sites are worst. The exclude-own-dimension pattern
# makes this work: each table shows all its values while reflecting filters
# from other dimensions.

library(blockr)
library(blockr.bi)
library(blockr.dag)

# --- Synthetic data ---
# ADLB with intentional missing values to simulate data quality issues

set.seed(123)

sites <- c("SITE-01", "SITE-02", "SITE-03", "SITE-04")
subjects_per_site <- 5
visits <- c("Screening", "Baseline", "Week 4", "Week 8", "Week 12")
params <- c("ALT", "AST", "CREAT", "BILI")

# Generate complete grid
grid <- expand.grid(
  SITEID = sites,
  SUBJNUM = 1:subjects_per_site,
  VISIT = visits,
  PARAMCD = params,
  stringsAsFactors = FALSE
)
grid$USUBJID <- paste0(grid$SITEID, "-SUBJ-", grid$SUBJNUM)

# Generate values with intentional missing patterns
n <- nrow(grid)
grid$AVAL <- round(runif(n, 10, 100), 1)

# SITE-03 has high missingness at later visits
missing_site3_late <- grid$SITEID == "SITE-03" &
  grid$VISIT %in% c("Week 8", "Week 12")
grid$AVAL[missing_site3_late] <- ifelse(
  runif(sum(missing_site3_late)) < 0.6, NA, grid$AVAL[missing_site3_late]
)

# SITE-04 has high missingness for CREAT specifically
missing_site4_creat <- grid$SITEID == "SITE-04" & grid$PARAMCD == "CREAT"
grid$AVAL[missing_site4_creat] <- ifelse(
  runif(sum(missing_site4_creat)) < 0.7, NA, grid$AVAL[missing_site4_creat]
)

# Scatter a few random NAs elsewhere
random_missing <- runif(n) < 0.05
grid$AVAL[random_missing & !missing_site3_late & !missing_site4_creat] <- NA

# Flag missing values
grid$MISSING <- ifelse(is.na(grid$AVAL), "Y", "N")

adlb <- grid[, c("USUBJID", "SITEID", "PARAMCD", "VISIT", "AVAL", "MISSING")]

# --- Workflow ---
#
# 1. Load ADLB as a static block
# 2. Crossfilter with dimensions: SITEID, PARAMCD, VISIT, MISSING
#
# No joins needed — this is a pure single-table exploration.

run_app(
  blocks = c(
    adlb_data   = new_static_block(data = adlb),
    crossfilter = new_table_filter_block()
  ),
  links = c(
    new_link("adlb_data", "crossfilter", "data")
  )
)
