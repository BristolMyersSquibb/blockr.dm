# Use Case 5: Data Quality Exploration
# "Which sites have unusual patterns of missing lab data?"
#
# Approach: create a dm from ADSL + ADLB, then dm crossfilter for cross-table
# exploration. Click SITE-03 in ADSL panel -> ADLB panel shows which PARAMCDs
# and VISITs have high missing rates at that site. Click CREAT in ADLB panel ->
# ADSL panel shows which sites have missing creatinine data.
#
# The dm crossfilter enables bidirectional exploration: filter sites in ADSL
# to see lab impact, or filter labs in ADLB to see affected sites/subjects.

library(blockr)
library(blockr.dag)

pkgload::load_all("../blockr.dm")

# --- Synthetic data ---
# ADSL with site assignments, ADLB with intentional missing values

set.seed(123)

sites <- c("SITE-01", "SITE-02", "SITE-03", "SITE-04")
subjects_per_site <- 5

adsl <- data.frame(
  USUBJID = paste0(rep(sites, each = subjects_per_site), "-SUBJ-",
                   rep(1:subjects_per_site, length(sites))),
  SITEID = rep(sites, each = subjects_per_site),
  AGE = sample(30:75, 20, replace = TRUE),
  SEX = sample(c("M", "F"), 20, replace = TRUE)
)

visits <- c("Screening", "Baseline", "Week 4", "Week 8", "Week 12")
params <- c("ALT", "AST", "CREAT", "BILI")

# Generate complete grid
grid <- expand.grid(
  USUBJID = adsl$USUBJID,
  VISIT = visits,
  PARAMCD = params,
  stringsAsFactors = FALSE
)

# Generate values with intentional missing patterns
n <- nrow(grid)
grid$AVAL <- round(runif(n, 10, 100), 1)

# Extract SITEID from USUBJID for missing patterns
grid$SITEID <- sub("-SUBJ-.*", "", grid$USUBJID)

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

adlb <- grid[, c("USUBJID", "PARAMCD", "VISIT", "AVAL", "MISSING")]

# --- Workflow ---
#
# 1. Combine ADSL and ADLB into a dm (auto-infers USUBJID PK/FK)
# 2. dm crossfilter — ADSL panel shows SITEID, SEX;
#    ADLB panel shows PARAMCD, VISIT, MISSING
#    Filters cascade bidirectionally between tables.
# 3. Pull the filtered ADLB table for display
#
# Click SITE-03 -> see which PARAMCDs/VISITs have high missing rates
# Click MISSING=Y -> see which sites and subjects have the most gaps

run_app(
  blocks = c(
    adsl_data   = new_static_block(data = adsl),
    adlb_data   = new_static_block(data = adlb),

    # Combine into dm with auto-inferred keys
    dm_obj      = new_dm_block(),

    # dm crossfilter: explore missing patterns across sites and labs
    crossfilter = new_dm_crossfilter_block(
      active_dims = list(
        adsl_data = c("SITEID", "SEX"),
        adlb_data = c("PARAMCD", "VISIT", "MISSING")
      )
    ),

    # Pull filtered labs
    result      = new_dm_pull_block(table = "adlb_data")
  ),
  links = c(
    # Combine into dm
    new_link("adsl_data", "dm_obj", "adsl_data"),
    new_link("adlb_data", "dm_obj", "adlb_data"),

    # Crossfilter on dm
    new_link("dm_obj", "crossfilter", "data"),

    # Pull filtered labs
    new_link("crossfilter", "result", "data")
  )
)
