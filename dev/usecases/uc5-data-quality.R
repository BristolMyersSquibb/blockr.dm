# Use Case 5: Data Quality Exploration
# "Which sites have unusual patterns of missing lab data?"
#
# What this demonstrates:
# - Single-table use case: ADLB with missing values across sites/visits
# - What blockr.dm CAN do: wrap a single table in a dm, filter, pull
# - What blockr.dm CANNOT do well: crossfilter with exclude-own-dimension
#
# The crossfilter pattern:
# The ideal UI for this use case is a crossfilter where you click on a
# site in a bar chart and all OTHER charts update (but not the clicked
# one). This "exclude-own-dimension" pattern lives in blockr.bi's
# table_filter_block, not in blockr.dm. blockr.dm's dm_filter_block
# cascades filters DOWN foreign key relationships, which is not the
# same as crossfiltering within a single table.
#
# This UC highlights the gap: single-table exploratory filtering is
# better served by blockr.bi, while blockr.dm excels at multi-table
# relational filtering.
#
# Limitations:
# - No crossfilter UI in blockr.dm (use blockr.bi for single-table)
# - No built-in missing data summary or heatmap visualization
# - dm_filter_block applies a static expression, not interactive
#   click-to-filter

library(blockr)
library(blockr.dag)
library(blockr.dm)

# --- Synthetic data ---
# ADLB with some intentional missing values to simulate data quality issues

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
# What blockr.dm can do here:
# - Wrap ADLB in a dm (single table)
# - Filter to a specific site or visit to investigate
# - Pull the result
#
# This is a minimal demonstration. For real data quality exploration,
# blockr.bi's crossfilter (table_filter_block) would allow interactive
# drill-down by SITEID, PARAMCD, and VISIT simultaneously.

run_app(
  blocks = c(
    adlb_data = new_static_block(data = adlb),

    # Wrap in dm (single table — no relationships to infer)
    dm_obj = new_dm_block(infer_keys = TRUE),

    # Filter to missing records at SITE-03 for investigation
    filtered_dm = new_dm_filter_block(
      table = "adlb_data",
      expr = "MISSING == 'Y' & SITEID == 'SITE-03'"
    ),

    # Pull the filtered result
    missing_records = new_dm_pull_block(table = "adlb_data")
  ),
  links = c(
    new_link("adlb_data", "dm_obj", "adlb_data"),
    new_link("dm_obj", "filtered_dm", "data"),
    new_link("filtered_dm", "missing_records", "data")
  ),
  extensions = list(new_dag_extension())
)
