# Use Case 8: Insurance Portfolio Analysis
# "Which areas and vehicle segments drive the most claims exposure?"
#
# Demonstrates: dm_crossfilter_block with the measure selector on a star schema
# (non-clinical data). The policies table holds dimension attributes (Area,
# VehGas, driver age band, vehicle power band). The claims table holds numeric
# measures (Amount, Exposure). Selecting a measure like "claims.Amount" in the
# dropdown shows SUM(Amount) per dimension value instead of row counts.
#
# Star schema:
#   policies (PK: policy_id)  <--FK--  claims (FK: policy_id)
#
# This mirrors the flat freMTPL2 insurance example from blockr.deploy, but
# split into a relational model so filters cascade across tables and the
# measure selector can aggregate facts from the claims table by dimensions
# in the policies table.

library(blockr)
library(blockr.dag)

pkgload::load_all("../blockr.dm")

# --- Synthetic insurance data (star schema) ---

set.seed(42)

# Dimension table: 60 motor insurance policies
n_policies <- 60
policies <- data.frame(
  policy_id = seq_len(n_policies),
  Area = sample(c("A", "B", "C", "D", "E", "F"), n_policies, replace = TRUE,
                prob = c(0.05, 0.10, 0.15, 0.25, 0.25, 0.20)),
  VehGas = sample(c("Diesel", "Regular"), n_policies, replace = TRUE,
                  prob = c(0.55, 0.45)),
  DrivAgeBand = sample(
    c("18-25", "26-35", "36-45", "46-55", "56-65", "65+"),
    n_policies, replace = TRUE,
    prob = c(0.08, 0.20, 0.25, 0.22, 0.15, 0.10)
  ),
  VehPowerBand = sample(
    c("4-5", "6-7", "8-9", "10-11", "12+"),
    n_policies, replace = TRUE,
    prob = c(0.30, 0.30, 0.20, 0.12, 0.08)
  ),
  Region = sample(
    c("Ile-de-France", "Provence", "Auvergne", "Bretagne", "Occitanie"),
    n_policies, replace = TRUE,
    prob = c(0.30, 0.20, 0.15, 0.15, 0.20)
  ),
  stringsAsFactors = FALSE
)

# Fact table: claims — multiple claims per policy (some policies have 0)
# Policies with claims: ~40% have 1 claim, ~15% have 2+
has_claim <- rbinom(n_policies, 1, prob = 0.55)
claim_counts <- ifelse(has_claim, pmax(1, rpois(n_policies, 1.2)), 0)

claims_list <- lapply(seq_len(n_policies), function(i) {
  nc <- claim_counts[i]
  if (nc == 0) return(NULL)
  data.frame(
    policy_id = rep(i, nc),
    Amount = round(runif(nc, min = 200, max = 15000), 2),
    Exposure = round(runif(nc, min = 0.1, max = 1.0), 3),
    ClaimType = sample(c("Bodily Injury", "Property Damage", "Third Party"),
                       nc, replace = TRUE, prob = c(0.25, 0.50, 0.25)),
    stringsAsFactors = FALSE
  )
})
claims <- do.call(rbind, Filter(Negate(is.null), claims_list))
claims$claim_id <- seq_len(nrow(claims))

# --- Workflow ---
#
# 1. Load policies (dimensions) and claims (facts) as static blocks
# 2. Combine into a dm with policy_id PK/FK
# 3. dm crossfilter with measure selector:
#    - Default (Count): bars show number of policies per dimension value
#    - Select "claims.Amount": bars show SUM(Amount) per dimension value
#    - Select "claims.Exposure": bars show SUM(Exposure) per dimension value
# 4. Pull the filtered claims table
#
# Try: switch the measure dropdown to "claims: Amount", then click
# Area = "D" or "E" to see how claim amounts concentrate in urban areas.

run_app(
  blocks = c(
    policies_data = new_static_block(data = policies),
    claims_data   = new_static_block(data = claims),
    dm_obj        = new_dm_block(),
    crossfilter   = new_dm_crossfilter_block(
      active_dims = list(
        policies_data = c("Area", "VehGas", "DrivAgeBand", "Region"),
        claims_data   = c("ClaimType")
      ),
      measure = "claims_data.Amount"
    ),
    result = new_dm_pull_block(table = "claims_data")
  ),
  links = c(
    new_link("policies_data", "dm_obj", "policies_data"),
    new_link("claims_data", "dm_obj", "claims_data"),
    new_link("dm_obj", "crossfilter", "data"),
    new_link("crossfilter", "result", "data")
  )
)
