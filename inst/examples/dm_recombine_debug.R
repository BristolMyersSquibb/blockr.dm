# Reproduce: dm recombine error
#
# Workflow:
# 1. Create a dm with 3 tables (adsl, adqspref, adqs2)
# 2. Pull out adsl
# 3. Filter adsl (via dplyr filter)
# 4. Recombine: filtered adsl + adqspref + adqs2 → new dm
#
# Expected error: "All inputs must be data frames or dm objects<br>
#   All `dm` objects need to share the same `src`."

library(blockr)
library(blockr.dag)
library(blockr.dm)

# Synthetic ADaM-like data
adsl <- data.frame(
  USUBJID = paste0("SUBJ-", 1:5),
  AGE = c(45, 52, 38, 61, 29),
  SEX = c("M", "F", "M", "F", "M")
)

adqspref <- data.frame(
  USUBJID = rep(paste0("SUBJ-", 1:5), each = 2),
  PARAMCD = rep(c("QS01", "QS02"), 5),
  AVAL = c(3, 4, 2, 5, 4, 3, 1, 2, 5, 4)
)

adqs2 <- data.frame(
  USUBJID = rep(paste0("SUBJ-", 1:5), each = 2),
  PARAMCD = rep(c("QS03", "QS04"), 5),
  AVAL = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
)

# Build the dm, pull adsl, filter, then try to recombine
run_app(
  blocks = c(
    # Step 1: three data sources
    adsl_data = new_static_block(data = adsl),
    adqspref_data = new_static_block(data = adqspref),
    adqs2_data = new_static_block(data = adqs2),

    # Step 2: combine into dm (this works fine)
    dm_obj = new_dm_block(infer_keys = TRUE),

    # Step 3: pull out adsl table
    adsl_pulled = new_dm_pull_block(table = "adsl_data"),

    # Step 4: filter adsl
    adsl_filtered = blockr.dplyr::new_filter_block(
      columns = "SEX",
      values = list("M"),
      filter_fun = "blockr.dplyr::filter_is_in"
    ),

    # Step 5: pull out other tables
    adqspref_pulled = new_dm_pull_block(table = "adqspref_data"),
    adqs2_pulled = new_dm_pull_block(table = "adqs2_data"),

    # Step 6: recombine
    dm_recombined = new_dm_block(infer_keys = TRUE)
  ),
  links = c(
    # Build initial dm
    new_link("adsl_data", "dm_obj", "adsl_data"),
    new_link("adqspref_data", "dm_obj", "adqspref_data"),
    new_link("adqs2_data", "dm_obj", "adqs2_data"),

    # Pull adsl from dm, then filter
    new_link("dm_obj", "adsl_pulled", "data"),
    new_link("adsl_pulled", "adsl_filtered", "data"),

    # Pull other tables
    new_link("dm_obj", "adqspref_pulled", "data"),
    new_link("dm_obj", "adqs2_pulled", "data"),

    # Recombine: filtered adsl + pulled tables -> new dm
    new_link("adsl_filtered", "dm_recombined", "adsl"),
    new_link("adqspref_pulled", "dm_recombined", "adqspref"),
    new_link("adqs2_pulled", "dm_recombined", "adqs2")
  ),
  extensions = list(
    new_dag_extension()
  )
)
