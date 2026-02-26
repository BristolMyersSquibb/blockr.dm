# Use Case 7: Realistic ADaM data (CDISC Pilot 01 from safetyData)
#
# ADSL  (254 subjects, 48 cols) - Subject Level
# ADLBC (74,264 rows, 46 cols) - Laboratory Chemistry
# ADVS  (32,139 rows, 34 cols) - Vital Signs
# ADAE  (1,191 rows, 55 cols)  - Adverse Events

pkgload::load_all("../blockr.core")
pkgload::load_all("../blockr.dock")
pkgload::load_all("../blockr.ai")
pkgload::load_all("../blockr.dm")

library(blockr)
library(safetyData)

# Enable debug logging to trace double evaluation
options(blockr.log_level = "debug")

serve(
  new_dock_board(
    blocks = c(
      adsl_data = new_static_block(data = adam_adsl),
      adae_data = new_static_block(data = adam_adae),
      adlb_data = new_static_block(data = adam_adlbc),
      advs_data = new_static_block(data = adam_advs),
      dm_obj    = new_dm_block(infer_keys = TRUE),
      result    = new_dm_pull_block(table = "adsl_data"),
      crossfilter = new_dm_crossfilter_block(
        active_dims = list(adsl_data = c("SEX", "AGE"), adae_data = c("AESEV"))
      )
    ),
    links = c(
      new_link("adsl_data", "dm_obj", "adsl_data"),
      new_link("adae_data", "dm_obj", "adae_data"),
      new_link("adlb_data", "dm_obj", "adlb_data"),
      new_link("advs_data", "dm_obj", "advs_data"),
      new_link("dm_obj", "crossfilter", "data"),
      new_link("crossfilter", "result", "data")
    ),
    extensions = list(dag = new_dag_extension())
  ),
  plugins = custom_plugins(ai_ctrl_block())
)
