# Use Case 6: Regulatory Query Response
# "For patients with elevated creatinine, what concomitant medications
#  were they on?"
#
# What this demonstrates:
# - The simplest cascading filter pattern: filter one child table (ADLB),
#   and the cascade propagates through the parent (ADSL) to another child
#   table (ADCM)
# - This is the pattern blockr.dm handles best today: relational filtering
#   across tables connected by foreign keys
#
# How it works:
# 1. Filter ADLB: PARAMCD == 'CREAT' & AVAL > 1.5
# 2. dm_filter cascades UP to ADSL (via semi-join on USUBJID),
#    identifying subjects with elevated creatinine
# 3. The filter then cascades DOWN from ADSL to ADCM, subsetting
#    to concomitant medications for those subjects only
# 4. Pull ADCM to get the medication list
#
# Limitations:
# - No visual crossfilter UI; the filter expression is static text
# - To change the threshold (e.g., 1.5 -> 2.0), you must edit the
#   expression in the UI or create a new block

library(blockr)
library(blockr.dag)
library(blockr.dm)

# --- Synthetic data ---

adsl <- data.frame(
  USUBJID = paste0("SUBJ-", 1:8),
  AGE = c(65, 52, 70, 45, 58, 72, 48, 63),
  SEX = c("M", "F", "M", "F", "M", "F", "M", "F"),
  TRT01A = c("Drug A", "Drug B", "Drug A", "Drug B",
             "Drug A", "Drug A", "Drug B", "Drug A")
)

# Lab data with creatinine values
adlb <- data.frame(
  USUBJID = rep(paste0("SUBJ-", 1:8), each = 3),
  PARAMCD = rep(c("CREAT", "ALT", "AST"), 8),
  PARAM = rep(c("Creatinine", "Alanine Aminotransferase",
                "Aspartate Aminotransferase"), 8),
  AVAL = c(
    1.8, 30, 28,   # SUBJ-1: elevated creatinine
    0.9, 25, 22,   # SUBJ-2: normal
    2.1, 35, 32,   # SUBJ-3: elevated creatinine
    1.0, 22, 20,   # SUBJ-4: normal
    1.6, 40, 38,   # SUBJ-5: elevated creatinine
    1.2, 28, 25,   # SUBJ-6: normal
    0.8, 20, 18,   # SUBJ-7: normal
    1.9, 45, 42    # SUBJ-8: elevated creatinine
  )
)

# Concomitant medications
adcm <- data.frame(
  USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-2", "SUBJ-3", "SUBJ-3",
              "SUBJ-4", "SUBJ-5", "SUBJ-5", "SUBJ-6", "SUBJ-8"),
  CMTRT = c("Lisinopril", "Metformin", "Aspirin", "Atorvastatin",
            "Ibuprofen", "Omeprazole", "Amlodipine", "Metformin",
            "Aspirin", "Furosemide"),
  CMDECOD = c("LISINOPRIL", "METFORMIN", "ACETYLSALICYLIC ACID",
              "ATORVASTATIN", "IBUPROFEN", "OMEPRAZOLE",
              "AMLODIPINE", "METFORMIN", "ACETYLSALICYLIC ACID",
              "FUROSEMIDE"),
  CMCLAS = c("ACE inhibitor", "Antidiabetic", "Analgesic",
             "Lipid-lowering", "NSAID", "PPI",
             "Calcium channel blocker", "Antidiabetic",
             "Analgesic", "Diuretic")
)

# --- Workflow ---
#
# 1. Load ADSL, ADLB, ADCM
# 2. Combine into dm (USUBJID links all three)
# 3. Filter ADLB: elevated creatinine (PARAMCD == 'CREAT' & AVAL > 1.5)
#    -> Cascades UP to ADSL, then DOWN to ADCM
# 4. Pull ADCM: concomitant medications for affected subjects
#
# Expected subjects with elevated creatinine: SUBJ-1, -3, -5, -8
# Expected ADCM records: medications for those 4 subjects
#   SUBJ-1: Lisinopril, Metformin
#   SUBJ-3: Atorvastatin, Ibuprofen
#   SUBJ-5: Amlodipine, Metformin
#   SUBJ-8: Furosemide

run_app(
  blocks = c(
    adsl_data = new_static_block(data = adsl),
    adlb_data = new_static_block(data = adlb),
    adcm_data = new_static_block(data = adcm),

    dm_obj = new_dm_block(infer_keys = TRUE),

    # Filter ADLB to elevated creatinine
    # Cascades: ADLB -> ADSL -> ADCM (via USUBJID foreign keys)
    filtered_dm = new_dm_filter_block(
      table = "adlb_data",
      expr = "PARAMCD == 'CREAT' & AVAL > 1.5"
    ),

    # Pull concomitant medications for affected subjects
    medications = new_dm_pull_block(table = "adcm_data"),

    # Also: nested view to explore subject -> medications interactively
    nested = new_dm_nested_view_block(root_table = "adsl_data")
  ),
  links = c(
    new_link("adsl_data", "dm_obj", "adsl_data"),
    new_link("adlb_data", "dm_obj", "adlb_data"),
    new_link("adcm_data", "dm_obj", "adcm_data"),

    new_link("dm_obj", "filtered_dm", "data"),
    new_link("filtered_dm", "medications", "data"),
    new_link("filtered_dm", "nested", "data")
  ),
  extensions = list(new_dag_extension())
)
