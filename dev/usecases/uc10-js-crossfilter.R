# Use Case 10: JS Crossfilter (experimental)
#
# Side-by-side comparison of the server-side dm crossfilter and the new
# client-side JS crossfilter. Same dm, same active dimensions — the JS
# version does all filtering in the browser using crossfilter2.js.
#
# Run from the repo root:
#   Rscript -e 'blockr.dag::run_app(source("dev/usecases/uc10-js-crossfilter.R")$value)'
# Or just source it directly:
#   source("dev/usecases/uc10-js-crossfilter.R")

library(blockr)
library(blockr.dag)

pkgload::load_all("blockr.dm")

# --- Synthetic clinical data ---

adsl <- data.frame(
  USUBJID = paste0("SUBJ-", 1:20),
  AGE = c(55, 62, 48, 70, 45, 58, 34, 67, 52, 41,
          73, 39, 60, 44, 56, 65, 50, 36, 71, 47),
  SEX = rep(c("M", "F"), 10),
  RACE = c("WHITE", "BLACK", "ASIAN", "WHITE", "BLACK",
           "WHITE", "ASIAN", "BLACK", "WHITE", "ASIAN",
           "WHITE", "BLACK", "ASIAN", "WHITE", "BLACK",
           "WHITE", "ASIAN", "BLACK", "WHITE", "ASIAN"),
  TRT01A = rep(c("Drug A", "Drug B"), 10),
  stringsAsFactors = FALSE
)

adae <- data.frame(
  USUBJID = c(
    "SUBJ-1", "SUBJ-1", "SUBJ-2", "SUBJ-3", "SUBJ-3",
    "SUBJ-4", "SUBJ-5", "SUBJ-6", "SUBJ-7", "SUBJ-8",
    "SUBJ-9", "SUBJ-10", "SUBJ-11", "SUBJ-12", "SUBJ-13",
    "SUBJ-14", "SUBJ-15", "SUBJ-17", "SUBJ-19", "SUBJ-20"
  ),
  AESEV = c(
    "SEVERE", "MILD", "MODERATE", "SEVERE", "MILD",
    "MODERATE", "SEVERE", "MILD", "MODERATE", "SEVERE",
    "MILD", "MODERATE", "SEVERE", "MILD", "MODERATE",
    "SEVERE", "MILD", "SEVERE", "MILD", "MODERATE"
  ),
  AEBODSYS = c(
    "CARDIAC", "GI", "GI", "RENAL", "CARDIAC",
    "GI", "RENAL", "CARDIAC", "GI", "RENAL",
    "CARDIAC", "GI", "RENAL", "CARDIAC", "GI",
    "RENAL", "CARDIAC", "GI", "RENAL", "CARDIAC"
  ),
  AESER = c(
    "Y", "N", "N", "Y", "N",
    "N", "Y", "N", "N", "Y",
    "N", "N", "Y", "N", "N",
    "Y", "N", "Y", "N", "N"
  ),
  stringsAsFactors = FALSE
)

set.seed(42)
adlb <- data.frame(
  USUBJID = rep(paste0("SUBJ-", 1:20), each = 4),
  PARAMCD = rep(c("ALT", "AST", "BILI", "CREAT"), 20),
  AVAL = round(runif(80, 10, 200), 1),
  stringsAsFactors = FALSE
)

# Shared dimensions for both blocks
active_dims <- list(
  adsl_data = c("SEX", "RACE", "TRT01A"),
  adae_data = c("AESEV", "AEBODSYS", "AESER"),
  adlb_data = c("PARAMCD", "AVAL")
)

run_app(
  blocks = c(
    # Data sources
    adsl_data = new_static_block(data = adsl),
    adae_data = new_static_block(data = adae),
    adlb_data = new_static_block(data = adlb),

    # Combine into dm
    dm_obj = new_dm_block(),

    # Server-side crossfilter (existing)
    server_cf = new_dm_crossfilter_block(active_dims = active_dims),

    # Client-side JS crossfilter (experimental)
    js_cf = new_js_crossfilter_block(active_dims = active_dims)
  ),
  links = c(
    new_link("adsl_data", "dm_obj", "adsl_data"),
    new_link("adae_data", "dm_obj", "adae_data"),
    new_link("adlb_data", "dm_obj", "adlb_data"),
    new_link("dm_obj", "server_cf", "data"),
    new_link("dm_obj", "js_cf", "data")
  )
)
