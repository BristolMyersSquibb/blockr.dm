# Use Case 10: JS Crossfilter (experimental)
#
# Client-side crossfilter using crossfilter2.js. R builds lookup tables and
# ships them to the browser; all filtering happens in JavaScript for instant
# responsiveness. Uses pharmaverseadam for realistic clinical data.
#
# Run from the repo root:
#   source("dev/usecases/uc10-js-crossfilter.R")

library(blockr)
library(blockr.dag)

pkgload::load_all("blockr.dm")

run_app(
  blocks = c(
    data = new_dm_example_block(dataset = "pharmaverseadam"),
    js_cf = new_js_crossfilter_block(
      active_dims = list(
        adsl = c("SEX", "RACE", "TRT01A", "ETHNIC"),
        adae = c("AESEV", "AEBODSYS", "AESER"),
        adlb = c("PARAMCD", "AVAL")
      )
    )
  ),
  links = c(
    new_link("data", "js_cf", "data")
  )
)
