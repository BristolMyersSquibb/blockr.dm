# Run the DuckDB remote-dm board against LOCAL source checkouts (your latest
# uncommitted changes to any blockr package). This is the pkgload::load_all()
# counterpart of the shipped, library()-based inst/examples/duckdb-remote-dm.R:
# it just flips the loader and sources it, so the two can never drift.
#
# Run from an R session at the workspace root:
#   source("blockr.dm/dev/duckdb-remote-dm.R")
#
# For a lighter local run (smaller fact table):
#   n_orders <- 5e5L; source("blockr.dm/dev/duckdb-remote-dm.R")
#
# (End users without the source checkouts run the shipped copy instead:
#   source(system.file("examples/duckdb-remote-dm.R", package = "blockr.dm")))

options(shiny.port = 3838, shiny.host = "0.0.0.0")

dev_local <- TRUE
source("blockr.dm/inst/examples/duckdb-remote-dm.R")
