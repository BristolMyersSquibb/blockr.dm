# Load required packages for testing
library(shiny)
library(blockr.core)
library(dm)

# The shinytest2 tests launch headless Chrome via chromote, which leaves
# `com.google.Chrome.*` scratch dirs in the session temp root. Those trip
# R CMD check's "checking for detritus in the temp directory" NOTE -- not
# anything blockr.dm writes. Sweep them after the whole test run (registered
# on testthat's teardown_env so it fires once, last). On Linux unlink() drops
# the directory entries even if Chrome still holds them open, so the detritus
# check no longer sees them.
withr::defer(
  {
    roots <- unique(c(
      tempdir(),
      dirname(tempdir()),
      Sys.getenv("TMPDIR"),
      Sys.getenv("TMP"),
      Sys.getenv("TEMP")
    ))
    roots <- roots[nzchar(roots)]
    junk <- unlist(lapply(roots, function(r) {
      Sys.glob(file.path(r, "com.google.Chrome.*"))
    }))
    if (length(junk)) unlink(unique(junk), recursive = TRUE, force = TRUE)
  },
  teardown_env()
)
