# Load required packages for testing
library(shiny)
library(blockr.core)
library(dm)

# The shinytest2 tests drive headless Chrome through chromote's default
# browser object. That Chrome stays alive past testthat's teardown (it only
# dies when the test process exits), so it keeps (re)creating
# `com.google.Chrome.*` scratch dirs in the temp root that R CMD check scans
# for "checking for detritus in the temp directory" -- a NOTE that fails CI
# under the strict fail-on-any-NOTE policy, even though the dirs aren't
# anything blockr.dm writes. So: stop that Chrome first, THEN sweep its
# leftovers (registered on teardown_env so it runs once, last). Killing the
# process before unlinking is what makes the sweep stick -- otherwise Chrome
# recreates a fresh dir after we delete. Scoped strictly to
# com.google.Chrome.* so nothing else is touched.
withr::defer(
  {
    if (requireNamespace("chromote", quietly = TRUE) &&
        isTRUE(chromote::has_default_chromote_object())) {
      obj <- chromote::default_chromote_object()
      proc <- tryCatch(obj$get_browser()$get_process(), error = function(e) NULL)
      try(obj$close(), silent = TRUE)
      if (!is.null(proc)) try(proc$kill(), silent = TRUE)
    }
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
