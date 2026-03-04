# Safety DM Demo
#
# Demonstrates new_safety_dm_block() as a zero-config data source for
# CDISC safety workflows. The block loads ADSL (254 subjects), ADAE (1,191
# rows), and ADLB (74,264 rows) from safetyData with USUBJID keys already
# set — no manual static blocks, dm_block, or link wiring needed.
#
# Compare with uc7-large-adam.R which requires 4 static blocks + dm_block
# + 4 links to achieve the same starting point.

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dag")

library(blockr)

serve(
  new_dock_board(
    blocks = c(
      # One block gives you a ready-to-use safety dm
      safety = new_safety_dm_block(),

      # Dedup subject-level columns from child tables so the
      # crossfilter panels only show columns specific to each domain
      cdisc = new_cdisc_dm_block(),

      # Interactive crossfilter across all three tables
      crossfilter = new_dm_crossfilter_block(
        active_dims = list(
          adsl = c("SEX", "AGE"),
          adae = c("AESEV"),
          adlb = c("PARAMCD")
        )
      ),

      # Pull the filtered subject table
      subjects = new_dm_pull_block(table = "adsl")
    ),
    links = c(
      new_link("safety", "cdisc", "data"),
      new_link("cdisc", "crossfilter", "data"),
      new_link("crossfilter", "subjects", "data")
    ),
    extensions = list(
      blockr.dag::new_dag_extension()
    )
  )
)
