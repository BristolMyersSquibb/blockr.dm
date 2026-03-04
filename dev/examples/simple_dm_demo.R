# Safety DM Demo
#
# Demonstrates new_safety_dm_block() as a zero-config data source for
# CDISC safety workflows. The block loads ADSL (254 subjects), ADAE (1,191
# rows), and ADLB (74,264 rows) from safetyData with USUBJID keys already
# set — no manual static blocks, dm_block, or link wiring needed.
#
# Compare with uc7-large-adam.R which requires 4 static blocks + dm_block
# + 4 links to achieve the same starting point.
pkgload::load_all("g6R")
pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.session")


library(blockr)

board <-
  new_dock_board(
    blocks = c(
      # One block gives you a ready-to-use safety dm
      safety = new_safety_dm_block()
    ),
    extensions = list(
      blockr.dag::new_dag_extension()
    )
  )

serve(board, "io_app", plugins = custom_plugins(manage_project()))
