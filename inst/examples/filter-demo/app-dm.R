# filter-demo/app-dm.R — new_value_filter_block() with dm input.
#
# Verifies the dm path: table picker appears in the gear popover, scope
# filter via dm::dm_filter() cascades through the dm.
#
# From /workspace:
#   Rscript blockr.dm/inst/examples/filter-demo/app-dm.R

options(
  blockr.tabular_display = blockr.ui::html_table_display,
  blockr.lazy_eval          = FALSE
)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.extra")

# Small demo dm: policies + claims sharing policy_id.
demo_dm <- dm::as_dm(list(
  policies = data.frame(
    policy_id = c("P001", "P002", "P003"),
    status    = c("active", "active", "lapsed"),
    stringsAsFactors = FALSE
  ),
  claims = data.frame(
    claim_id   = c("C1", "C2", "C3", "C4"),
    policy_id  = c("P001", "P001", "P002", "P003"),
    claim_year = c(2024L, 2025L, 2024L, 2023L),
    stringsAsFactors = FALSE
  )
))

board <- new_dock_board(
  blocks = c(
    src = new_static_block(demo_dm),
    flt = new_value_filter_block(),
    # Pull one table out so the dock can render a preview; the filter's
    # output is itself a dm and the dock's table preview expects a df.
    out = new_dm_pull_block(table = "policies")
  ),
  links = c(
    new_link("src", "flt", "data"),
    new_link("flt", "out", "data")
  ),
  extensions = new_dock_extensions(list(
    new_dag_extension()
  ))
)

serve(board, "filter-demo-dm")
