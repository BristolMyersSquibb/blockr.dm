# Manual probe for the range-card changes: editable min/max labels + the
# blue density overlay. Numeric (AGE) and date (AENDT) cards are open on
# load, so there is nothing to click through the gear.
options(blockr.tabular_display = blockr.ui::html_table_display)
options(blockr.dock_is_locked = FALSE)
options(shiny.port = as.integer(Sys.getenv("PORT", "3842")), shiny.host = "0.0.0.0")

pkgload::load_all("/workspace/blockr.core")
pkgload::load_all("/workspace/blockr.ui")
pkgload::load_all("/workspace/blockr.dock")
pkgload::load_all("/workspace/blockr.dag")
pkgload::load_all("/workspace/blockr.dm")

board <- new_dock_board(
  blocks = c(
    data = new_dm_example_block(dataset = "safetydata_adam"),
    cf = new_crossfilter_block(
      # adlbc is the big one (74k rows): AVAL there is the perf case for the
      # per-filter-change density rebuild.
      active_dims = list(
        adsl = c("AGE", "ARM"), adae = "AENDT", adlbc = "AVAL"
      )
    )
  ),
  links = new_link(from = "data", to = "cf", input = "data"),
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
