pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.session")
pkgload::load_all("blockr.extra")

options(
  # shiny.port = 7861L,
  # shiny.host = "0.0.0.0",
  # shiny.launch.browser = FALSE,
  blockr.html_table_preview = TRUE
)

# datasets::penguins has NAs in `sex` (11 rows) and `body_mass_g` (2 rows),
# making it a good test case for crossfilter NA handling.
board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("penguins", package = "datasets"),
    cf = new_crossfilter_block()
  ),
  links = new_link(from = "data", to = "cf", input = "data"),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

serve(board)
