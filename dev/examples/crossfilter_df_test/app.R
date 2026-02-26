pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.session")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.sandbox")

options(
  shiny.port = 7860L,
  shiny.launch.browser = FALSE,
  blockr.html_table_preview = TRUE
)

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("iris", package = "datasets"),
    cf = new_crossfilter_block()
  ),
  links = new_link(from = "data", to = "cf", input = "data"),
  extensions = list(
    new_edit_board_extension(),
    blockr.dag::new_dag_extension()
  )
)

serve(board)
