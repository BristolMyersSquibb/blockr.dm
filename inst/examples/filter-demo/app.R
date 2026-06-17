# filter-demo/app.R — new_value_filter_block() inside a dock + DAG board.
#
# The dock UI gives you the block sidebar, gear offcanvas, DAG panel.
# The DAG makes the dataset -> filter -> (downstream) flow visible.
#
#   Rscript /workspace/blockr.dm/inst/examples/filter-demo/app.R
#
# then open http://localhost:3838 .

# options(shiny.port = 3838, shiny.host = "0.0.0.0",
#         shiny.launch.browser = FALSE)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.sandbox")


# register_dm_blocks()

# Demo data with column labels and haven-style value labels so the filter
# select shows both.
df <- iris
attr(df$Species, "label") <- "Iris Species"

df$Sex <- sample(c(1L, 2L), nrow(df), replace = TRUE)
attr(df$Sex, "label") <- "Sex"
attr(df$Sex, "labels") <- c(Male = 1L, Female = 2L)

df$Region <- sample(c("North", "South", "East", "West"), nrow(df), replace = TRUE)
attr(df$Region, "label") <- "Sales Region"

board <- new_dock_board(
  blocks = c(
    src = new_static_block(df),
    flt = new_value_filter_block()
  ),
  links = links(from = "src", to = "flt"),
  extensions = new_dock_extensions(list(
    new_dag_extension()
  ))
)

serve(board, "filter-demo")
