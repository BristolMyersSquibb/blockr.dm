library(blockr.core)
library(blockr.dm)

# Two paths the AI assistant / board-restore goes through:
#
# 1. CONSTRUCTOR — the block is created with non-empty state. The
#    data observe should ship that state in its initial setData so
#    the JS UI paints bars / sliders without a user click.
#
# 2. RUNTIME — something outside the JS UI mutates one of the
#    externally-controlled reactiveVals (`vars[[nm]](val)` in
#    ai_ctrl_server). The R->JS push observer then has to ship the
#    new state so the UI repaints.
#
# We expose path (2) via a custom ctrl_block plugin that watches a
# test-driven actionButton and writes a known state into vars.

# Test-only ctrl plugin: a button per "preset" that overwrites the
# block's state reactiveVals exactly like an AI tool call would.
test_ctrl_server <- function(id, x, vars, data, eval) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$set_setosa, {
      vars$filters(list(.tbl = list(Species = "setosa")))
    })
    shiny::observeEvent(input$set_range, {
      vars$range_filters(list(.tbl = list(Sepal.Length = c(5, 6))))
    })
    shiny::observeEvent(input$clear_all, {
      vars$filters(list())
      vars$range_filters(list())
    })
    shiny::reactive(TRUE)
  })
}

test_ctrl_ui <- function(id, x) {
  ns <- shiny::NS(id)
  shiny::div(
    shiny::actionButton(ns("set_setosa"), "test:setosa"),
    shiny::actionButton(ns("set_range"), "test:range"),
    shiny::actionButton(ns("clear_all"), "test:clear")
  )
}

serve(
  new_board(
    blocks = c(
      iris_data = new_dataset_block("iris"),
      cf_default = new_crossfilter_block(),
      cf_active = new_crossfilter_block(
        active_dims = list(.tbl = c("Species", "Sepal.Length"))
      ),
      cf_filtered = new_crossfilter_block(
        active_dims = list(.tbl = c("Species", "Sepal.Length")),
        filters = list(.tbl = list(Species = "setosa")),
        range_filters = list(.tbl = list(Sepal.Length = c(5, 6)))
      )
    ),
    links = c(
      new_link("iris_data", "cf_default", "data"),
      new_link("iris_data", "cf_active", "data"),
      new_link("iris_data", "cf_filtered", "data")
    )
  ),
  plugins = custom_plugins(list(
    ctrl_block = ctrl_block(
      server = test_ctrl_server,
      ui = test_ctrl_ui
    )
  )),
  id = "board"
)
