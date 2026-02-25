library(blockr.core)
library(blockr.dm)

app <- serve(
  new_crossfilter_block(),
  data = list(data = iris)
)

shiny::runApp(app, port = 7860, host = "0.0.0.0")
