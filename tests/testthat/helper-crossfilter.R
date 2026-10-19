# The crossfilter's payload for a block as its client asks for it: the
# block's state, with what its client would send.
cf_payload <- function(blk, data, subject_unit = NULL) {
  crossfilter_payload(
    data, blockr.core:::initial_block_state(blk), subject_unit
  )
}


# A crossfilter block's UI and server in a plain Shiny app, for the browser
# tests. The UI does not depend on the state.
cf_app_ui <- function(id) {
  new_crossfilter_block()$expr_ui(id)
}

cf_app_server <- function(..., id, data) {
  new_crossfilter_block(...)$expr_server(id, data = data)
}
