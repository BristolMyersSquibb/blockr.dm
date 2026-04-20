#' dm Pull Block Constructor
#'
#' This block extracts a single table from a dm object as a regular data
#' frame. Use this after filtering to get a specific table for further
#' analysis or visualization.
#'
#' @param table Character, the name of the table to extract. Default `""`.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A block object for extracting tables from dm objects
#'
#' @examples
#' new_dm_pull_block(table = "flights")
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList req isolate
#'
#' @export
new_dm_pull_block <- function(table = "", ...) {

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns
          r_table <- reactiveVal(table)

          observeEvent(input$table, {
            val <- input$table
            if (!is.null(val) && nzchar(val)) r_table(val)
          })

          observeEvent(data(), {
            opts <- build_dm_table_options(data())
            tbl_names <- vapply(opts, `[[`, character(1), "value")
            current <- isolate(r_table())
            selected <- if (current %in% tbl_names) {
              current
            } else if (length(tbl_names) > 0L) {
              tbl_names[[1L]]
            } else {
              ""
            }
            session$sendCustomMessage(
              "dm-table-picker",
              list(
                id = ns("table"),
                mode = "single",
                options = opts,
                selected = selected
              )
            )
            r_table(selected)
          })

          list(
            expr = reactive({
              tbl <- r_table()
              req(tbl, nzchar(tbl))
              bquote(
                dm::pull_tbl(data, .(tbl_sym)),
                list(tbl_sym = as.name(tbl))
              )
            }),
            state = list(
              table = r_table
            )
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        dm_table_picker_deps(),
        div(
          class = "block-container",
          div(
            id = NS(id, "table"),
            class = "dm-pull-table-picker"
          )
        )
      )
    },
    class = "dm_pull_block",
    ...
  )
}
