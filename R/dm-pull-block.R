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
#' @section External control:
#' `table` is externally controllable (see
#' [blockr.core::external_ctrl_vars()]): a board update or an assistant can
#' switch the pulled table with a `mod` delta, and the picker follows.
#'
#' @examples
#' new_dm_pull_block(table = "flights")
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList req isolate
#'
#' @export
new_dm_pull_block <- function(table = "", ...) {

  # Read inside the server closure, i.e. after this call returns: left as a
  # promise it carries the caller's environment, and any harness that revives
  # the block in a fresh R process forces it there, where the caller's locals
  # are gone.
  force(table)

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_table <- reactiveVal(table)

          # Set by the observers that own a write, so the mirror below can
          # tell the user's own edit from someone else's and skip the echo.
          self_write <- new.env(parent = emptyenv())
          self_write$table <- FALSE

          set_table <- function(val) {
            if (identical(val, isolate(r_table()))) {
              return(invisible(FALSE))
            }
            self_write$table <- TRUE
            r_table(val)
            invisible(TRUE)
          }

          observeEvent(input$table, {
            val <- input$table
            if (!is.null(val) && nzchar(val)) set_table(val)
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
            dm_picker_push(session, "table", data(), selected, "single")
            set_table(selected)
          })

          # R -> JS: mirror an externally set table into the picker, so the
          # widget cannot show one table while the block pulls another.
          observeEvent(r_table(), {
            if (self_write$table) {
              self_write$table <- FALSE
              return()
            }
            dm_picker_push(session, "table", data(), r_table(), "single")
          }, ignoreInit = TRUE)

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
    external_ctrl = TRUE,
    ...
  )
}
