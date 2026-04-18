#' dm Filter Block Constructor
#'
#' Filters a `dm` object by applying one or more conditions to a selected
#' table. The filter cascades to related tables via semi-joins on foreign
#' key relationships (handled by `dm::dm_filter()`), keeping only rows that
#' match across the data model.
#'
#' The condition UI is reused from blockr.dplyr's filter block: each
#' condition is type-aware (categorical multi-select, numeric operator +
#' value, or free R expression) and conditions are combined with `&` or `|`.
#' A single table picker above the conditions selects which table in the dm
#' the conditions apply to. Changing the table clears existing conditions.
#'
#' @param table Name of the dm table to filter. Default `""` (none selected
#'   until data arrives).
#' @param state Filter state as used by blockr.dplyr's filter block — a list
#'   with `conditions` (list of condition objects) and `operator` (`"&"` or
#'   `"|"`).
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A block object for filtering dm objects.
#'
#' @examples
#' # Filter the ADAE table for subjects where SEX == "F"
#' new_dm_filter_block(
#'   table = "ADAE",
#'   state = list(
#'     conditions = list(list(
#'       type = "values", column = "SEX",
#'       values = list("F"), mode = "include"
#'     )),
#'     operator = "&"
#'   )
#' )
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList req isolate
#'
#' @export
new_dm_filter_block <- function(
  table = "",
  state = list(conditions = list(), operator = "&"),
  ...
) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        ns <- session$ns
        r_table <- reactiveVal(table)
        r_state <- reactiveVal(state)

        self_write <- new.env(parent = emptyenv())
        self_write$active <- FALSE

        # Send table picker options when dm arrives/changes
        observeEvent(data(), {
          opts <- build_dm_table_options(data())
          current <- isolate(r_table())
          tbl_names <- vapply(opts, `[[`, character(1), "value")
          selected <- if (current %in% tbl_names) {
            current
          } else if (length(tbl_names) > 0) {
            tbl_names[[1]]
          } else {
            ""
          }
          session$sendCustomMessage(
            "dm-table-picker",
            list(
              id = ns("table_input"),
              mode = "single",
              options = opts,
              selected = selected
            )
          )
          r_table(selected)
        })

        # R <- JS: table picker changed. ignoreNULL = TRUE (default) skips
        # the NULL-valued eval before the JS has sent a value.
        observeEvent(input$table_input, {
          new_tbl <- input$table_input
          if (!nzchar(new_tbl)) return()
          if (!identical(new_tbl, isolate(r_table()))) {
            r_table(new_tbl)
            # Clear existing conditions on table switch.
            r_state(list(conditions = list(), operator = "&"))
          }
        })

        # Send column summary for the currently selected table.
        observeEvent(list(r_table(), data()), {
          tbl <- r_table()
          req(nzchar(tbl))
          meta <- build_dm_column_summary(data(), tbl)
          session$sendCustomMessage(
            "filter-columns",
            list(id = ns("filter_input"), columns = meta)
          )
        })

        # On-demand unique-values request from JS
        observeEvent(input$filter_input_request_values, {
          col <- input$filter_input_request_values
          tbl <- r_table()
          if (!is.null(col) && nzchar(tbl) && inherits(data(), "dm")) {
            df <- tryCatch(
              as.data.frame(data()[[tbl]]),
              error = function(e) NULL
            )
            if (!is.null(df) && col %in% colnames(df)) {
              info <- blockr.dplyr::build_column_values(df, col)
              session$sendCustomMessage(
                "filter-column-values",
                list(id = ns("filter_input"), column = info)
              )
            }
          }
        })

        # JS -> R: user changed conditions
        observeEvent(input$filter_input, {
          self_write$active <- TRUE
          r_state(input$filter_input)
        })

        # R -> JS: external control changed state
        observeEvent(r_state(), {
          if (self_write$active) {
            self_write$active <- FALSE
          } else {
            session$sendCustomMessage(
              "filter-block-update",
              list(
                id = ns("filter_input"),
                state = normalize_dm_filter_state_for_js(r_state())
              )
            )
          }
        })

        list(
          expr = reactive({
            tbl <- r_table()
            s <- r_state()
            conds <- s$conditions %||% list()
            # No table or no conditions: pass dm through unchanged.
            # dm::dm_filter(data) with no filter args returns the dm as-is.
            if (!nzchar(tbl) || length(conds) == 0L) {
              return(bquote(dm::dm_filter(data)))
            }
            cond_filter <- blockr.dplyr::make_filter_expr(
              conds,
              s$operator %||% "&",
              isTRUE(s$preserveOrder)
            )
            # cond_filter is a bquoted `dplyr::filter(.(data), <cond>)`.
            # Extract just the <cond> and wrap it in
            # dm::dm_filter(data, <tbl> = <cond>).
            cond_inner <- cond_filter[[3L]]
            call <- call("dm_filter", quote(data))
            call[[tbl]] <- cond_inner
            call[[1L]] <- quote(dm::dm_filter)
            call
          }),
          state = list(
            table = r_table,
            state = r_state
          )
        )
      })
    },
    ui = function(id) {
      tagList(
        dm_table_picker_deps(),
        blockr.dplyr::blockr_input_dep(),
        blockr.dplyr::filter_block_dep(),
        div(
          class = "block-container",
          div(
            id = NS(id, "table_input"),
            class = "dm-filter-table-picker",
            style = "margin-bottom: 12px;"
          ),
          div(
            id = NS(id, "filter_input"),
            class = "filter-block-container"
          )
        )
      )
    },
    class = "dm_filter_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = c("table", "state"),
    ...
  )
}

#' Normalize filter state before sending to JS
#'
#' Mirrors blockr.dplyr's own normalization: wraps length-1 `values` vectors
#' in `as.list()` so JSON always emits an array (auto_unbox would otherwise
#' flatten them to strings and the filter UI would render one chip per
#' character).
#' @noRd
normalize_dm_filter_state_for_js <- function(state) {
  if (is.null(state)) return(state)
  conds <- state$conditions %||% list()
  state$conditions <- lapply(conds, function(cond) {
    if (!is.null(cond$values)) cond$values <- as.list(cond$values)
    cond
  })
  state
}

#' @method block_output dm_filter_block
#' @export
block_output.dm_filter_block <- function(x, result, session) {
  block_output.dm_block(x, result, session)
}

#' @method block_ui dm_filter_block
#' @export
block_ui.dm_filter_block <- function(id, x, ...) {
  block_ui.dm_block(id, x, ...)
}
