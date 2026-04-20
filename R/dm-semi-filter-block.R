#' dm Semi-Filter Block Constructor
#'
#' Restricts a `dm` to rows whose `key_col` values appear in a second
#' `data.frame` input (`ids`). Equivalent to a semi-join on one chosen
#' table of the dm; because `dm::dm_filter()` cascades through foreign
#' keys, the restriction propagates to every related table.
#'
#' Useful for bridging drill-down outputs (which produce data frames)
#' back into a dm: click a patient on a trajectory chart, feed the
#' resulting data frame in via `ids`, and downstream dm consumers
#' (patient profile, flatten, summaries) all see the restricted dm.
#'
#' @param table Character. Name of the dm table to filter. Default
#'   `"adsl"`.
#' @param key_col Character. Column name used for the semi-join. Must
#'   exist in both `ids` and in `table`. Default `"USUBJID"`.
#' @param distinct_only Logical. `TRUE` (default) uses `unique()` of
#'   `ids[[key_col]]` before matching. Set `FALSE` to pass the raw
#'   column through.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A block object for semi-filtering a dm by ids from a
#'   secondary data frame.
#'
#' @examples
#' # Typical wiring: a drill-down chart selects a USUBJID, which
#' # restricts the whole dm to that patient for a patient-profile view.
#' new_dm_semi_filter_block(table = "adsl", key_col = "USUBJID")
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent
#'   updateSelectInput selectInput checkboxInput NS div tagList req
#'   isolate
#'
#' @export
new_dm_semi_filter_block <- function(
  table = "adsl",
  key_col = "USUBJID",
  distinct_only = TRUE,
  ...
) {
  blockr.core::new_transform_block(
    server = function(id, dm, ids) {
      moduleServer(id, function(input, output, session) {
        ns <- session$ns
        r_table <- reactiveVal(table)
        r_key <- reactiveVal(key_col)
        r_distinct <- reactiveVal(isTRUE(distinct_only))

        # Populate table picker from incoming dm.
        observeEvent(dm(), {
          opts <- build_dm_table_options(dm())
          tbl_names <- vapply(opts, `[[`, character(1), "value")
          current <- isolate(r_table())
          selected <- if (current %in% tbl_names) current
            else if (length(tbl_names)) tbl_names[[1L]]
            else ""
          updateSelectInput(
            session, "table",
            choices = tbl_names,
            selected = selected
          )
          r_table(selected)
        })

        # Populate key_col picker from intersection of the selected
        # dm table's columns and the ids data frame's columns.
        observeEvent(list(dm(), ids(), r_table()), {
          tbl <- r_table()
          req(inherits(dm(), "dm"), is.data.frame(ids()), nzchar(tbl))
          tbls <- dm::dm_get_tables(dm())
          if (!tbl %in% names(tbls)) return()
          candidates <- intersect(colnames(tbls[[tbl]]), colnames(ids()))
          current <- isolate(r_key())
          selected <- if (current %in% candidates) current
            else if (length(candidates)) candidates[[1L]]
            else ""
          updateSelectInput(
            session, "key_col",
            choices = candidates,
            selected = selected
          )
          if (!identical(selected, current)) r_key(selected)
        })

        observeEvent(input$table, {
          val <- input$table
          if (!is.null(val) && nzchar(val) && !identical(val, isolate(r_table()))) {
            r_table(val)
          }
        })

        observeEvent(input$key_col, {
          val <- input$key_col
          if (!is.null(val) && nzchar(val) && !identical(val, isolate(r_key()))) {
            r_key(val)
          }
        })

        observeEvent(input$distinct_only, {
          r_distinct(isTRUE(input$distinct_only))
        }, ignoreNULL = TRUE, ignoreInit = TRUE)

        list(
          expr = reactive({
            tbl <- r_table()
            key <- r_key()
            # Empty state: pass dm through unchanged.
            if (!nzchar(tbl) || !nzchar(key)) {
              return(bquote(dm::dm_filter(dm)))
            }
            key_sym <- as.name(key)
            inner <- if (isTRUE(r_distinct())) {
              bquote(.(key_sym) %in% unique(ids[[.(key)]]))
            } else {
              bquote(.(key_sym) %in% ids[[.(key)]])
            }
            call <- call("dm_filter", quote(dm))
            call[[tbl]] <- inner
            call[[1L]] <- quote(dm::dm_filter)
            call
          }),
          state = list(
            table = r_table,
            key_col = r_key,
            distinct_only = r_distinct
          )
        )
      })
    },
    ui = function(id) {
      tagList(
        div(
          class = "block-container",
          selectInput(
            inputId = NS(id, "table"),
            label = "Table to filter",
            choices = character(),
            selected = table
          ),
          selectInput(
            inputId = NS(id, "key_col"),
            label = "Key column",
            choices = character(),
            selected = key_col
          ),
          checkboxInput(
            inputId = NS(id, "distinct_only"),
            label = "Distinct values only",
            value = isTRUE(distinct_only)
          )
        )
      )
    },
    dat_valid = function(dm, ids) {
      stopifnot(inherits(dm, "dm"), is.data.frame(ids))
    },
    expr_type = "bquoted",
    class = "dm_semi_filter_block",
    allow_empty_state = c("table", "key_col"),
    ...
  )
}

#' @method block_output dm_semi_filter_block
#' @export
block_output.dm_semi_filter_block <- function(x, result, session) {
  block_output.dm_block(x, result, session)
}

#' @method block_ui dm_semi_filter_block
#' @export
block_ui.dm_semi_filter_block <- function(id, x, ...) {
  block_ui.dm_block(id, x, ...)
}
