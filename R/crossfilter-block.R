#' @importFrom rlang .data :=
#' @importFrom stats setNames
#' @importFrom utils head
NULL

# Declare global variables to avoid R CMD check notes
utils::globalVariables(c(".count", ".selected"))

#' Crossfilter Block
#'
#' A crossfilter block for a single data frame. Internally wraps the data as a
#' one-table dm object and delegates to the dm crossfilter engine, giving it
#' search UI, date sliders, and density overlays for free.
#'
#' @param filters Named list of categorical filters. Each element is a character
#'   vector of selected values.
#'   E.g., `list(Species = c("setosa", "virginica"))`
#' @param range_filters Named list of range filters. Each element is a
#'   numeric vector of length 2 (min, max).
#'   E.g., `list(Sepal.Length = c(5, 7))`
#' @param active_dims Named list of active filter columns (per-table format,
#'   but only one table `.tbl`). E.g., `list(.tbl = c("Species", "Sepal.Width"))`
#' @param agg_func Optional aggregation function name (e.g., `"sum"`, `"mean"`)
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A blockr transform block that returns filtered data
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dm)
#'
#'   serve(
#'     new_crossfilter_block(),
#'     data = list(data = iris)
#'   )
#' }
new_crossfilter_block <- function(
    filters = list(),
    range_filters = list(),
    active_dims = list(),
    agg_func = NULL,
    ...
) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      # After external_ctrl injection: filters, range_filters, active_dims
      # may be reactiveVals (AI-controllable) or plain values (standalone).
      is_reactive <- inherits(filters, "reactiveVal")

      if (is_reactive) {
        # Create per-table reactiveVals for the dm crossfilter engine
        dm_rv_filters <- shiny::reactiveVal(
          if (length(shiny::isolate(filters())) > 0) {
            list(.tbl = shiny::isolate(filters()))
          } else {
            list()
          }
        )
        dm_rv_range_filters <- shiny::reactiveVal(
          if (length(shiny::isolate(range_filters())) > 0) {
            list(.tbl = shiny::isolate(range_filters()))
          } else {
            list()
          }
        )

        # Sync flat → per-table (AI sets flat filters → dm engine gets per-table)
        shiny::observeEvent(filters(), {
          val <- if (length(filters()) > 0) list(.tbl = filters()) else list()
          if (!identical(dm_rv_filters(), val)) dm_rv_filters(val)
        })
        shiny::observeEvent(range_filters(), {
          val <- if (length(range_filters()) > 0) {
            list(.tbl = range_filters())
          } else {
            list()
          }
          if (!identical(dm_rv_range_filters(), val)) dm_rv_range_filters(val)
        })

        # Sync per-table → flat (user UI changes → flat reactiveVals)
        shiny::observeEvent(dm_rv_filters(), {
          flat <- dm_rv_filters()[[".tbl"]] %||% list()
          if (!identical(filters(), flat)) filters(flat)
        })
        shiny::observeEvent(dm_rv_range_filters(), {
          flat <- dm_rv_range_filters()[[".tbl"]] %||% list()
          if (!identical(range_filters(), flat)) range_filters(flat)
        })

        dm_server <- dm_crossfilter_server_factory(
          active_dims, dm_rv_filters, dm_rv_range_filters, measure = NULL,
          agg_func = agg_func
        )
      } else {
        # Original non-reactive path
        dm_filters <- if (length(filters) > 0) list(.tbl = filters) else list()
        dm_range_filters <- if (length(range_filters) > 0) {
          list(.tbl = range_filters)
        } else {
          list()
        }
        dm_server <- dm_crossfilter_server_factory(
          active_dims, dm_filters, dm_range_filters, measure = NULL,
          agg_func = agg_func
        )
      }

      # Wrap data frame as a single-table dm, delegate to dm crossfilter
      dm_data <- shiny::reactive({
        df <- data()
        shiny::req(is.data.frame(df))
        dm::dm(.tbl = df)
      })

      result <- dm_server(id, dm_data)

      # Convert dm expression to dplyr expression
      dm_expr <- result$expr
      result$expr <- shiny::reactive({
        e <- dm_expr()
        # If identity, pass through
        e_str <- deparse(e)
        if (any(grepl("identity", e_str))) {
          return(quote(identity(data)))
        }
        # dm_filter(data, .tbl = cond) -> extract condition for .tbl
        # The expression is: dm::dm_filter(data, .tbl = <condition>)
        cond <- e[[".tbl"]]
        if (is.null(cond)) {
          return(quote(identity(data)))
        }
        as.call(list(quote(dplyr::filter), quote(data), cond))
      })

      # State: use injected reactiveVals if available, otherwise unwrap from dm
      if (is_reactive) {
        result$state <- list(
          active_dims = active_dims,
          filters = filters,
          range_filters = range_filters,
          agg_func = result$state$agg_func
        )
      } else {
        dm_state <- result$state

        # Create flat reactiveVals that sync bidirectionally with dm engine
        flat_filters <- shiny::reactiveVal(list())
        flat_range_filters <- shiny::reactiveVal(list())

        # Sync per-table → flat (UI changes in dm engine → flat reactiveVals)
        shiny::observeEvent(dm_state$filters(), {
          flat <- dm_state$filters()[[".tbl"]] %||% list()
          if (!identical(flat_filters(), flat)) flat_filters(flat)
        })
        shiny::observeEvent(dm_state$range_filters(), {
          flat <- dm_state$range_filters()[[".tbl"]] %||% list()
          if (!identical(flat_range_filters(), flat)) flat_range_filters(flat)
        })

        # Sync flat → per-table (external writes to flat → dm engine)
        shiny::observeEvent(flat_filters(), {
          val <- if (length(flat_filters()) > 0) {
            list(.tbl = flat_filters())
          } else {
            list()
          }
          if (!identical(dm_state$filters(), val)) dm_state$filters(val)
        })
        shiny::observeEvent(flat_range_filters(), {
          val <- if (length(flat_range_filters()) > 0) {
            list(.tbl = flat_range_filters())
          } else {
            list()
          }
          if (!identical(dm_state$range_filters(), val)) {
            dm_state$range_filters(val)
          }
        })

        result$state <- list(
          active_dims = dm_state$active_dims,
          filters = flat_filters,
          range_filters = flat_range_filters,
          agg_func = dm_state$agg_func
        )
      }

      result
    },
    ui = dm_crossfilter_ui,
    dat_valid = function(data) {
      if (!is.data.frame(data)) {
        stop("Input must be a data frame")
      }
    },
    allow_empty_state = c("active_dims", "filters", "range_filters", "agg_func"),
    external_ctrl = TRUE,
    class = "crossfilter_block",
    ...
  )
}
