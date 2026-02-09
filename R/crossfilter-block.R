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
#' search UI, duckdb backend, date sliders, and density overlays for free.
#'
#' @param filters Named list of categorical filters. Each element is a character
#'   vector of selected values.
#'   E.g., `list(Species = c("setosa", "virginica"))`
#' @param range_filters Named list of range filters. Each element is a
#'   numeric vector of length 2 (min, max).
#'   E.g., `list(Sepal.Length = c(5, 7))`
#' @param active_dims Named list of active filter columns (per-table format,
#'   but only one table `.tbl`). E.g., `list(.tbl = c("Species", "Sepal.Width"))`
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
    ...
) {
  # Convert flat filters to per-table format for the dm crossfilter engine
  dm_filters <- if (length(filters) > 0) list(.tbl = filters) else list()
  dm_range_filters <- if (length(range_filters) > 0) {
    list(.tbl = range_filters)
  } else {
    list()
  }

  dm_server <- dm_crossfilter_server_factory(active_dims, dm_filters, dm_range_filters, measure = NULL)

  blockr.core::new_transform_block(
    server = function(id, data) {
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

      # Convert state: unwrap per-table format back to flat
      dm_state <- result$state
      result$state <- list(
        active_dims = dm_state$active_dims,
        filters = function() {
          f <- dm_state$filters()
          f[[".tbl"]] %||% list()
        },
        range_filters = function() {
          f <- dm_state$range_filters()
          f[[".tbl"]] %||% list()
        }
      )

      result
    },
    ui = dm_crossfilter_ui,
    dat_valid = function(data) {
      if (!is.data.frame(data)) {
        stop("Input must be a data frame")
      }
    },
    allow_empty_state = c("active_dims", "filters", "range_filters"),
    class = "crossfilter_block",
    ...
  )
}
