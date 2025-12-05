#' @importFrom blockr.core block_output block_ui block_render_trigger
NULL

# Helper function to extract argument names for variadic blocks
# Copied from blockr.core:::dot_args_names (not exported)
dot_args_names <- function(x) {
  res <- names(x)
  unnamed <- grepl("^[1-9][0-9]*$", res)

  if (all(unnamed)) {
    return(NULL)
  }

  if (any(unnamed)) {
    return(replace(res, unnamed, ""))
  }

  res
}

#' Create dm Block Constructor
#'
#' This block combines multiple data frames into a dm (data model) object.
#'
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A block object for creating dm objects
#'
#' @examples
#' # Create a dm block
#' new_dm_block()
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dm)
#'
#'   # Combine multiple tables into a dm
#'   serve(
#'     new_board(
#'       blocks = list(
#'         airlines = new_dataset_block(dataset = "airlines", package = "nycflights13"),
#'         flights = new_dataset_block(dataset = "flights", package = "nycflights13"),
#'         dm_obj = new_dm_block()
#'       ),
#'       links = c(
#'         new_link("airlines", "dm_obj", "1"),
#'         new_link("flights", "dm_obj", "2")
#'       )
#'     )
#'   )
#' }
#'
#' @export
new_dm_block <- function(...) {
  blockr.core::new_transform_block(
    server = function(id, ...args) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Get table names from input connections
          arg_names <- shiny::reactive({
            nms <- names(...args)
            # Use dot_args_names to get proper names or use numeric indices
            display_nms <- dot_args_names(...args)
            if (is.null(display_nms)) {
              display_nms <- paste0("table_", nms)
            }
            stats::setNames(nms, display_nms)
          })

          list(
            expr = shiny::reactive({
              table_names <- arg_names()

              # Require at least one input before building expression
              shiny::req(length(table_names) > 0)

              # Build the dm() call with named tables
              # We use bquote with splice to build: dm::dm(name1 = data1, name2 = data2, ...)
              table_syms <- lapply(table_names, as.name)
              names(table_syms) <- names(table_names)

              bquote(
                dm::dm(..(tables)),
                list(tables = table_syms),
                splice = TRUE
              )
            }),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      shiny::tagList(
        shiny::div(
          class = "block-container",
          shiny::tags$p(
            class = "text-muted mb-0",
            "Combines connected data frames into a dm (data model) object."
          )
        )
      )
    },
    dat_valid = function(...args) {
      # Check that we have at least one input
      if (length(...args) < 1L) {
        stop("At least one data input is required")
      }
      # Check all inputs are data frames
      for (arg in ...args) {
        if (!is.data.frame(arg)) {
          stop("All inputs must be data frames")
        }
      }
    },
    allow_empty_state = TRUE,
    class = "dm_block",
    ...
  )
}

#' Custom output for dm blocks
#'
#' Displays dm structure instead of trying to render as a table.
#'
#' @param x The block object
#' @param result The dm result
#' @param session Shiny session
#'
#' @method block_output dm_block
#' @export
block_output.dm_block <- function(x, result, session) {

  shiny::renderPrint({
    if (!inherits(result, "dm")) {
      return(result)
    }

    # Get table info
    tables <- dm::dm_get_tables(result)
    table_names <- names(tables)

    cat("dm object with", length(table_names), "tables:\n\n")

    for (tbl_name in table_names) {
      tbl <- tables[[tbl_name]]
      cat("*", tbl_name, "-", nrow(tbl), "rows,", ncol(tbl), "cols\n")
    }

    # Show keys if any
    pks <- tryCatch(dm::dm_get_all_pks(result), error = function(e) NULL)
    if (!is.null(pks) && nrow(pks) > 0) {
      cat("\nPrimary keys:\n")
      for (i in seq_len(nrow(pks))) {
        cat("*", pks$table[i], ":", as.character(pks$pk_col[[i]]), "\n")
      }
    }

    fks <- tryCatch(dm::dm_get_all_fks(result), error = function(e) NULL)
    if (!is.null(fks) && nrow(fks) > 0) {
      cat("\nForeign keys:\n")
      for (i in seq_len(nrow(fks))) {
        cat("*", fks$child_table[i], ".", as.character(fks$child_fk_cols[[i]]),
            "->", fks$parent_table[i], "\n")
      }
    }
  })
}

#' Custom UI for dm blocks
#'
#' @param id Namespace ID
#' @param x The block object
#' @param ... Additional arguments
#'
#' @method block_ui dm_block
#' @export
block_ui.dm_block <- function(id, x, ...) {

  shiny::tagList(
    shiny::verbatimTextOutput(shiny::NS(id, "result"))
  )
}

#' Custom render trigger for dm blocks
#'
#' Override transform_block's render trigger which tries to access
#' board options that may not be set. dm blocks don't need pagination.
#'
#' @param x The block object
#' @param session Shiny session
#'
#' @method block_render_trigger dm_block
#' @export
block_render_trigger.dm_block <- function(x, session = blockr.core::get_session()) {
  NULL
}
