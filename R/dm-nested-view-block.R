#' Create dm Nested View Block
#'
#' Displays a dm object as an interactive nested table where clicking
#' a row in the parent table expands to show related child tables.
#'
#' @param root_table Character, the name of the root/parent table to display.
#'   Child tables with foreign keys pointing to this table will be shown
#'   as expandable nested content.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A block object for nested dm visualization
#'
#' @examples
#' # Display ADSL with nested ADAE and ADLB
#' new_dm_nested_view_block(root_table = "adsl")
#'
#' @export
new_dm_nested_view_block <- function(root_table = character(), ...) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns

          root_rv <- shiny::reactiveVal(root_table)

          # Get available tables from dm
          table_choices <- shiny::reactive({
            dm_obj <- if (shiny::is.reactive(data)) data() else data
            shiny::req(inherits(dm_obj, "dm"))
            names(dm::dm_get_tables(dm_obj))
          })

          # Update dropdown when dm changes
          shiny::observe({
            choices <- table_choices()
            current <- root_rv()
            if (length(current) == 0 || !current %in% choices) {
              # Try to find a table with PKs (likely parent)
              dm_obj <- if (shiny::is.reactive(data)) data() else data
              pks <- dm::dm_get_all_pks(dm_obj)
              if (nrow(pks) > 0) {
                current <- pks$table[1]
              } else {
                current <- choices[1]
              }
              root_rv(current)
            }
            shiny::updateSelectInput(
              session, "root_table",
              choices = choices,
              selected = current
            )
          })

          shiny::observeEvent(input$root_table, {
            root_rv(input$root_table)
          }, ignoreInit = TRUE)

          # Find child tables (tables with FK pointing to root)
          child_tables <- shiny::reactive({
            dm_obj <- if (shiny::is.reactive(data)) data() else data
            root <- root_rv()
            shiny::req(inherits(dm_obj, "dm"), length(root) > 0)

            fks <- dm::dm_get_all_fks(dm_obj)
            # Child tables are those with FK pointing to root
            fks$child_table[fks$parent_table == root]
          })

          list(
            expr = shiny::reactive({
              root <- root_rv()
              children <- child_tables()
              shiny::req(length(root) > 0, nzchar(root))

              # Build nesting expression - start with data
              if (length(children) == 0) {
                # No children to nest, just pull the root table
                return(bquote(dm::pull_tbl(data, .(as.name(root)))))
              }

              # Build chain: dm_nest_tbl(dm_nest_tbl(data, child1), child2) ...
              nest_expr <- quote(data)
              for (child in children) {
                nest_expr <- bquote(dm::dm_nest_tbl(.(nest_expr), .(as.name(child))))
              }

              # Final expression: pull_tbl(nested_dm, root)
              bquote(dm::pull_tbl(.(nest_expr), .(as.name(root))))
            }),
            state = list(
              root_table = root_rv
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::tags$p(
          class = "text-muted mb-2",
          "Display dm as nested table with expandable child rows."
        ),
        shiny::selectInput(
          ns("root_table"),
          "Root Table (parent)",
          choices = NULL,
          width = "100%"
        )
      )
    },
    dat_valid = function(data) {
      if (!inherits(data, "dm")) {
        stop("Input must be a dm object")
      }
    },
    class = "dm_nested_view_block",
    ...
  )
}

#' @rdname block_output.dm_block
#' @method block_output dm_nested_view_block
#' @export
block_output.dm_nested_view_block <- function(x, result, session) {
  # Find nested columns (list columns from dm_nest_tbl)
  nested_cols <- character()
  for (col_name in names(result)) {
    col_val <- result[[col_name]]
    if (is.list(col_val) && !is.data.frame(col_val)) {
      nested_cols <- c(nested_cols, col_name)
    }
  }

  # If no nested columns, just show a regular table
  if (length(nested_cols) == 0L) {
    return(reactable::renderReactable({
      reactable::reactable(result, striped = TRUE, bordered = TRUE)
    }))
  }

  # Build column definitions - hide nested columns
  col_defs <- list()
  for (col_name in names(result)) {
    if (col_name %in% nested_cols) {
      col_defs[[col_name]] <- reactable::colDef(show = FALSE)
    } else {
      col_defs[[col_name]] <- reactable::colDef()
    }
  }

  # Store data in local scope for details function
  local_result <- result
  local_nested_cols <- nested_cols

  # Create the details function for expandable rows
  details_fn <- function(index) {
    # Ensure index is a single integer
    idx <- as.integer(index)[1L]

    sections <- vector("list", length(local_nested_cols))
    for (i in seq_along(local_nested_cols)) {
      col_name <- local_nested_cols[i]
      child_data <- local_result[[col_name]][[idx]]

      # Check if empty
      is_empty <- is.null(child_data) ||
        !is.data.frame(child_data) ||
        NROW(child_data) == 0L

      if (is_empty) {
        sections[[i]] <- htmltools::tagList(
          htmltools::h6(
            style = "margin-top: 10px; margin-bottom: 5px; color: #666;",
            col_name
          ),
          htmltools::div(
            style = "color: #888; font-style: italic; margin-bottom: 10px;",
            paste("No", col_name, "records")
          )
        )
      } else {
        sections[[i]] <- htmltools::tagList(
          htmltools::h6(
            style = "margin-top: 10px; margin-bottom: 5px; color: #666;",
            col_name
          ),
          htmltools::div(
            style = "margin-bottom: 10px;",
            reactable::reactable(
              child_data,
              outlined = TRUE,
              compact = TRUE,
              pagination = FALSE
            )
          )
        )
      }
    }

    htmltools::div(
      style = "padding: 10px; background: #f8f9fa; border-left: 3px solid #dee2e6;",
      sections
    )
  }

  reactable::renderReactable({
    if (NROW(result) == 0L) return(NULL)
    reactable::reactable(
      result,
      columns = col_defs,
      details = details_fn,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      onClick = "expand",
      pagination = TRUE,
      defaultPageSize = 10
    )
  })
}

#' @rdname block_ui.dm_block
#' @method block_ui dm_nested_view_block
#' @export
block_ui.dm_nested_view_block <- function(id, x, ...) {
  ns <- shiny::NS(id)
  reactable::reactableOutput(ns("result"))
}
