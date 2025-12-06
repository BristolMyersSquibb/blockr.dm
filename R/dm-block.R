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

#' Infer primary and foreign keys from column name equality
#'
#' For each pair of tables, finds columns with matching names. If one table
#' has unique values in that column (potential PK) and the other has
#' non-unique values (potential FK), establishes the relationship.
#'
#' @param dm_obj A dm object
#' @return A dm object with inferred keys added
#' @keywords internal
infer_keys_from_column_names <- function(dm_obj) {
  table_names <- names(dm_obj)

  if (length(table_names) < 2) {
    return(dm_obj)
  }

  # Get all column names for each table
  all_cols <- lapply(table_names, function(tbl) {
    names(dm_obj[[tbl]])
  })
  names(all_cols) <- table_names

  # Find columns that appear in multiple tables
  all_col_names <- unlist(all_cols)
  col_counts <- table(all_col_names)
  shared_cols <- names(col_counts[col_counts > 1])

  if (length(shared_cols) == 0) {
    return(dm_obj)
  }

  # For each shared column, determine PK/FK relationships
  for (col in shared_cols) {
    # Find tables that have this column
    tables_with_col <- table_names[vapply(all_cols, function(cols) col %in% cols, logical(1))]

    if (length(tables_with_col) < 2) next

    # Check which tables have unique values for this column (potential PK)
    uniqueness <- vapply(tables_with_col, function(tbl) {
      vals <- dm_obj[[tbl]][[col]]
      # A column is a PK candidate if all values are unique and no NAs
      !anyNA(vals) && !anyDuplicated(vals)
    }, logical(1))

    pk_tables <- tables_with_col[uniqueness]
    fk_tables <- tables_with_col[!uniqueness]

    # If exactly one table has unique values and others don't, establish relationships
    if (length(pk_tables) == 1 && length(fk_tables) >= 1) {
      pk_table <- pk_tables[1]

      # Check existing PKs to see if this table already has one
      existing_pks <- dm::dm_get_all_pks(dm_obj)
      has_pk <- pk_table %in% existing_pks$table

      # Add PK if not already set
      if (!has_pk) {
        dm_obj <- dm::dm_add_pk(dm_obj, !!rlang::sym(pk_table), !!rlang::sym(col))
      }

      # Add FKs from other tables
      for (fk_table in fk_tables) {
        # Check existing FKs
        existing_fks <- dm::dm_get_all_fks(dm_obj)
        has_fk <- any(
          existing_fks$child_table == fk_table &
            existing_fks$parent_table == pk_table
        )

        if (!has_fk) {
          dm_obj <- tryCatch(
            dm::dm_add_fk(dm_obj, !!rlang::sym(fk_table), !!rlang::sym(col), !!rlang::sym(pk_table)),
            error = function(e) dm_obj
          )
        }
      }
    }
  }

  dm_obj
}

#' Create dm Block Constructor
#'
#' This block combines multiple data frames into a dm (data model) object.
#'
#' @param infer_keys Logical, whether to automatically infer primary and foreign
#'   key relationships from columns with matching names. Default is `TRUE`.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A block object for creating dm objects
#'
#' @examples
#' # Create a dm block
#' new_dm_block()
#'
#' # Create a dm block with auto-inferred relationships
#' new_dm_block(infer_keys = TRUE)
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
new_dm_block <- function(infer_keys = TRUE, ...) {
  blockr.core::new_transform_block(
    server = function(id, ...args) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Reactive value for infer_keys state
          infer_keys_rv <- shiny::reactiveVal(infer_keys)

          # Update from UI input
          shiny::observeEvent(input$infer_keys, {
            infer_keys_rv(input$infer_keys)
          }, ignoreInit = TRUE)

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
              do_infer <- infer_keys_rv()

              # Require at least one input before building expression
              shiny::req(length(table_names) > 0)

              # Build the dm() call with named tables
              # We use bquote with splice to build: dm::dm(name1 = data1, name2 = data2, ...)
              table_syms <- lapply(table_names, as.name)
              names(table_syms) <- names(table_names)

              dm_call <- bquote(
                dm::dm(..(tables)),
                list(tables = table_syms),
                splice = TRUE
              )

              if (!do_infer) {
                return(dm_call)
              }

              # Wrap in local() to add key inference logic
              bquote(
                local({
                  base_dm <- .(dm_call)
                  blockr.dm:::infer_keys_from_column_names(base_dm)
                })
              )
            }),
            state = list(
              infer_keys = infer_keys_rv
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::div(
          class = "block-container",
          shiny::tags$p(
            class = "text-muted mb-2",
            "Combines connected data frames into a dm (data model) object."
          ),
          shiny::checkboxInput(
            ns("infer_keys"),
            "Infer relationships from column names",
            value = infer_keys
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
#' Displays dm structure as an interactive diagram showing tables and relationships.
#'
#' @param x The block object
#' @param result The dm result
#' @param session Shiny session
#'
#' @method block_output dm_block
#' @export
block_output.dm_block <- function(x, result, session) {
  DiagrammeR::renderGrViz({
    if (!inherits(result, "dm")) {
      return(NULL)
    }
    dm::dm_draw(result, view_type = "keys_only")
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
    DiagrammeR::grVizOutput(shiny::NS(id, "result"), height = "300px")
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
