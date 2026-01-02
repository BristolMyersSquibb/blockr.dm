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

    # If at least one table has unique values and others don't, establish relationship
    # When multiple PK candidates exist, pick the first one
    if (length(pk_tables) >= 1 && length(fk_tables) >= 1) {
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
#' This block combines multiple data frames and/or dm objects into a single
#' dm (data model) object.
#'
#' @param infer_keys Logical, whether to automatically infer primary and foreign
#'   key relationships from columns with matching names. Default is `TRUE`.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A block object for creating dm objects
#'
#' @details
#' Inputs can be data frames or existing dm objects:
#' - Data frames are added as new tables in the dm
#' - Existing dm objects have their tables merged into the result
#' - If table names conflict, later inputs overwrite earlier ones
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

          # Analyze inputs to classify as dm or data.frame, and pre-compute keys
          input_info <- shiny::reactive({
            nms <- names(...args)
            display_nms <- dot_args_names(...args)
            if (is.null(display_nms)) {
              display_nms <- paste0("table_", nms)
            }

            # Classify each input and collect data
            is_dm <- logical(length(nms))
            all_tables <- list()

            for (i in seq_along(nms)) {
              nm <- nms[i]
              arg <- ...args[[nm]]
              if (shiny::is.reactive(arg)) arg <- arg()

              if (inherits(arg, "dm")) {
                is_dm[i] <- TRUE
                # Extract tables from dm
                for (tbl_name in names(dm::dm_get_tables(arg))) {
                  all_tables[[tbl_name]] <- arg[[tbl_name]]
                }
              } else {
                is_dm[i] <- FALSE
                all_tables[[display_nms[i]]] <- arg
              }
            }

            list(
              nms = nms,
              display_nms = display_nms,
              is_dm = is_dm,
              all_tables = all_tables
            )
          })

          # Compute inferred keys based on current data
          inferred_keys <- shiny::reactive({
            info <- input_info()
            do_infer <- infer_keys_rv()

            if (!do_infer || length(info$all_tables) < 2) {
              return(list(pks = list(), fks = list()))
            }

            # Analyze tables for key relationships
            table_names <- names(info$all_tables)
            all_cols <- lapply(table_names, function(tbl) names(info$all_tables[[tbl]]))
            names(all_cols) <- table_names

            # Find shared columns
            all_col_names <- unlist(all_cols)
            col_counts <- table(all_col_names)
            shared_cols <- names(col_counts[col_counts > 1])

            pks <- list()
            fks <- list()

            for (col in shared_cols) {
              tables_with_col <- table_names[vapply(all_cols, function(cols) col %in% cols, logical(1))]
              if (length(tables_with_col) < 2) next

              # Check uniqueness
              uniqueness <- vapply(tables_with_col, function(tbl) {
                vals <- info$all_tables[[tbl]][[col]]
                !anyNA(vals) && !anyDuplicated(vals)
              }, logical(1))

              pk_tables <- tables_with_col[uniqueness]
              fk_tables <- tables_with_col[!uniqueness]

              # If at least one table has unique values and others don't, establish relationship
              # When multiple PK candidates exist, pick the first one
              if (length(pk_tables) >= 1 && length(fk_tables) >= 1) {
                pk_table <- pk_tables[1]
                pks <- c(pks, list(list(table = pk_table, column = col)))

                for (fk_table in fk_tables) {
                  fks <- c(fks, list(list(
                    child_table = fk_table,
                    child_column = col,
                    parent_table = pk_table
                  )))
                }
              }
            }

            list(pks = pks, fks = fks)
          })

          list(
            expr = shiny::reactive({
              info <- input_info()
              keys <- inferred_keys()

              shiny::req(length(info$nms) > 0)

              # Build base expression for creating dm
              base_expr <- bquote(
                local({
                  result_dm <- dm::dm()
                  input_names <- .(info$display_nms)
                  input_is_dm <- .(info$is_dm)

                  for (i in seq_along(input_names)) {
                    input_data <- get(.(info$nms)[i])
                    input_name <- input_names[i]

                    if (input_is_dm[i]) {
                      result_dm <- dm::dm_bind(result_dm, input_data)
                    } else {
                      new_dm <- dm::dm()
                      new_dm <- dm::dm(new_dm, !!rlang::sym(input_name) := input_data)
                      result_dm <- dm::dm_bind(result_dm, new_dm)
                    }
                  }
                  result_dm
                })
              )

              # If no keys to add, return base expression
              if (length(keys$pks) == 0 && length(keys$fks) == 0) {
                return(base_expr)
              }

              # Build expression with hardcoded key additions using nested calls
              result_expr <- base_expr

              for (pk in keys$pks) {
                table_sym <- as.name(pk$table)
                col_sym <- as.name(pk$column)
                # Wrap result in dm_add_pk call
                result_expr <- bquote(
                  dm::dm_add_pk(.(inner), .(tbl), .(col)),
                  list(inner = result_expr, tbl = table_sym, col = col_sym)
                )
              }

              for (fk in keys$fks) {
                child_sym <- as.name(fk$child_table)
                col_sym <- as.name(fk$child_column)
                parent_sym <- as.name(fk$parent_table)
                # Wrap result in dm_add_fk call
                result_expr <- bquote(
                  dm::dm_add_fk(.(inner), .(child), .(col), .(parent)),
                  list(
                    inner = result_expr,
                    child = child_sym,
                    col = col_sym,
                    parent = parent_sym
                  )
                )
              }

              result_expr
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
            "Combines data frames and/or dm objects into a single dm."
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
      # Check all inputs are data frames or dm objects
      for (arg in ...args) {
        if (!is.data.frame(arg) && !inherits(arg, "dm")) {
          stop("All inputs must be data frames or dm objects")
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
