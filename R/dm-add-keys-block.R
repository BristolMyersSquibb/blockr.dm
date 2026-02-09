#' dm Add Keys Block Constructor
#'
#' This block adds primary and foreign key relationships to a dm object.
#' It allows selecting a table and column to set as primary key, and
#' optionally link it as a foreign key from one or more other tables.
#'
#' @param pk_table Character, the table to add primary key to. Default "".
#' @param pk_column Character, the column to use as primary key. Default "".
#' @param fk_tables Character vector, tables containing the foreign key.
#'   Default `character(0)`.
#' @param fk_column Character, the column to use as foreign key. Default "".
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A block object for adding keys to dm objects
#'
#' @details
#' Primary keys uniquely identify rows in a table. Foreign keys reference
#' the primary key of another table, establishing relationships.
#'
#' For ADaM datasets, USUBJID is typically the primary key in ADSL
#' and a foreign key in other tables (ADAE, ADLB, etc.).
#'
#' @examples
#' # Add USUBJID as key linking ADSL to multiple tables
#' new_dm_add_keys_block(
#'   pk_table = "adsl",
#'   pk_column = "USUBJID",
#'   fk_tables = c("adae", "adlb"),
#'   fk_column = "USUBJID"
#' )
#'
#' @export
new_dm_add_keys_block <- function(
  pk_table = "",
  pk_column = "",
  fk_tables = character(0),
  fk_column = "",
  ...
) {

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Reactive values
          r_pk_table <- shiny::reactiveVal(pk_table)
          r_pk_column <- shiny::reactiveVal(pk_column)
          r_fk_tables <- shiny::reactiveVal(fk_tables)
          r_fk_column <- shiny::reactiveVal(fk_column)

          # Update reactives from inputs
          shiny::observeEvent(input$pk_table, r_pk_table(input$pk_table))
          shiny::observeEvent(input$pk_column, r_pk_column(input$pk_column))
          shiny::observeEvent(input$fk_tables, {
            r_fk_tables(input$fk_tables %||% character(0))
          }, ignoreNULL = FALSE, ignoreInit = TRUE)
          shiny::observeEvent(input$fk_column, r_fk_column(input$fk_column))

          # Update table choices when dm changes
          shiny::observeEvent(data(), {
            dm_obj <- data()
            if (inherits(dm_obj, "dm")) {
              tables <- names(dm::dm_get_tables(dm_obj))

              # Update PK table
              current_pk <- r_pk_table()
              selected_pk <- if (current_pk %in% tables) current_pk else tables[1]
              shiny::updateSelectInput(session, "pk_table", choices = tables, selected = selected_pk)
              r_pk_table(selected_pk)

              # Update FK tables (multi-select)
              current_fk <- r_fk_tables()
              selected_fk <- current_fk[current_fk %in% tables]
              shiny::updateSelectizeInput(
                session, "fk_tables",
                choices = tables,
                selected = selected_fk
              )
            }
          })

          # Update PK column choices when pk_table changes
          shiny::observeEvent(list(data(), r_pk_table()), {
            dm_obj <- data()
            tbl <- r_pk_table()
            if (inherits(dm_obj, "dm") && nzchar(tbl)) {
              tbl_data <- tryCatch(
                dm::pull_tbl(dm_obj, !!tbl),
                error = function(e) NULL
              )
              if (!is.null(tbl_data)) {
                cols <- colnames(tbl_data)
                current <- r_pk_column()
                selected <- if (current %in% cols) current else cols[1]
                shiny::updateSelectInput(session, "pk_column", choices = cols, selected = selected)
                r_pk_column(selected)
              }
            }
          })

          # Update FK column choices when fk_tables changes
          shiny::observeEvent(list(data(), r_fk_tables()), {
            dm_obj <- data()
            tbls <- r_fk_tables()
            if (inherits(dm_obj, "dm") && length(tbls) > 0) {
              # Show columns common to ALL selected FK tables
              common_cols <- NULL
              for (tbl in tbls) {
                tbl_data <- tryCatch(
                  dm::pull_tbl(dm_obj, !!tbl),
                  error = function(e) NULL
                )
                if (is.null(tbl_data)) next
                if (is.null(common_cols)) {
                  common_cols <- colnames(tbl_data)
                } else {
                  common_cols <- intersect(common_cols, colnames(tbl_data))
                }
              }
              if (!is.null(common_cols) && length(common_cols) > 0) {
                current <- r_fk_column()
                selected <- if (current %in% common_cols) current else common_cols[1]
                shiny::updateSelectInput(session, "fk_column", choices = c("", common_cols), selected = selected)
              }
            } else {
              shiny::updateSelectInput(session, "fk_column", choices = character(), selected = "")
            }
          })

          list(
            expr = shiny::reactive({
              pk_tbl <- r_pk_table()
              pk_col <- r_pk_column()
              fk_tbls <- r_fk_tables()
              fk_col <- r_fk_column()

              shiny::req(pk_tbl, nzchar(pk_tbl), pk_col, nzchar(pk_col))

              # Start with adding PK (force = TRUE to allow overwrite)
              result_expr <- bquote(
                dm::dm_add_pk(data, .(pk_tbl_sym), .(pk_col_sym), force = TRUE),
                list(
                  pk_tbl_sym = as.name(pk_tbl),
                  pk_col_sym = as.name(pk_col)
                )
              )

              # Chain dm_add_fk for each FK table
              if (length(fk_tbls) > 0 && nzchar(fk_col)) {
                for (fk_tbl in fk_tbls) {
                  result_expr <- bquote(
                    dm::dm_add_fk(.(inner), .(fk_tbl_sym), .(fk_col_sym), .(pk_tbl_sym)),
                    list(
                      inner = result_expr,
                      fk_tbl_sym = as.name(fk_tbl),
                      fk_col_sym = as.name(fk_col),
                      pk_tbl_sym = as.name(pk_tbl)
                    )
                  )
                }
              }

              result_expr
            }),
            state = list(
              pk_table = r_pk_table,
              pk_column = r_pk_column,
              fk_tables = r_fk_tables,
              fk_column = r_fk_column
            )
          )
        }
      )
    },
    ui = function(id) {
      shiny::tagList(
        shiny::div(
          class = "block-container",
          shiny::div(
            class = "block-form-grid",
            shiny::div(
              class = "block-section",
              shiny::tags$h4("Primary Key"),
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    shiny::NS(id, "pk_table"),
                    label = "Table",
                    choices = if (nzchar(pk_table)) pk_table else character(),
                    selected = pk_table
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    shiny::NS(id, "pk_column"),
                    label = "Column",
                    choices = if (nzchar(pk_column)) pk_column else character(),
                    selected = pk_column
                  )
                )
              )
            ),
            shiny::div(
              class = "block-section",
              shiny::tags$h4("Foreign Keys (optional)"),
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectizeInput(
                    shiny::NS(id, "fk_tables"),
                    label = "Tables",
                    choices = if (length(fk_tables) > 0) fk_tables else character(),
                    selected = fk_tables,
                    multiple = TRUE
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    shiny::NS(id, "fk_column"),
                    label = "Column",
                    choices = if (nzchar(fk_column)) fk_column else character(),
                    selected = fk_column
                  )
                )
              )
            )
          )
        )
      )
    },
    allow_empty_state = c("fk_tables", "fk_column"),
    class = "dm_add_keys_block",
    ...
  )
}

#' @rdname block_output.dm_block
#' @method block_output dm_add_keys_block
#' @export
block_output.dm_add_keys_block <- function(x, result, session) {
  block_output.dm_block(x, result, session)
}

#' @rdname block_ui.dm_block
#' @method block_ui dm_add_keys_block
#' @export
block_ui.dm_add_keys_block <- function(id, x, ...) {
  block_ui.dm_block(id, x, ...)
}
