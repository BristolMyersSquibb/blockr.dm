#' dm Add Keys Block Constructor
#'
#' This block adds primary and foreign key relationships to a dm object.
#' It allows selecting a table and column to set as primary key, and
#' optionally link it as a foreign key from another table.
#'
#' @param pk_table Character, the table to add primary key to. Default "".
#' @param pk_column Character, the column to use as primary key. Default "".
#' @param fk_table Character, the table containing the foreign key. Default "".
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
#' # Add USUBJID as key linking ADSL to ADAE
#' new_dm_add_keys_block(
#'   pk_table = "adsl",
#'   pk_column = "USUBJID",
#'   fk_table = "adae",
#'   fk_column = "USUBJID"
#' )
#'
#' @export
new_dm_add_keys_block <- function(
  pk_table = "",
  pk_column = "",
  fk_table = "",
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
          r_fk_table <- shiny::reactiveVal(fk_table)
          r_fk_column <- shiny::reactiveVal(fk_column)

          # Update reactives from inputs
          shiny::observeEvent(input$pk_table, r_pk_table(input$pk_table))
          shiny::observeEvent(input$pk_column, r_pk_column(input$pk_column))
          shiny::observeEvent(input$fk_table, r_fk_table(input$fk_table))
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

              # Update FK table (allow empty)
              current_fk <- r_fk_table()
              selected_fk <- if (current_fk %in% tables) current_fk else ""
              shiny::updateSelectInput(session, "fk_table", choices = c("", tables), selected = selected_fk)
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

          # Update FK column choices when fk_table changes
          shiny::observeEvent(list(data(), r_fk_table()), {
            dm_obj <- data()
            tbl <- r_fk_table()
            if (inherits(dm_obj, "dm") && nzchar(tbl)) {
              tbl_data <- tryCatch(
                dm::pull_tbl(dm_obj, !!tbl),
                error = function(e) NULL
              )
              if (!is.null(tbl_data)) {
                cols <- colnames(tbl_data)
                current <- r_fk_column()
                selected <- if (current %in% cols) current else cols[1]
                shiny::updateSelectInput(session, "fk_column", choices = c("", cols), selected = selected)
              }
            }
          })

          list(
            expr = shiny::reactive({
              pk_tbl <- r_pk_table()
              pk_col <- r_pk_column()
              fk_tbl <- r_fk_table()
              fk_col <- r_fk_column()

              shiny::req(pk_tbl, nzchar(pk_tbl), pk_col, nzchar(pk_col))

              # Check if PK already exists in the dm object
              dm_obj <- data()
              pk_exists <- FALSE
              if (inherits(dm_obj, "dm")) {
                existing_pks <- tryCatch(
                  dm::dm_get_all_pks(dm_obj),
                  error = function(e) data.frame(table = character())
                )
                pk_exists <- pk_tbl %in% existing_pks$table
              }

              if (nzchar(fk_tbl) && nzchar(fk_col)) {
                if (pk_exists) {
                  # PK already exists, only add FK
                  bquote(
                    dm::dm_add_fk(data, .(fk_tbl_sym), .(fk_col_sym), .(pk_tbl_sym)),
                    list(
                      pk_tbl_sym = as.name(pk_tbl),
                      fk_tbl_sym = as.name(fk_tbl),
                      fk_col_sym = as.name(fk_col)
                    )
                  )
                } else {
                  # Add both PK and FK
                  bquote(
                    dm::dm_add_pk(data, .(pk_tbl_sym), .(pk_col_sym)) |>
                      dm::dm_add_fk(.(fk_tbl_sym), .(fk_col_sym), .(pk_tbl_sym)),
                    list(
                      pk_tbl_sym = as.name(pk_tbl),
                      pk_col_sym = as.name(pk_col),
                      fk_tbl_sym = as.name(fk_tbl),
                      fk_col_sym = as.name(fk_col)
                    )
                  )
                }
              } else {
                if (pk_exists) {
                  # PK exists, nothing to do - return identity
                  quote(data)
                } else {
                  # Add only PK
                  bquote(
                    dm::dm_add_pk(data, .(pk_tbl_sym), .(pk_col_sym)),
                    list(
                      pk_tbl_sym = as.name(pk_tbl),
                      pk_col_sym = as.name(pk_col)
                    )
                  )
                }
              }
            }),
            state = list(
              pk_table = r_pk_table,
              pk_column = r_pk_column,
              fk_table = r_fk_table,
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
              shiny::tags$h4("Foreign Key (optional)"),
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    shiny::NS(id, "fk_table"),
                    label = "Table",
                    choices = if (nzchar(fk_table)) fk_table else character(),
                    selected = fk_table
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
    allow_empty_state = c("fk_table", "fk_column"),
    class = "dm_add_keys_block",
    ...
  )
}

#' @rdname block_output.dm_block
#' @export
block_output.dm_add_keys_block <- function(x, result, session) {
  block_output.dm_block(x, result, session)
}

#' @rdname block_ui.dm_block
#' @export
block_ui.dm_add_keys_block <- function(id, x, ...) {
  block_ui.dm_block(id, x, ...)
}
