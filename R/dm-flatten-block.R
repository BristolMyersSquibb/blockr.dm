#' dm Flatten Block Constructor
#'
#' This block flattens a dm object into a single data frame by joining
#' all related tables based on their foreign key relationships.
#'
#' @param start_table Character, the table to start flattening from. Default "".
#' @param include_tables Character vector, specific tables to include in the join.
#'   If empty, all reachable tables are included.
#' @param join_type Character, type of join to use: "left", "inner", "full", "right".
#'   Default "left".
#' @param recursive Logical, whether to recursively join all related tables.
#'   Default TRUE.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A block object for flattening dm objects
#'
#' @details
#' The starting table determines the base of the join. Related tables are
#' joined based on their foreign key relationships. With `recursive = TRUE`,
#' all transitively related tables are included.
#'
#' Use `include_tables` to limit which tables are joined - useful when you
#' want to flatten only specific relationships (e.g., join ADAE with ADSL
#' but not ADLB).
#'
#' @examples
#' # Create a dm flatten block
#' new_dm_flatten_block(start_table = "flights", recursive = TRUE)
#'
#' # Flatten with specific tables and inner join
#' new_dm_flatten_block(
#'   start_table = "ADSL",
#'   include_tables = c("ADAE"),
#'   join_type = "inner"
#' )
#'
#' @export
new_dm_flatten_block <- function(
    start_table = "",
    include_tables = character(),
    join_type = "left",
    recursive = TRUE,
    ...
) {
  join_type <- match.arg(join_type, c("left", "inner", "full", "right"))

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Reactive values
          r_start_table <- shiny::reactiveVal(start_table)
          r_include_tables <- shiny::reactiveVal(include_tables)
          r_join_type <- shiny::reactiveVal(join_type)
          r_recursive <- shiny::reactiveVal(recursive)

          # Update reactives from inputs
          shiny::observeEvent(input$start_table, {
            r_start_table(input$start_table)
          })

          shiny::observeEvent(input$include_tables, {
            r_include_tables(input$include_tables)
          }, ignoreNULL = FALSE)

          shiny::observeEvent(input$join_type, {
            r_join_type(input$join_type)
          })

          shiny::observeEvent(input$recursive, {
            r_recursive(input$recursive)
          })

          # Update table choices when dm changes
          shiny::observeEvent(data(), {
            dm_obj <- data()
            if (inherits(dm_obj, "dm")) {
              tables <- names(dm::dm_get_tables(dm_obj))
              current_start <- r_start_table()
              current_include <- r_include_tables()

              # Keep current start selection if valid, otherwise use first table
              selected_start <- if (current_start %in% tables) current_start else tables[1]
              shiny::updateSelectInput(
                session, "start_table",
                choices = tables,
                selected = selected_start
              )
              r_start_table(selected_start)

              # Update include tables (exclude start table from choices)
              other_tables <- setdiff(tables, selected_start)
              valid_include <- intersect(current_include, other_tables)
              shiny::updateSelectizeInput(
                session, "include_tables",
                choices = other_tables,
                selected = valid_include
              )
            }
          })

          # Update include choices when start table changes
          shiny::observeEvent(r_start_table(), {
            dm_obj <- data()
            if (inherits(dm_obj, "dm")) {
              tables <- names(dm::dm_get_tables(dm_obj))
              other_tables <- setdiff(tables, r_start_table())
              current_include <- r_include_tables()
              valid_include <- intersect(current_include, other_tables)
              shiny::updateSelectizeInput(
                session, "include_tables",
                choices = other_tables,
                selected = valid_include
              )
            }
          })

          list(
            expr = shiny::reactive({
              tbl <- r_start_table()
              include <- r_include_tables()
              jtype <- r_join_type()
              is_recursive <- r_recursive()

              shiny::req(tbl, nzchar(tbl))

              if (length(include) == 0) {
                # No specific tables selected - use default (all reachable)
                # Build expression based on join type
                if (jtype == "left") {
                  # Default left_join - don't need to specify .join
                  bquote(
                    dm::dm_flatten_to_tbl(data, .(tbl_sym), .recursive = .(is_recursive)),
                    list(
                      tbl_sym = as.name(tbl),
                      is_recursive = is_recursive
                    )
                  )
                } else {
                  join_fn <- switch(jtype,
                    "inner" = quote(dplyr::inner_join),
                    "full" = quote(dplyr::full_join),
                    "right" = quote(dplyr::right_join)
                  )
                  bquote(
                    dm::dm_flatten_to_tbl(data, .(tbl_sym), .join = .(join_fn), .recursive = .(is_recursive)),
                    list(
                      tbl_sym = as.name(tbl),
                      join_fn = join_fn,
                      is_recursive = is_recursive
                    )
                  )
                }
              } else {
                # Specific tables selected - pass them to ...
                include_syms <- lapply(include, as.name)
                join_fn <- switch(jtype,
                  "left" = quote(dplyr::left_join),
                  "inner" = quote(dplyr::inner_join),
                  "full" = quote(dplyr::full_join),
                  "right" = quote(dplyr::right_join)
                )
                # Build call with spliced table symbols
                tbl_sym <- as.name(tbl)
                call_args <- c(
                  list(quote(dm::dm_flatten_to_tbl)),
                  list(quote(data)),
                  list(tbl_sym),
                  include_syms,
                  list(.join = join_fn),
                  list(.recursive = is_recursive)
                )
                as.call(call_args)
              }
            }),
            state = list(
              start_table = r_start_table,
              include_tables = r_include_tables,
              join_type = r_join_type,
              recursive = r_recursive
            )
          )
        }
      )
    },
    ui = function(id) {
      shiny::tagList(
        block_responsive_css(),
        shiny::div(
          class = "block-container",
          shiny::div(
            class = "block-form-grid",
            shiny::div(
              class = "block-section",
              shiny::tags$h4("Flatten dm"),
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    shiny::NS(id, "start_table"),
                    label = "Start from table",
                    choices = if (nzchar(start_table)) start_table else character(),
                    selected = start_table
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectizeInput(
                    shiny::NS(id, "include_tables"),
                    label = "Include tables (optional)",
                    choices = include_tables,
                    selected = include_tables,
                    multiple = TRUE,
                    options = list(
                      placeholder = "All reachable tables",
                      plugins = list("remove_button")
                    )
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    shiny::NS(id, "join_type"),
                    label = "Join type",
                    choices = c(
                      "Left join" = "left",
                      "Inner join" = "inner",
                      "Full join" = "full",
                      "Right join" = "right"
                    ),
                    selected = join_type
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::checkboxInput(
                    shiny::NS(id, "recursive"),
                    label = "Recursive (follow all relationships)",
                    value = recursive
                  )
                )
              ),
              shiny::tags$p(
                class = "text-muted",
                "Joins related tables into a single data frame based on foreign keys."
              )
            )
          )
        )
      )
    },
    allow_empty_state = c("include_tables"),
    class = "dm_flatten_block",
    ...
  )
}
