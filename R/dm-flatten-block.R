#' dm Flatten Block Constructor
#'
#' This block flattens a dm object into a single data frame by joining
#' all related tables based on their foreign key relationships.
#'
#' @param start_table Character, the table to start flattening from. Default "".
#' @param include_tables Character vector, specific tables to include
#'   in the join. If empty, all reachable tables are included.
#' @param join_type Character, type of join to use: "left", "inner",
#'   "full", "right". Default "left".
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
#' new_dm_flatten_block(start_table = "flights", recursive = TRUE)
#'
#' new_dm_flatten_block(
#'   start_table = "ADSL",
#'   include_tables = c("ADAE"),
#'   join_type = "inner"
#' )
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList req isolate
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
          ns <- session$ns
          r_start_table <- reactiveVal(start_table)
          r_include_tables <- reactiveVal(include_tables)
          r_join_type <- reactiveVal(join_type)
          r_recursive <- reactiveVal(recursive)

          observeEvent(input$start_table, {
            val <- input$start_table
            if (!is.null(val) && nzchar(val)) r_start_table(val)
          })

          observeEvent(input$include_tables, {
            # multi-picker: NULL means no tables chosen, keep existing
            val <- input$include_tables
            if (is.null(val)) return()
            r_include_tables(val)
          })

          observeEvent(input$join_type, {
            r_join_type(input$join_type)
          })

          observeEvent(input$recursive, {
            r_recursive(input$recursive)
          })

          # Send start-table picker options on data change
          observeEvent(data(), {
            opts <- build_dm_table_options(data())
            tbl_names <- vapply(opts, `[[`, character(1), "value")
            current_start <- isolate(r_start_table())
            selected_start <- if (current_start %in% tbl_names) {
              current_start
            } else if (length(tbl_names) > 0L) {
              tbl_names[[1L]]
            } else {
              ""
            }
            session$sendCustomMessage(
              "dm-table-picker",
              list(
                id = ns("start_table"),
                mode = "single",
                options = opts,
                selected = selected_start
              )
            )
            r_start_table(selected_start)
          })

          # Include-tables picker: options depend on the currently selected
          # start table (exclude it from choices).
          observeEvent(list(r_start_table(), data()), {
            dm_obj <- data()
            if (!inherits(dm_obj, "dm")) return()
            opts <- build_dm_table_options(dm_obj)
            other_opts <- Filter(function(o) o$value != r_start_table(), opts)
            current_include <- isolate(r_include_tables())
            other_names <- vapply(other_opts, `[[`, character(1), "value")
            valid_include <- intersect(current_include, other_names)
            session$sendCustomMessage(
              "dm-table-picker",
              list(
                id = ns("include_tables"),
                mode = "multi",
                options = other_opts,
                selected = valid_include,
                placeholder = "All reachable tables"
              )
            )
            r_include_tables(valid_include)
          })

          list(
            expr = reactive({
              tbl <- r_start_table()
              include <- r_include_tables()
              jtype <- r_join_type()
              is_recursive <- r_recursive()

              req(tbl, nzchar(tbl))

              if (length(include) == 0) {
                if (jtype == "left") {
                  bquote(
                    dm::dm_flatten_to_tbl(
                      data, .(tbl_sym),
                      .recursive = .(is_recursive)
                    ),
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
                    dm::dm_flatten_to_tbl(
                      data, .(tbl_sym),
                      .join = .(join_fn),
                      .recursive = .(is_recursive)
                    ),
                    list(
                      tbl_sym = as.name(tbl),
                      join_fn = join_fn,
                      is_recursive = is_recursive
                    )
                  )
                }
              } else {
                include_syms <- lapply(include, as.name)
                join_fn <- switch(jtype,
                  "left" = quote(dplyr::left_join),
                  "inner" = quote(dplyr::inner_join),
                  "full" = quote(dplyr::full_join),
                  "right" = quote(dplyr::right_join)
                )
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
      tagList(
        dm_table_picker_deps(),
        block_responsive_css(),
        div(
          class = "block-container",
          div(
            class = "block-form-grid",
            div(
              class = "block-section",
              shiny::tags$h4("Flatten dm"),
              div(
                class = "block-section-grid",
                div(
                  class = "block-input-wrapper",
                  shiny::tags$label(
                    class = "control-label",
                    "Start from table"
                  ),
                  div(
                    id = NS(id, "start_table"),
                    class = "dm-flatten-start-picker"
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  shiny::tags$label(
                    class = "control-label",
                    "Include tables (optional)"
                  ),
                  div(
                    id = NS(id, "include_tables"),
                    class = "dm-flatten-include-picker"
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    NS(id, "join_type"),
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
                div(
                  class = "block-input-wrapper",
                  shiny::checkboxInput(
                    NS(id, "recursive"),
                    label = "Recursive (follow all relationships)",
                    value = recursive
                  )
                )
              ),
              shiny::tags$p(
                class = "text-muted",
                "Joins related tables into a single data",
                "frame based on foreign keys."
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
