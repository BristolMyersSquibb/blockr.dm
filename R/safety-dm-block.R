#' Safety DM Block
#'
#' A data block that returns a ready-to-use CDISC safety `dm` object built from
#' the `safetyData` package.
#'
#' The returned dm contains selectable ADaM tables with USUBJID primary
#' and foreign keys already configured. The `adsl` table is always included
#' as it serves as the primary-key table for all foreign-key relationships.
#'
#' @param tables Character vector of table names to include. Defaults to
#'   `c("adsl", "adtte")`. Available tables: adsl, adae, adlbc, adlbh,
#'   adlbhy, adqsadas, adqscibc, adqsnpix, adtte, advs.
#' @param ... Forwarded to [blockr.core::new_data_block()]
#'
#' @return A data block of class `c("safety_dm_block", "dm_block")`.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dm)
#'   serve(new_safety_dm_block())
#'   serve(new_safety_dm_block(tables = c("adsl", "adae", "adlbc")))
#' }
#'
#' @export
new_safety_dm_block <- function(tables = c("adsl", "adtte"), ...) {
  # Named map of table name -> safetyData dataset
  all_safety_tables <- c(
    adsl     = "safetyData::adam_adsl",
    adae     = "safetyData::adam_adae",
    adlbc    = "safetyData::adam_adlbc",
    adlbh    = "safetyData::adam_adlbh",
    adlbhy   = "safetyData::adam_adlbhy",
    adqsadas = "safetyData::adam_adqsadas",
    adqscibc = "safetyData::adam_adqscibc",
    adqsnpix = "safetyData::adam_adqsnpix",
    adtte    = "safetyData::adam_adtte",
    advs     = "safetyData::adam_advs"
  )

  all_choices <- names(all_safety_tables)

  # Validate initial selection
  tables <- intersect(tables, all_choices)
  # Always include adsl
  if (!"adsl" %in% tables) {
    tables <- c("adsl", tables)
  }

  blockr.core::new_data_block(
    server = function(id) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_tables <- shiny::reactiveVal(tables)

          shiny::observeEvent(input$tables, {
            selected <- input$tables
            # Always keep adsl
            if (!"adsl" %in% selected) {
              selected <- c("adsl", selected)
              shiny::updateSelectizeInput(
                session, "tables",
                selected = selected
              )
            }
            r_tables(selected)
          }, ignoreNULL = TRUE, ignoreInit = TRUE)

          list(
            expr = shiny::reactive({
              selected <- r_tables()
              if (length(selected) == 0) selected <- "adsl"
              if (!"adsl" %in% selected) selected <- c("adsl", selected)

              # Build expression: local({ adsl <- ...; ... dm::dm(...) ... })
              load_exprs <- lapply(selected, function(tbl) {
                call("<-", as.name(tbl), parse(text = all_safety_tables[[tbl]])[[1]])
              })

              # dm::dm(adsl = adsl, adae = adae, ...)
              dm_args <- stats::setNames(
                lapply(selected, as.name),
                selected
              )
              dm_call <- as.call(c(list(quote(dm::dm)), dm_args))
              assign_dm <- call("<-", quote(result), dm_call)

              # PK on adsl
              pk_call <- call("<-", quote(result),
                quote(dm::dm_add_pk(result, adsl, USUBJID))
              )

              # FK on each non-adsl table
              fk_exprs <- lapply(setdiff(selected, "adsl"), function(tbl) {
                fk_call <- substitute(
                  dm::dm_add_fk(result, TBL, USUBJID, adsl),
                  list(TBL = as.name(tbl))
                )
                call("<-", quote(result), fk_call)
              })

              body_exprs <- c(load_exprs, list(assign_dm, pk_call), fk_exprs, list(quote(result)))
              body_block <- as.call(c(list(as.name("{")), body_exprs))
              call("local", body_block)
            }),
            state = list(
              tables = r_tables
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::selectizeInput(
          ns("tables"),
          label = "ADaM tables",
          choices = all_choices,
          selected = tables,
          multiple = TRUE,
          options = list(
            placeholder = "Select tables...",
            plugins = list("remove_button")
          )
        )
      )
    },
    allow_empty_state = TRUE,
    class = c("safety_dm_block", "dm_block"),
    ...
  )
}

#' @method block_output safety_dm_block
#' @export
block_output.safety_dm_block <- function(x, result, session) {
  block_output.dm_block(x, result, session)
}

#' @method block_ui safety_dm_block
#' @export
block_ui.safety_dm_block <- function(id, x, ...) {
  block_ui.dm_block(id, x, ...)
}

#' @method block_render_trigger safety_dm_block
#' @export
block_render_trigger.safety_dm_block <- function(x, session = blockr.core::get_session()) {
  NULL
}
