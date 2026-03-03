#' Safety DM Block
#'
#' A data block that returns a ready-to-use CDISC safety `dm` object built from
#' the `safetyData` package.
#'
#' The returned dm contains three ADaM tables — `adsl` (subject-level),
#' `adae` (adverse events), and `adlb` (lab chemistry) — with USUBJID primary
#' and foreign keys already configured.
#'
#' @param ... Forwarded to [blockr.core::new_data_block()]
#'
#' @return A data block of class `c("safety_dm_block", "dm_block")`.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dm)
#'   serve(new_safety_dm_block())
#' }
#'
#' @export
new_safety_dm_block <- function(...) {
  blockr.core::new_data_block(
    server = function(id) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = shiny::reactive({
              quote(
                local({
                  adsl <- safetyData::adam_adsl
                  adae <- safetyData::adam_adae
                  adlb <- safetyData::adam_adlbc

                  result <- dm::dm(adsl = adsl, adae = adae, adlb = adlb)
                  result <- dm::dm_add_pk(result, adsl, USUBJID)
                  result <- dm::dm_add_fk(result, adae, USUBJID, adsl)
                  result <- dm::dm_add_fk(result, adlb, USUBJID, adsl)
                  result
                })
              )
            }),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      shiny::tagList()
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
