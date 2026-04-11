#' Find the CDISC parent table in a dm object
#'
#' Looks for a table named "adsl" or "dm" (case-insensitive). If both exist,
#' ADSL is preferred (ADAM takes precedence).
#'
#' @param dm_obj A dm object
#' @return The name of the parent table, or NULL if not found
#' @keywords internal
find_cdisc_parent <- function(dm_obj) {
  match <- grep("^(adsl|dm)$", names(dm_obj), ignore.case = TRUE, value = TRUE)
  if (length(match) > 0) match[1] else NULL
}

#' Find duplicated columns between parent and child tables
#'
#' For each child table that has USUBJID, finds columns shared with the parent
#' table (excluding USUBJID and STUDYID which are always kept).
#'
#' @param dm_obj A dm object
#' @param parent_name Name of the parent table
#' @return A named list mapping child table names to character vectors of
#'   columns to remove
#' @keywords internal
find_duplicated_cols <- function(dm_obj, parent_name) {
  parent_cols <- names(dm_obj[[parent_name]])
  keep_always <- "USUBJID"

  # Keep STUDYID only if there are multiple studies — otherwise it's
 # safe to deduplicate (single value repeated across all tables).
  if ("STUDYID" %in% parent_cols) {
    n_studies <- length(unique(dm_obj[[parent_name]][["STUDYID"]]))
    if (n_studies > 1L) {
      keep_always <- c(keep_always, "STUDYID")
    }
  }

  result <- list()

  for (tbl_name in setdiff(names(dm_obj), parent_name)) {
    child_cols <- names(dm_obj[[tbl_name]])
    if (!"USUBJID" %in% child_cols) next
    shared <- intersect(child_cols, parent_cols)
    to_remove <- setdiff(shared, keep_always)
    if (length(to_remove) > 0) {
      result[[tbl_name]] <- to_remove
    }
  }

  result
}

#' Create CDISC DM Block
#'
#' Transforms a dm object by detecting the CDISC parent table (ADSL or DM)
#' and setting correct PK/FK relationships on USUBJID, with optional
#' column deduplication.
#'
#' @param set_keys Logical, whether to set USUBJID as PK on the parent table
#'   and FK on child tables. Default is `TRUE`.
#' @param dedup_cols Logical, whether to remove duplicated subject columns
#'   from child tables. Default is `TRUE`.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A block object of class `dm_block`
#'
#' @details
#' The block expects a single dm input containing CDISC tables. It:
#' \enumerate{
#'   \item Strips any existing PK/FK relationships
#'   \item Sets USUBJID as PK on the parent table (ADSL or DM)
#'   \item Sets USUBJID as FK on all child tables that contain it
#'   \item Optionally removes duplicated subject-level columns from child tables
#' }
#'
#' If no parent table (ADSL or DM) is found, a warning is issued and the
#' dm is passed through unchanged.
#'
#' @export
new_cdisc_dm_block <- function(set_keys = TRUE, dedup_cols = TRUE, ...) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        set_keys_rv <- shiny::reactiveVal(set_keys)
        dedup_rv <- shiny::reactiveVal(dedup_cols)

        shiny::observeEvent(
          input$set_keys, set_keys_rv(input$set_keys),
          ignoreInit = TRUE
        )
        shiny::observeEvent(
          input$dedup_cols, dedup_rv(input$dedup_cols),
          ignoreInit = TRUE
        )

        output$cdisc_badge <- shiny::renderUI({
          dm_input <- data()
          shiny::req(inherits(dm_input, "dm"))

          parent_name <- find_cdisc_parent(dm_input)

          if (is.null(parent_name)) {
            return(shiny::tags$span(
              class = "blockr-path-badge blockr-path-badge-error",
              "No CDISC parent"
            ))
          }

          label <- if (grepl("^adsl$", parent_name, ignore.case = TRUE)) {
            "ADAM"
          } else {
            "SDTM"
          }

          all_tables <- names(dm_input)
          n_tables <- length(all_tables)
          total_rows <- sum(vapply(
            all_tables,
            function(t) nrow(dm_input[[t]]),
            integer(1)
          ))

          # Per-table detail
          table_details <- vapply(all_tables, function(t) {
            paste0(t, " (", format(nrow(dm_input[[t]]), big.mark = ","), ")")
          }, character(1))

          shiny::tagList(
            shiny::div(
              style = "display: flex; align-items: center; gap: 8px;",
              shiny::tags$span(
                class = "blockr-path-badge blockr-path-badge-success",
                label
              ),
              shiny::tags$span(
                class = "text-muted",
                style = "font-size: 0.8rem;",
                paste0(
                  n_tables, " table", if (n_tables != 1) "s",
                  " \u00b7 ",
                  format(total_rows, big.mark = ","), " rows"
                )
              )
            ),
            shiny::tags$p(
              class = "text-muted",
              style = "font-size: 0.75rem; margin-top: 4px; margin-bottom: 0;",
              paste(table_details, collapse = " \u00b7 ")
            )
          )
        })

        list(
          expr = shiny::reactive({
            dm_input <- data()
            shiny::req(inherits(dm_input, "dm"))

            parent_name <- find_cdisc_parent(dm_input)

            if (is.null(parent_name)) {
              warning(
                "No CDISC parent table (ADSL or DM) found. ",
                "Passing through unchanged."
              )
              return(quote(identity(data)))
            }

            do_keys <- set_keys_rv()
            do_dedup <- dedup_rv()

            # Find child tables with USUBJID
            child_tables <- character(0)
            for (tbl_name in setdiff(names(dm_input), parent_name)) {
              if ("USUBJID" %in% names(dm_input[[tbl_name]])) {
                child_tables <- c(child_tables, tbl_name)
              }
            }

            # Build expression body as list of calls
            body_exprs <- list()

            # Assign input to result
            body_exprs <- c(body_exprs, list(
              quote(result <- data)
            ))

            # Strip existing FKs (hardcoded at build time)
            existing_fks <- dm::dm_get_all_fks(dm_input)
            if (nrow(existing_fks) > 0) {
              pairs <- unique(existing_fks[, c("child_table", "parent_table")])
              for (i in seq_len(nrow(pairs))) {
                cs <- as.name(pairs$child_table[i])
                ps <- as.name(pairs$parent_table[i])
                body_exprs <- c(body_exprs, list(
                  bquote(result <- dm::dm_rm_fk(
                    result, .(cs), ref_table = .(ps)
                  ))
                ))
              }
            }

            # Strip existing PKs
            existing_pks <- dm::dm_get_all_pks(dm_input)
            if (nrow(existing_pks) > 0) {
              for (i in seq_len(nrow(existing_pks))) {
                ts <- as.name(existing_pks$table[i])
                body_exprs <- c(body_exprs, list(
                  bquote(result <- dm::dm_rm_pk(
                    result, .(ts), fail_fk = FALSE
                  ))
                ))
              }
            }

            # Add PK/FK only when keys checkbox is on
            if (do_keys) {
              parent_sym <- as.name(parent_name)
              body_exprs <- c(body_exprs, list(
                bquote(result <- dm::dm_add_pk(
                  result, .(parent_sym), USUBJID
                ))
              ))

              for (child in child_tables) {
                child_sym <- as.name(child)
                body_exprs <- c(body_exprs, list(
                  bquote(result <- dm::dm_add_fk(
                    result, .(child_sym), USUBJID, .(parent_sym)
                  ))
                ))
              }
            }

            # Dedup columns if enabled
            if (do_dedup) {
              dedup_info <- find_duplicated_cols(dm_input, parent_name)
              for (tbl_name in names(dedup_info)) {
                cols <- dedup_info[[tbl_name]]
                tbl_sym <- as.name(tbl_name)
                remove_args <- lapply(cols, function(col) {
                  bquote(-.(as.name(col)))
                })
                # Pipe operator inside bquote breaks substitution
                body_exprs <- c(body_exprs, list(
                  bquote(result <- dm::dm_zoom_to(result, .(tbl_sym)))
                ))
                sel <- as.call(c(
                  list(quote(dplyr::select), quote(result)),
                  remove_args
                ))
                body_exprs <- c(body_exprs, list(
                  bquote(result <- .(sel))
                ))
                body_exprs <- c(body_exprs, list(
                  quote(result <- dm::dm_update_zoomed(result))
                ))
              }
            }

            body_exprs <- c(body_exprs, list(quote(result)))

            block <- as.call(c(list(quote(`{`)), body_exprs))
            bquote(local(.(block)))
          }),
          state = list(
            set_keys = set_keys_rv,
            dedup_cols = dedup_rv
          )
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        block_responsive_css(),
        shiny::tags$style(shiny::HTML(
          ".blockr-path-badge {
            display: inline-block; padding: 2px 8px;
            font-size: 0.625rem; border-radius: 4px;
            white-space: nowrap; line-height: 1.4;
          }
          .blockr-path-badge-success {
            background-color: #ecfdf5; color: #047857;
            border: 1px solid #a7f3d0;
          }
          .blockr-path-badge-error {
            background-color: #fef2f2; color: #b91c1c;
            border: 1px solid #fca5a5;
          }
          .cdisc-dm-adjustments .block-input-wrapper {
            margin-bottom: 4px;
          }"
        )),
        shiny::div(
          class = "block-container",
          shiny::div(
            class = "block-form-grid",
            shiny::div(
              class = "block-section",
              shiny::tags$h4("Verification"),
              shiny::uiOutput(ns("cdisc_badge"))
            ),
            shiny::div(
              class = "block-section",
              shiny::tags$h4("Data Adjustments"),
              shiny::div(
                class = "block-section-grid cdisc-dm-adjustments",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::checkboxInput(
                    ns("set_keys"),
                    "Set CDISC keys",
                    value = set_keys
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::checkboxInput(
                    ns("dedup_cols"),
                    "Remove duplicated subject columns",
                    value = dedup_cols
                  )
                )
              )
            )
          )
        )
      )
    },
    dat_valid = function(data) {
      if (!inherits(data, "dm")) stop("Input must be a dm object")
    },
    allow_empty_state = TRUE,
    class = c("cdisc_dm_block", "dm_block"),
    ...
  )
}
