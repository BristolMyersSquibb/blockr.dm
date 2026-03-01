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
  keep_always <- c("USUBJID", "STUDYID")
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
new_cdisc_dm_block <- function(dedup_cols = TRUE, ...) {
  blockr.core::new_transform_block(
    server = function(id, ...args) {
      shiny::moduleServer(id, function(input, output, session) {
        dedup_rv <- shiny::reactiveVal(dedup_cols)

        shiny::observeEvent(input$dedup_cols, dedup_rv(input$dedup_cols),
          ignoreInit = TRUE)

        list(
          expr = shiny::reactive({
            nms <- names(...args)
            dm_input <- ...args[[nms[1]]]
            if (shiny::is.reactive(dm_input)) dm_input <- dm_input()
            shiny::req(inherits(dm_input, "dm"))

            input_sym <- as.name(nms[1])
            parent_name <- find_cdisc_parent(dm_input)

            if (is.null(parent_name)) {
              warning(
                "No CDISC parent table (ADSL or DM) found. ",
                "Passing through unchanged."
              )
              return(bquote(identity(.(input_sym))))
            }

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
              bquote(result <- .(input_sym))
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

            # Add PK on parent
            parent_sym <- as.name(parent_name)
            body_exprs <- c(body_exprs, list(
              bquote(result <- dm::dm_add_pk(
                result, .(parent_sym), USUBJID
              ))
            ))

            # Add FK for each child
            for (child in child_tables) {
              child_sym <- as.name(child)
              body_exprs <- c(body_exprs, list(
                bquote(result <- dm::dm_add_fk(
                  result, .(child_sym), USUBJID, .(parent_sym)
                ))
              ))
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
                # Avoid |> inside bquote (pipe desugaring interferes with .())
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
            dedup_cols = dedup_rv
          )
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shiny::div(
          class = "block-container",
          shiny::tags$p(
            class = "text-muted mb-2",
            "CDISC data model transform"
          ),
          shiny::checkboxInput(
            ns("keys"),
            "Set CDISC keys",
            value = TRUE
          ) |>
            shiny::tagAppendAttributes(disabled = "disabled"),
          shiny::checkboxInput(
            ns("dedup_cols"),
            "Remove duplicated subject columns",
            value = dedup_cols
          )
        )
      )
    },
    dat_valid = function(...args) {
      if (length(...args) != 1L) stop("Exactly one dm input required")
      arg <- ...args[[1]]
      if (!inherits(arg, "dm")) stop("Input must be a dm object")
    },
    allow_empty_state = TRUE,
    class = c("cdisc_dm_block", "dm_block"),
    ...
  )
}
