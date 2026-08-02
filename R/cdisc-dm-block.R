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

#' Turn a key's column names into a tidyselect expression
#'
#' A single column becomes a symbol, multiple columns a `c(a, b)` call.
#'
#' @param cols Character vector of column names
#' @return A symbol or call suitable for splicing into a `dm` key expression
#' @keywords internal
key_cols_expr <- function(cols) {
  cols <- unlist(cols)
  if (length(cols) == 1L) {
    as.name(cols)
  } else {
    as.call(c(quote(c), lapply(cols, as.name)))
  }
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

#' Everything the CDISC expression needs to know about the input dm
#'
#' Reduces a `dm` to the plain, comparable description the expression builder
#' reads from it: the parent table, the child tables carrying USUBJID, the
#' existing keys to strip, and the duplicated columns to drop. Deliberately
#' returns base vectors and lists (not the `dm_get_all_*()` tibbles) so two
#' equal-but-freshly-built inputs produce an `identical()` result — that is
#' what lets the `reactiveVal` holding it treat a spurious re-derivation as a
#' no-op. See the note on the observer in [new_cdisc_dm_block()].
#'
#' @param dm_obj A dm object
#' @return A list with `parent` (`NULL` when no CDISC parent was found),
#'   `child_tables`, `fks`, `pk_tables` and `dedup_info`
#' @keywords internal
cdisc_dm_shape <- function(dm_obj) {
  parent_name <- find_cdisc_parent(dm_obj)

  if (is.null(parent_name)) {
    return(list(parent = NULL))
  }

  # Child tables with USUBJID
  child_tables <- character(0)
  for (tbl_name in setdiff(names(dm_obj), parent_name)) {
    if ("USUBJID" %in% names(dm_obj[[tbl_name]])) {
      child_tables <- c(child_tables, tbl_name)
    }
  }

  # Existing FKs. Each key names its columns and ref_columns: an
  # underspecified dm_rm_fk() makes dm emit a disambiguation message.
  existing_fks <- dm::dm_get_all_fks(dm_obj)
  fks <- lapply(seq_len(nrow(existing_fks)), function(i) {
    list(
      child_table  = as.character(existing_fks$child_table[i]),
      parent_table = as.character(existing_fks$parent_table[i]),
      child_cols   = unlist(existing_fks$child_fk_cols[[i]]),
      parent_cols  = unlist(existing_fks$parent_key_cols[[i]])
    )
  })

  existing_pks <- dm::dm_get_all_pks(dm_obj)

  list(
    parent       = parent_name,
    child_tables = child_tables,
    fks          = fks,
    pk_tables    = as.character(existing_pks$table),
    dedup_info   = find_duplicated_cols(dm_obj, parent_name)
  )
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
#'   from child tables. Default is `FALSE`: dropping columns is a decision
#'   about someone's data, and a block that reshapes what it was handed
#'   before anyone asks makes the tables downstream disagree with the tables
#'   on disk. Opt in when the collisions in a downstream join are the bigger
#'   problem.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A block object of class `dm_block`
#'
#' @section External control:
#' `set_keys` and `dedup_cols` are externally controllable (see
#' [blockr.core::external_ctrl_vars()]): a board update or an assistant can
#' flip either with a `mod` delta, and the checkboxes follow.
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
new_cdisc_dm_block <- function(set_keys = TRUE, dedup_cols = FALSE, ...) {

  # Read inside the server closure, i.e. after this call returns: left as
  # promises they carry the caller's environment, and any harness that revives
  # the block in a fresh R process forces them there, where the caller's
  # locals are gone.
  force(set_keys)
  force(dedup_cols)

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        set_keys_rv <- shiny::reactiveVal(set_keys)
        dedup_rv <- shiny::reactiveVal(dedup_cols)

        # Set by the input observers, so the mirrors below can tell the
        # user's own click from an external write and skip the echo.
        self_write <- new.env(parent = emptyenv())
        self_write$set_keys <- FALSE
        self_write$dedup <- FALSE

        shiny::observeEvent(
          input$set_keys,
          {
            if (identical(input$set_keys, set_keys_rv())) {
              return()
            }
            self_write$set_keys <- TRUE
            set_keys_rv(input$set_keys)
          },
          ignoreInit = TRUE
        )
        shiny::observeEvent(
          input$dedup_cols,
          {
            if (identical(input$dedup_cols, dedup_rv())) {
              return()
            }
            self_write$dedup <- TRUE
            dedup_rv(input$dedup_cols)
          },
          ignoreInit = TRUE
        )

        # R -> JS: an externally set switch has to show on the switch.
        shiny::observeEvent(set_keys_rv(), {
          if (self_write$set_keys) {
            self_write$set_keys <- FALSE
            return()
          }
          shiny::updateCheckboxInput(
            session, "set_keys", value = isTRUE(set_keys_rv())
          )
        }, ignoreInit = TRUE)

        shiny::observeEvent(dedup_rv(), {
          if (self_write$dedup) {
            self_write$dedup <- FALSE
            return()
          }
          shiny::updateCheckboxInput(
            session, "dedup_cols", value = isTRUE(dedup_rv())
          )
        }, ignoreInit = TRUE)

        # Everything `expr` needs from the upstream `dm` is derived HERE, in an
        # observer keyed on `data()`, and parked in a reactiveVal. `expr` reads
        # that reactiveVal and never touches `data()` itself.
        #
        # Why: blockr.core skips re-evaluating a block only when the expression
        # it is handed is the SAME OBJECT as last time (`same_ref()`, see
        # blockr.core/R/block-server.R). `expr` builds its call tree with
        # `bquote()`, which allocates afresh on every read, so an `expr` that
        # depended on `data()` re-ran -- and re-evaluated this block plus
        # everything downstream of it -- on every spurious upstream
        # invalidation (blockr.dock's view switches churn the chain with
        # byte-for-byte identical data). Reading a reactiveVal that only
        # changes when the DECISION changes, `expr` is not invalidated by that
        # churn at all and hands back its cached object. This is the same
        # observer discipline the value filter block uses for
        # `enforce_single_rule()`. See blockr.cdex/dev/profiling-plan.md,
        # "Settled" item 8.
        #
        # The invalid branch MUST write an explicit "not ready" marker rather
        # than skip the write: silently leaving the last good decision in place
        # would make `expr` serve a STALE expression exactly where it has to
        # propagate `req()`'s silent stop, which blockr.core reads as "this
        # block is waiting" (not "this block has output").
        dm_shape <- shiny::reactiveVal(NULL)

        shiny::observe({
          dm_shape(
            tryCatch(
              {
                dm_input <- data()
                if (inherits(dm_input, "dm")) {
                  list(ok = TRUE, shape = cdisc_dm_shape(dm_input))
                } else {
                  # Mirrors the former `req(inherits(dm_input, "dm"))`.
                  list(ok = FALSE, cond = NULL)
                }
              },
              # An upstream `req()` / error must keep reaching blockr.core; it
              # is captured here (an erroring observer would kill the session)
              # and re-raised from `expr` below, unchanged.
              error = function(e) list(ok = FALSE, cond = e)
            )
          )
        })

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
          # Reads `dm_shape()` (see the observer above) and the two state
          # reactiveVals -- never `data()`. That is what keeps the expression
          # object stable across spurious upstream invalidations, so
          # blockr.core's `same_ref()` skip check succeeds.
          expr = shiny::reactive({
            derived <- dm_shape()
            shiny::req(derived)
            if (!isTRUE(derived$ok)) {
              # Not a dm (silent stop, as the former `req()` did), or the
              # upstream itself failed (re-raise its condition verbatim).
              if (is.null(derived$cond)) shiny::req(FALSE)
              stop(derived$cond)
            }
            shape <- derived$shape

            parent_name <- shape$parent

            if (is.null(parent_name)) {
              warning(
                "No CDISC parent table (ADSL or DM) found. ",
                "Passing through unchanged."
              )
              return(quote(identity(data)))
            }

            do_keys <- set_keys_rv()
            do_dedup <- dedup_rv()

            # Child tables with USUBJID (derived in the observer)
            child_tables <- shape$child_tables

            # Build expression body as list of calls
            body_exprs <- list()

            # Assign input to result
            body_exprs <- c(body_exprs, list(
              quote(result <- data)
            ))

            # Strip existing FKs (hardcoded at build time). Each key names its
            # columns and ref_columns: an underspecified dm_rm_fk() makes dm
            # emit a disambiguation message.
            for (fk in shape$fks) {
              cs <- as.name(fk$child_table)
              ps <- as.name(fk$parent_table)
              cc <- key_cols_expr(fk$child_cols)
              pc <- key_cols_expr(fk$parent_cols)
              body_exprs <- c(body_exprs, list(
                bquote(result <- dm::dm_rm_fk(
                  result, .(cs), columns = .(cc),
                  ref_table = .(ps), ref_columns = .(pc)
                ))
              ))
            }

            # Strip existing PKs
            for (pk_table in shape$pk_tables) {
              ts <- as.name(pk_table)
              body_exprs <- c(body_exprs, list(
                bquote(result <- dm::dm_rm_pk(
                  result, .(ts)
                ))
              ))
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
              dedup_info <- shape$dedup_info
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
    external_ctrl = TRUE,
    ...
  )
}
