#' Build table picker options with labels from a dm object
#'
#' Returns a list of `{value, label}` entries suitable for
#' `Blockr.Select` via the `dm-table-picker` custom message. The label is
#' read from `attr(tbl, "label")` when present (common on ADaM and other
#' annotated datasets). Unlabelled tables get `label = ""` and render as
#' value-only.
#'
#' @param dm_obj A `dm` object.
#' @return A list of lists, each with `value` and `label` string fields.
#' @keywords internal
build_dm_table_options <- function(dm_obj) {
  if (!inherits(dm_obj, "dm")) return(list())
  tbls <- dm::dm_get_tables(dm_obj)
  lapply(names(tbls), function(nm) {
    lbl <- attr(tbls[[nm]], "label")
    list(
      value = nm,
      label = if (is.character(lbl) && length(lbl) == 1L && !is.na(lbl)) lbl else ""
    )
  })
}

#' Push a table selection into a dm table picker
#'
#' The picker is a JS widget, so R has to tell it what to show: its options
#' come from the upstream dm and its selection from the block's state. Every
#' write to that state goes through here, whoever made it -- the user, a board
#' restore, or an external controller -- so what the block reads and what the
#' block shows cannot drift apart.
#'
#' `selected` is wrapped in `as.list()` for a multi picker: a length-one
#' character vector would otherwise reach JS as a bare string rather than an
#' array of one (jsonlite's `auto_unbox`), and the widget would show nothing.
#'
#' @param session Shiny session.
#' @param id Input id inside the module namespace.
#' @param dm_obj Upstream dm, for the option list.
#' @param selected Table name(s) to select.
#' @param mode `"single"` or `"multi"`.
#'
#' @noRd
dm_picker_push <- function(session, id, dm_obj, selected,
                           mode = c("single", "multi")) {

  opts <- build_dm_table_options(dm_obj)

  if (!length(opts)) {
    return(invisible(NULL))
  }

  dm_picker_mount(session, id, opts, selected, match.arg(mode))
}

#' Mount a table picker from an option list
#'
#' The lower half of [dm_picker_push()], for blocks whose options do not come
#' from an upstream dm -- the read block builds them from files on disk.
#'
#' The message is what puts the widget on screen: the handler mounts it on
#' first receipt and reconciles options and selection on every later one, and
#' it queues by element id, so a block whose dock panel has not mounted yet
#' gets the same widget when it does. Nothing in a block should push at an
#' element it hopes is already there.
#'
#' @param session Shiny session.
#' @param id Input id inside the module namespace.
#' @param options List of `{value, label}` entries.
#' @param selected Value(s) to select.
#' @param mode `"single"` or `"multi"`.
#' @param placeholder Placeholder text for an empty multi picker.
#'
#' @noRd
dm_picker_mount <- function(session, id, options, selected,
                            mode = c("single", "multi"),
                            placeholder = "Select tables\u2026") {

  mode <- match.arg(mode)

  msg <- list(
    id = session$ns(id),
    mode = mode,
    options = options,
    # A length-one character vector would otherwise reach JS as a bare
    # string rather than an array of one (jsonlite's `auto_unbox`), and the
    # widget would show nothing.
    selected = if (identical(mode, "multi")) as.list(selected) else selected
  )

  if (identical(mode, "multi")) {
    msg$placeholder <- placeholder
  }

  session$sendCustomMessage("dm-table-picker", msg)

  invisible(NULL)
}

#' Build column summary for one table inside a dm object
#'
#' Thin wrapper that extracts a named table from a dm, coerces to a plain
#' data frame (preserving column-level `label` attributes), and delegates to
#' [blockr.dplyr::build_column_summary()].
#'
#' @param dm_obj A `dm` object.
#' @param table Name of a table in `dm_obj`.
#' @return A list of column summary objects as returned by
#'   `blockr.dplyr::build_column_summary()`.
#' @keywords internal
build_dm_column_summary <- function(dm_obj, table) {
  if (!inherits(dm_obj, "dm")) return(list())
  if (!nzchar(table)) return(list())
  tbls <- dm::dm_get_tables(dm_obj)
  if (!table %in% names(tbls)) return(list())
  df <- tryCatch(as.data.frame(tbls[[table]]), error = function(e) NULL)
  if (is.null(df)) return(list())
  blockr.dplyr::build_column_summary(df)
}
