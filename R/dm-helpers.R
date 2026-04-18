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
