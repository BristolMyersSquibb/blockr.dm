#' @importFrom blockr.ai data_schema
#' @method data_schema dm
#' @export
data_schema.dm <- function(x, ...) {
  tbl_names <- names(x)

  # Table dimensions
  dims <- vapply(tbl_names, function(nm) {
    tbl <- x[[nm]]
    paste0("  ", nm, ": ", nrow(tbl), " rows x ", ncol(tbl), " cols")
  }, character(1))

  # Primary keys
  pk_text <- tryCatch({
    pks <- dm::dm_get_all_pks(x)
    if (nrow(pks) > 0) {
      lines <- vapply(seq_len(nrow(pks)), function(i) {
        paste0("  ", pks$table[i], ": ", paste(pks$pk_col[[i]], collapse = ", "))
      }, character(1))
      paste0("\nPrimary keys:\n", paste(lines, collapse = "\n"))
    } else ""
  }, error = function(e) "")

  # Foreign keys
  fk_text <- tryCatch({
    fks <- dm::dm_get_all_fks(x)
    if (nrow(fks) > 0) {
      lines <- vapply(seq_len(nrow(fks)), function(i) {
        paste0(
          "  ", fks$child_table[i], ".", paste(fks$child_fk_cols[[i]], collapse = ", "),
          " -> ", fks$parent_table[i], ".", paste(fks$parent_key_cols[[i]], collapse = ", ")
        )
      }, character(1))
      paste0("\nForeign keys:\n", paste(lines, collapse = "\n"))
    } else ""
  }, error = function(e) "")

  # Per-table column names only (concise — LLM can use data_query to drill in)
  col_lists <- vapply(tbl_names, function(nm) {
    paste0("## ", nm, "\n", paste(names(x[[nm]]), collapse = ", "))
  }, character(1))

  paste0(
    "dm object with ", length(tbl_names), " tables:\n",
    paste(dims, collapse = "\n"),
    pk_text, fk_text,
    "\n\nUse data_query to explore specific tables.\n\n",
    paste(col_lists, collapse = "\n\n")
  )
}
