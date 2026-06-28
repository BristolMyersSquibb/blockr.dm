# =============================================================================
# CANONICAL (and ONLY) `data_schema` method for the `dm` type.
# -----------------------------------------------------------------------------
# `data_schema` is a generic owned by blockr.ai. The method for `dm` lives HERE,
# in the package that owns the dm integration, and is registered onto blockr.ai's
# generic via S3method() (see @method/@export below). It is the ONE describing a
# dm input to the LLM assistant — its text becomes the "# Input Data" section of
# the assistant prompt, so it MUST tell the model how to extract a table inside a
# block's `fn` (a dm is not a data frame; `data$<tbl>` etc.).
#
# DO NOT add a second `data_schema.dm` anywhere else (it previously also lived in
# blockr.ai, commit #85 — two registrations of the same generic+class, winner
# decided by namespace LOAD ORDER, so the blockr.ai copy silently shadowed this
# one in some configs and edits went nowhere). One generic, one method, one home.
# =============================================================================

#' @importFrom blockr.ai data_schema
#' @method data_schema dm
#' @export
data_schema.dm <- function(x, ...) {
  tbl_names <- names(x)
  eg <- tbl_names[1L]

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
        paste0(
          "  ", pks$table[i], ": ",
          paste(pks$pk_col[[i]], collapse = ", ")
        )
      }, character(1))
      paste0(
        "\nPrimary keys:\n",
        paste(lines, collapse = "\n")
      )
    } else {
      ""
    }
  }, error = function(e) "")

  # Foreign keys
  fk_text <- tryCatch({
    fks <- dm::dm_get_all_fks(x)
    if (nrow(fks) > 0) {
      lines <- vapply(seq_len(nrow(fks)), function(i) {
        child_fk <- paste(
          fks$child_fk_cols[[i]], collapse = ", "
        )
        parent_pk <- paste(
          fks$parent_key_cols[[i]], collapse = ", "
        )
        paste0(
          "  ", fks$child_table[i], ".", child_fk,
          " -> ", fks$parent_table[i], ".", parent_pk
        )
      }, character(1))
      paste0(
        "\nForeign keys:\n",
        paste(lines, collapse = "\n")
      )
    } else {
      ""
    }
  }, error = function(e) "")

  # Per-table column names only (concise — LLM can use data_query to drill in)
  col_lists <- vapply(tbl_names, function(nm) {
    paste0("## ", nm, "\n", paste(names(x[[nm]]), collapse = ", "))
  }, character(1))

  paste0(
    "The input `data` is a dm: a set of ", length(tbl_names), " related tables, ",
    "NOT a single data frame. In the block's code you MUST extract a table from ",
    "`data` before using its columns -- `data[[\"", eg, "\"]]` (equivalently ",
    "`data$", eg, "`, or `dm::pull_tbl(data, ", eg, ")`). Do NOT call dplyr verbs ",
    "on `data` directly (it errors with 'unzoomed dm'), and do NOT reference a ",
    "table's column on `data` (`data$SOMECOL` is NULL -- use `data[[\"", eg,
    "\"]]$SOMECOL`).\n\nTables:\n",
    paste(dims, collapse = "\n"),
    pk_text, fk_text,
    "\n\n", paste(col_lists, collapse = "\n\n")
  )
}
