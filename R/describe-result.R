# describe_result is a generic owned by blockr.assistant, a soft dependency
# (Suggests). This method is registered onto it lazily from .onLoad via
# vctrs::s3_register(), so it activates only when the assistant is loaded and
# adds no hard dependency. Its text is fed to the model as a block-result
# summary, capped at ~2000 chars -- hence a compact per-table overview with
# columns deferred to the assistant's query_data tool, not a schema dump.
describe_result.dm <- function(x, ...) {

  tbls <- dm::dm_get_tables(x)
  tbl_names <- names(tbls)
  eg <- tbl_names[1L]

  header <- paste0(
    "This result is a dm -- a set of ", length(tbl_names), " related tables, ",
    "not a single data frame. A table becomes a data frame only when extracted ",
    "with a dm_pull_block(table = \"<name>\"); inspect a table's columns with ",
    "query_data, e.g. names(<id>$", eg, ")."
  )

  table_lines <- vapply(
    tbl_names,
    function(nm) {
      tbl <- tbls[[nm]]
      paste0("- ", nm, ": ", nrow(tbl), " rows x ", ncol(tbl), " cols")
    },
    character(1),
    USE.NAMES = FALSE
  )

  c(header, "", table_lines)
}
