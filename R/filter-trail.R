#' Filter trail
#'
#' The record of which filters produced a table, carried on the data itself as
#' a `blockr_filters` attribute so that any downstream consumer (a chart
#' caption, a table footnote, a download) can print what was applied without
#' knowing anything about the board.
#'
#' The trail is a **named character vector, keyed by block**. A filter block
#' writes its clause under its own key, so two branches meeting at a fan-in
#' collapse to one entry per block, and a consumer can map a clause back to
#' the block that set it.
#'
#' @section Where it lives:
#' On a data frame the attribute sits on the frame. On a `dm` it sits on the
#' **tables**, not on the dm object: every dm verb (`dm::dm_filter()`, zooming,
#' key edits) rebuilds the dm and drops attributes set on it, but keeps the
#' attributes of its tables, and `dm::pull_tbl()` and `dm::dm_flatten_to_tbl()`
#' hand a table's attributes on to the frame they return (a flatten keeps the
#' start table's). So a trail written onto the tables reaches every frame cut
#' from the dm with no further code, the way dplyr keeps a data frame's
#' attributes through `filter()`, `mutate()` and joins.
#'
#' Which tables a clause goes on says which tables the filter narrowed.
#' `dm::dm_filter()` cascades over the keys, so a filter written against `orders`
#' narrows `customers` and through it every other table: by default the clause goes
#' on every table. A filter that narrows one table without cascading passes
#' that table as `tables`, and a frame pulled from another table does not
#' carry the clause.
#'
#' `dplyr::summarise()` and `tidyr::pivot_wider()` do not keep attributes, so a
#' block that aggregates has to copy the trail across itself.
#'
#' @param x A data frame or a `dm`.
#' @param key Name the clause is stored under, unique to the block that writes
#'   it.
#' @param clause What the block filtered, e.g. `"segment = Consumer; city = Boston"`. An
#'   empty or missing clause adds nothing, so a filter with nothing selected
#'   leaves no trace.
#' @param tables For a `dm`: the tables to write the clause on
#'   (`add_filter_trail()`) or read the trail from (`filter_trail()`). `NULL`,
#'   the default, means every table. Ignored for a data frame.
#'
#' @return `filter_trail()` returns a named character vector, or `NULL` when
#'   nothing is recorded. On a `dm` it is the union of the tables' trails in
#'   the order the clauses were added. `add_filter_trail()` returns `x` with
#'   the clause added.
#'
#' @examples
#' d <- data.frame(x = 1:3)
#' d <- add_filter_trail(d, "segment", "segment = Consumer")
#' filter_trail(d)
#'
#' @export
filter_trail <- function(x, tables = NULL) {

  trail <- if (inherits(x, "dm")) {
    tbls <- dm::dm_get_tables(x)
    if (!is.null(tables)) {
      tbls <- tbls[intersect(names(tbls), tables)]
    }
    merge_trails(lapply(tbls, trail_attr))
  } else {
    trail_attr(x)
  }

  if (!length(trail)) {
    return(NULL)
  }

  trail
}

#' @rdname filter_trail
#' @export
add_filter_trail <- function(x, key, clause, tables = NULL) {

  if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key) ||
        !length(clause) || is.na(clause[[1L]]) || !nzchar(clause[[1L]])) {
    return(x)
  }

  clause <- as.character(clause[[1L]])

  stamp <- function(t) {
    trail <- trail_attr(t)
    trail[[key]] <- clause
    attr(t, "blockr_filters") <- trail
    t
  }

  if (!inherits(x, "dm")) {
    return(stamp(x))
  }

  tbls <- dm::dm_get_tables(x)
  nms <- if (is.null(tables)) names(tbls) else intersect(tables, names(tbls))

  if (!length(nms)) {
    return(x)
  }

  do.call(dm::dm_mutate_tbl, c(list(x), lapply(tbls[nms], stamp)))
}

# The trail on one object, as a named character vector (empty if none).
# Started from `character()`, not `NULL`: `NULL[["key"]] <- value` builds a
# list, and consumers paste the trail into a caption.
trail_attr <- function(x) {
  trail <- attr(x, "blockr_filters", exact = TRUE)
  if (!is.character(trail) || is.null(names(trail))) {
    return(character())
  }
  trail
}

# Several tables' trails as one. Each table's trail is in the order its
# clauses were added, but a table may lack some (a clause written on
# `orders` alone). The longest goes first and every other key is inserted after the
# last key that precedes it in its own table, so the result keeps the order
# the filters ran in.
merge_trails <- function(trails) {

  trails <- Filter(length, trails)
  trails <- trails[order(-lengths(trails))]
  out <- character()

  for (trail in trails) {
    keys <- names(trail)
    for (i in seq_along(trail)) {
      if (keys[[i]] %in% names(out)) {
        next
      }
      pos <- match(keys[seq_len(i - 1L)], names(out))
      after <- if (any(!is.na(pos))) max(pos, na.rm = TRUE) else 0L
      out <- append(out, trail[i], after = after)
    }
  }

  out
}

# The key a block writes its clause under: its module namespace, which
# carries the board's block id and is unique per block per session.
#
# NOT the `id` a block server receives. `expr_server.block()` calls every
# block's server with `id = "expr"` (blockr.core/R/block-server.R), so using
# it would file every block's clause under one key and each filter would
# overwrite the one before it.
trail_key <- function(session = shiny::getDefaultReactiveDomain()) {
  ns <- tryCatch(session$ns(""), error = function(e) NULL)
  if (is.null(ns) || !length(ns) || !nzchar(ns)) "block" else ns
}

# Wrap a block's expression so it writes its clause. With no clause the
# expression is returned as it is: the trail upstream rides through on the
# tables. Self-qualified, because block expressions are deparsed into the
# exported script.
trail_expr <- function(inner, key, clause, tables = NULL) {

  if (is.null(key) || !length(clause) || is.na(clause[[1L]]) ||
        !nzchar(clause[[1L]])) {
    return(inner)
  }

  args <- list(quote(blockr.dm::add_filter_trail), inner, key, clause[[1L]])

  if (length(tables)) {
    args <- c(args, list(tables = as.character(tables)))
  }

  as.call(args)
}

# Where the data came from: a named list a reader sets as a
# `blockr_provenance` attribute on a table of its dm. `note` is the line the
# crossfilter prints under its cards; blockr.viz reads `dataset` for
# download file names. The first table that has one wins.
provenance <- function(x) {
  cands <- if (inherits(x, "dm")) {
    lapply(dm::dm_get_tables(x), attr, "blockr_provenance", exact = TRUE)
  } else {
    list(attr(x, "blockr_provenance", exact = TRUE))
  }
  for (prov in cands) {
    if (is.list(prov) && length(prov)) {
      return(prov)
    }
  }
  NULL
}

# How many values a categorical clause spells out before eliding. Keeps a
# filter on a high-cardinality column from turning a caption into a list.
TRAIL_MAX_VALUES <- 6L

trail_values <- function(x) {
  vals <- as.character(unlist(x, use.names = FALSE))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (!length(vals)) {
    return(NULL)
  }
  if (length(vals) > TRAIL_MAX_VALUES) {
    # Horizontal ellipsis \u-escaped rather than literal: R sources stay ASCII
    # (R CMD check "non-ASCII characters").
    vals <- c(vals[seq_len(TRAIL_MAX_VALUES)], "\u2026")
  }
  paste(vals, collapse = ", ")
}

# Render a value filter's columns as one clause, the twin of
# crossfilter_clause() below. A dm-backed filter names the table only when
# more than one is filtered, for the same reason; the NA / empty sentinels
# the widget stores read as words. NULL when nothing is selected, so an
# unconfigured filter leaves no trace.
value_filter_clause <- function(columns, operator = "&") {

  cols <- Filter(
    function(e) length(e$values) && nzchar(e$name %||% ""),
    columns %||% list()
  )

  if (!length(cols)) {
    return(NULL)
  }

  tables <- vapply(cols, function(e) e$table %||% "", character(1L))
  qualify <- length(unique(tables[nzchar(tables)])) > 1L
  parts <- character()

  for (i in seq_along(cols)) {

    e <- cols[[i]]

    # A drill claim on subject ids carries the clause the user clicked
    # (blockr.viz `dd_ctrl_claims(subject =)`): "category_name = Books" reads,
    # sixty-nine ids do not.
    lbl <- e$label
    if (is.character(lbl) && length(lbl) == 1L && !is.na(lbl) && nzchar(lbl)) {
      parts <- c(parts, lbl)
      next
    }

    vals <- as.character(unlist(e$values, use.names = FALSE))
    vals[vals == VALUE_FILTER_NA] <- "NA"
    vals[vals == VALUE_FILTER_EMPTY] <- "(empty)"
    shown <- trail_values(vals)

    if (is.null(shown)) {
      next
    }

    prefix <- if (qualify && nzchar(tables[[i]])) paste0(tables[[i]], ".") else ""
    parts <- c(parts, paste0(prefix, e$name, " = ", shown))
  }

  if (!length(parts)) {
    return(NULL)
  }

  paste(parts, collapse = if (identical(operator, "|")) " or " else "; ")
}

# Render the crossfilter's live state as one clause. The table is named only
# when more than one is filtered: on a single-table filter the column alone
# reads better, and on a multi-table one it is the whole point (a filter
# written against `orders` restricts `customers` through the key graph, so a caption that
# does not say which table it was written against is misleading).
crossfilter_clause <- function(cat_filters, range_filters) {

  tables <- unique(c(names(cat_filters), names(range_filters)))
  qualify <- length(tables) > 1L
  parts <- character()

  for (tbl in tables) {

    prefix <- if (qualify) paste0(tbl, ".") else ""

    for (dim in names(cat_filters[[tbl]])) {
      vals <- trail_values(cat_filters[[tbl]][[dim]])
      if (!is.null(vals)) {
        parts <- c(parts, paste0(prefix, dim, " = ", vals))
      }
    }

    for (dim in names(range_filters[[tbl]])) {
      rng <- unlist(range_filters[[tbl]][[dim]], use.names = FALSE)
      if (length(rng) == 2L && !anyNA(rng)) {
        parts <- c(
          parts,
          paste0(prefix, dim, " ", format(rng[[1L]]), " to ", format(rng[[2L]]))
        )
      }
    }
  }

  if (!length(parts)) {
    return(NULL)
  }

  paste(parts, collapse = "; ")
}
