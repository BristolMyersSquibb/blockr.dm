#' Filter trail
#'
#' The record of which filters produced a table, carried on the data itself as
#' a `blockr_filters` attribute so that any downstream consumer -- a chart
#' caption, a composer footnote, a code block script -- can print what was
#' applied without knowing anything about the board.
#'
#' The trail is a **named character vector, keyed by block id**. The key makes
#' three things work without further machinery: a block that re-evaluates
#' replaces its own clause rather than appending a second copy, two branches
#' meeting at a fan-in collapse to one entry per producing block, and a
#' consumer can map a clause back to the block that set it.
#'
#' @section Why every block reads and writes explicitly:
#' `dm::dm_filter()` drops attributes set on a `dm`, and
#' `dm::dm_flatten_to_tbl()` / `dm::pull_tbl()` build a fresh tibble, so a
#' trail cannot be left to survive a dm on its own. `add_filter_trail()`
#' therefore takes both the result and the block's *input*: it reads the
#' incoming trail off the input, appends this block's clause if it has one, and
#' writes the whole thing onto the output. Nothing depends on an attribute
#' surviving an operation.
#'
#' On the data-frame side dplyr does preserve attributes (`filter`, `mutate`,
#' `select`, `arrange`, `distinct`, `left_join`, `bind_rows`), so blocks that
#' merely reshape need no wrapping. `summarise()` and `tidyr::pivot_wider()`
#' do not preserve them, and a block that aggregates has to carry the trail
#' across with `add_filter_trail(out, data)` explicitly.
#'
#' @param x Object to read a trail from (a `dm`, a data frame, or anything
#'   else, in which case `NULL` is returned).
#' @param out The block's result: the object the trail is written onto.
#' @param input The block's input, read for the incoming trail. `NULL` starts
#'   a fresh trail.
#' @param key Block id under which this block's clause is stored. `NULL` (the
#'   default) carries the incoming trail without adding to it, which is what a
#'   block that reshapes rather than filters wants.
#' @param clause Human-readable description of what this block filtered, e.g.
#'   `"SEX = F; AGE 18 to 64"`. An empty or missing clause adds nothing, so a
#'   filter block with nothing selected leaves no trace.
#'
#' @return `filter_trail()` returns a named character vector or `NULL`.
#'   `add_filter_trail()` returns `out`, with a `blockr_filters` attribute when
#'   there is a trail to carry.
#'
#' @examples
#' d <- data.frame(x = 1:3)
#' d <- add_filter_trail(d, NULL, "global_filter", "SEX = F")
#' filter_trail(d)
#'
#' @export
filter_trail <- function(x) {
  trail <- attr(x, "blockr_filters", exact = TRUE)
  if (!length(trail)) {
    return(NULL)
  }
  trail
}

#' @rdname filter_trail
#' @export
add_filter_trail <- function(out, input = NULL, key = NULL, clause = NULL) {

  trail <- filter_trail(input)

  # Start from `character()`, not `NULL`: `NULL[["key"]] <- value` builds a
  # LIST, and consumers paste the trail into a caption expecting a character
  # vector.
  if (!is.character(trail)) {
    trail <- character()
  }

  if (!is.null(key) && length(clause) && !is.na(clause[[1L]]) &&
        nzchar(clause[[1L]])) {
    trail[[key]] <- clause[[1L]]
  }

  if (!length(trail)) {
    return(out)
  }

  attr(out, "blockr_filters") <- trail
  out
}

#' @param session The block server's Shiny session, used to derive the key.
#'
#' @rdname filter_trail
#' @export
trail_key <- function(session = shiny::getDefaultReactiveDomain()) {

  # NOT the `id` a block server receives. `expr_server.block()` calls every
  # block's server with `id = "expr"` (blockr.core/R/block-server.R), so using
  # it would file every block's clause under one key and each filter would
  # overwrite the one before it -- the global filter erasing the flag filter's
  # entry, which is how this was found. The module namespace carries the
  # board's block id and is unique per block per session.
  ns <- tryCatch(session$ns(""), error = function(e) NULL)

  if (is.null(ns) || !length(ns) || !nzchar(ns)) "block" else ns
}

#' @param inner The expression the block would otherwise return.
#' @param data How the block's expression refers to its input. The default,
#'   the bare symbol `data`, is what a plain block expression uses; a block
#'   with `expr_type = "bquoted"` must pass its own input slot instead, or the
#'   generated code carries a bare `data` that resolves to [utils::data()].
#'
#' @rdname filter_trail
#' @export
trail_expr <- function(inner, key = NULL, clause = NULL,
                       data = quote(data)) {

  # Self-qualified: block expressions are deparsed into the exported script.
  if (is.null(key) || !length(clause) || !nzchar(clause[[1L]])) {
    return(
      bquote(
        blockr.dm::add_filter_trail(.(inner), .(data)),
        list(inner = inner, data = data)
      )
    )
  }

  bquote(
    blockr.dm::add_filter_trail(.(inner), .(data), .(key), .(clause)),
    list(inner = inner, data = data, key = key, clause = clause[[1L]])
  )
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

# Render the crossfilter's live state as one clause. The table is named only
# when more than one is filtered: on a single-table filter the column alone
# reads better, and on a multi-table one it is the whole point (a filter
# written against `lb` restricts `ae` through the key graph, so a caption that
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
