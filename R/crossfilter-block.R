#' Crossfilter Block
#'
#' Client-side crossfilter using crossfilter2.js. R builds lookup tables and
#' ships them to the browser; all filtering, counting, and UI updates happen
#' in JavaScript for instant responsiveness. Accepts either a data frame or
#' a [dm][dm::dm()] object.
#'
#' @param active_dims Named list of per-table active filter columns.
#'   E.g., `list(adsl = c("SEX", "AGE"), adae = c("AESEV"))`
#' @param filters Named list of per-table categorical filters.
#'   E.g., `list(adae = list(AESEV = c("SEVERE")))`
#' @param range_filters Named list of per-table range filters.
#'   E.g., `list(adsl = list(AGE = c(40, 80)))`
#' @param measure Measure column as `"table.column"` string, or `NULL`
#'   for row counts. E.g., `"orders.amount"`.
#' @param agg_func Aggregation function: `"sum"` or `"mean"`. Only used
#'   when `measure` is set.
#' @param featured Columns worth showing up front: they get a pill each above
#'   the cards, rank first in the search, and are the columns `pinned` may
#'   name. A pill's one job is to open and close that column's filter card.
#'   E.g. `c("SEX", "RACE", "AGE")`. Empty (the default) means no pills, but
#'   the row is still there: it carries `+ More filters`, which opens the
#'   column search and is the only way in once blockr.dock has hidden the gear
#'   on a simplified or locked board. The vocabulary itself is edited in the
#'   gear, as a `Shown up front` field beside `Measure` and `Aggregation`:
#'   dropping a column from it also takes it out of `Group by`, for every
#'   reader of the board, which is a setting and not a filter.
#' @param pinned The single column the block reports as the board's group,
#'   chosen in the `Group by` select at the top of the block. Only `featured`
#'   columns on the parent table qualify. Grouping is independent of filtering: a pinned
#'   column need not have a card, and a card does not make a column the group.
#'   `NULL` (the default) means no group.
#' @param groups Group definitions for the pinned column and the subgroup,
#'   edited in the `Show groups` section under each field. A named list keyed by
#'   column, so switching the group column and back, or swapping group and
#'   subgroup, keeps each definition. An entry is `list(columns =
#'   list(list(name = , members = , custom = ), ...))`, the columns in table
#'   order. A column with an empty name is one level shown on its own; a named
#'   one is a pool of its members, `custom = FALSE` meaning the name follows
#'   the members. A level may be in two columns (shown on its own and pooled).
#'   A level in no column is not shown. A column with no entry shows every
#'   level in [crossfilter_level_order()], one column each. The older shape,
#'   `list(show = <levels>, pools = <list>)`, is still read: its levels come
#'   first, then its pools. The block only records the definition; a package
#'   building on it (blockr.pharma's population filter) applies it.
#' @param subgroup A second column the board is split by, under `pinned`,
#'   chosen in the `Subgroup by` select below the groups. Same eligible
#'   columns as `pinned`, minus the pinned one. `NULL` (the default) means no
#'   subgroup. Its levels are pooled like the group's, through its entry in
#'   `groups`, and a button in its field swaps it with `pinned`. Like
#'   `pinned`, the block only records it; blockr.pharma's population filter
#'   stamps it as `Subgroup`.
#' @param ... Forwarded to [blockr.core::new_transform_block()]. A package
#'   building on this block passes its own `class` here: the subclass has to be
#'   set at construction, which is where block metadata is resolved from the
#'   registry. It is not a formal because every constructor formal has to come
#'   back out as block state. The same holds for `subject_unit`: the word the
#'   header counts the subject table's keys in ("patients"), set by a package
#'   that knows what its subjects are. Unset, the header counts rows.
#'
#' @return A blockr transform block with client-side crossfiltering
#'
#' @export
new_crossfilter_block <- function(
  active_dims = list(),
  filters = list(),
  range_filters = list(),
  measure = NULL,
  agg_func = NULL,
  featured = character(),
  pinned = NULL,
  groups = list(),
  subgroup = NULL,
  ...
) {
  args <- list(...)
  cls <- args[["class"]] %||% "crossfilter_block"
  args[["class"]] <- NULL
  # What the header counts. Unset, it counts rows across the tables, which is
  # all a generic dm offers. A block that knows what its subject table holds
  # (blockr.pharma's population filter: patients) names the unit, and the
  # header then counts the subject table's keys in it. An option, not state:
  # it comes from the constructor every time.
  subject_unit <- args[["subject_unit"]]
  args[["subject_unit"]] <- NULL

  do.call(
    blockr.ui::new_js_block,
    c(
      list(
        "crossfilter",
        state = list(
          active_dims = active_dims,
          filters = filters,
          range_filters = range_filters,
          measure = measure,
          agg_func = agg_func,
          featured = featured,
          pinned = pinned,
          groups = crossfilter_clean_groups(groups),
          subgroup = subgroup
        ),
        expr = crossfilter_expr,
        class = cls,
        pkg = "blockr.dm",
        options = list(subject_unit = subject_unit),
        allow_empty_state = c(
          "active_dims", "filters", "range_filters", "measure", "agg_func",
          "featured", "pinned", "groups", "subgroup"
        )
      ),
      args
    )
  )
}

# Internal helper that builds the deprecation-banner UI wrapper shared
# by the legacy js_crossfilter_block and dm_crossfilter_block constructors.
deprecated_crossfilter_banner <- function(legacy_name) {
  shiny::div(
    class = "alert alert-warning blockr-deprecated-banner",
    role = "alert",
    style = paste0(
      "margin: 8px 0; padding: 10px 12px; border-radius: 4px; ",
      "background-color: #fff3cd; color: #856404; ",
      "border: 1px solid #ffeeba; font-size: 0.9rem;"
    ),
    shiny::tags$strong("Deprecated block: "),
    "this is the legacy ",
    shiny::tags$code(legacy_name),
    ". Use ",
    shiny::tags$code("crossfilter_block"),
    " instead \u2014 same functionality, canonical name."
  )
}

#' Legacy constructor for the crossfilter block (deprecated)
#'
#' `new_js_crossfilter_block()` is the pre-rename alias kept so saved boards
#' that reference the `js_crossfilter_block` UID continue to deserialize.
#' All new code should use [new_crossfilter_block()] directly. This wrapper
#' emits a [.Deprecated()] warning at construction time and tags the
#' produced block with a `js_crossfilter_block` subclass so the served
#' Shiny UI renders a visible deprecation banner above the normal block
#' output.
#'
#' @inheritParams new_crossfilter_block
#' @return A crossfilter block carrying a `js_crossfilter_block` subclass.
#' @export
new_js_crossfilter_block <- function(
  active_dims = list(),
  filters = list(),
  range_filters = list(),
  measure = NULL,
  agg_func = NULL,
  ...
) {
  .Deprecated(
    new = "new_crossfilter_block",
    package = "blockr.dm",
    msg = paste0(
      "'new_js_crossfilter_block' is deprecated and will be removed in a ",
      "future release. Use 'new_crossfilter_block' instead \u2014 it has the ",
      "same signature and behaviour under the canonical name."
    )
  )
  blk <- new_crossfilter_block(
    active_dims = active_dims,
    filters = filters,
    range_filters = range_filters,
    measure = measure,
    agg_func = agg_func,
    ...
  )
  class(blk) <- c("js_crossfilter_block", class(blk))
  blk
}

#' @method block_ui js_crossfilter_block
#' @export
block_ui.js_crossfilter_block <- function(id, x, ...) {
  shiny::tagList(
    deprecated_crossfilter_banner("js_crossfilter_block"),
    NextMethod()
  )
}

#' Legacy dm-only crossfilter constructor (deprecated)
#'
#' `new_dm_crossfilter_block()` is the pre-unification alias kept so saved
#' boards that reference the `dm_crossfilter_block` UID continue to
#' deserialize. The block now delegates to [new_crossfilter_block()] —
#' which accepts both data frames and dm objects — and emits a
#' [.Deprecated()] warning at construction time. Produced blocks carry
#' a `dm_crossfilter_block` subclass so the served Shiny UI renders a
#' visible deprecation banner above the normal block output.
#'
#' @inheritParams new_crossfilter_block
#' @return A crossfilter block carrying a `dm_crossfilter_block` subclass.
#' @export
new_dm_crossfilter_block <- function(
  active_dims = list(),
  filters = list(),
  range_filters = list(),
  measure = NULL,
  agg_func = NULL,
  ...
) {
  .Deprecated(
    new = "new_crossfilter_block",
    package = "blockr.dm",
    msg = paste0(
      "'new_dm_crossfilter_block' is deprecated and will be removed in a ",
      "future release. Use 'new_crossfilter_block' instead \u2014 it handles ",
      "both dm and data.frame inputs under the canonical name."
    )
  )
  blk <- new_crossfilter_block(
    active_dims = active_dims,
    filters = filters,
    range_filters = range_filters,
    measure = measure,
    agg_func = agg_func,
    ...
  )
  class(blk) <- c("dm_crossfilter_block", class(blk))
  blk
}

#' @method block_ui dm_crossfilter_block
#' @export
block_ui.dm_crossfilter_block <- function(id, x, ...) {
  shiny::tagList(
    deprecated_crossfilter_banner("dm_crossfilter_block"),
    NextMethod()
  )
}

#' @method block_output crossfilter_block
#' @export
block_output.crossfilter_block <- function(x, result, session) {
  if (inherits(result, "dm")) {
    return(block_output.dm_block(x, result, session))
  }
  # Data frame or other: use blockr.extra's dynamic renderer if available,
  # otherwise a simple HTML table preview
  render_fn <- tryCatch(
    get("render_dynamic_output", asNamespace("blockr.extra")),
    error = function(e) NULL
  )
  if (!is.null(render_fn)) {
    render_fn(result, x, session)
  } else {
    shiny::renderUI({
      if (!is.data.frame(result)) return(NULL)
      dat <- utils::head(result, 100)
      shiny::tags$div(
        style = "max-height: 400px; overflow: auto;",
        shiny::tags$table(
          class = "table table-sm table-striped",
          shiny::tags$thead(shiny::tags$tr(
            lapply(names(dat), function(n) shiny::tags$th(n))
          )),
          shiny::tags$tbody(
            lapply(seq_len(nrow(dat)), function(i) {
              shiny::tags$tr(lapply(dat[i, ], function(v) {
                shiny::tags$td(as.character(v))
              }))
            })
          )
        )
      )
    })
  }
}

#' @method block_ui crossfilter_block
#' @export
block_ui.crossfilter_block <- function(id, x, ...) {
  shiny::uiOutput(shiny::NS(id, "result"))
}

#' @method block_render_trigger crossfilter_block
#' @export
block_render_trigger.crossfilter_block <- function(
  x, session = blockr.core::get_session()
) {
  NULL
}


# -- Server ------------------------------------------------------------------

# The table everything else hangs off: the one no foreign key leaves. A
# featured column may only be pinned when it lives there -- a pinned card
# splits the whole block, and a column on a child table (one row per event)
# cannot split its parent (one row per subject).
crossfilter_parent_table <- function(info) {
  tbls <- info$table_names
  if (length(tbls) <= 1L) {
    return(if (length(tbls)) tbls[[1L]] else NULL)
  }
  fks <- info$fks
  if (is.null(fks) || nrow(fks) == 0L) {
    return(NULL)
  }
  counts <- table(as.character(fks$parent_table))
  names(counts)[[which.max(counts)]]
}

# Featured column names resolved to a table and a type, in the order the board
# wrote them. A name no table offers as a dimension is dropped rather than
# reported: `featured` is a vocabulary written once for a board, and boards
# outlive the columns of any one study.
crossfilter_featured_dims <- function(featured, col_info, parent = NULL) {
  out <- list()
  search_order <- c(parent, setdiff(names(col_info), parent))
  for (nm in as.character(featured)) {
    for (tbl in search_order) {
      info <- col_info[[tbl]]
      if (is.null(info)) next
      type <- if (nm %in% unlist(info$dimensions)) {
        "categorical"
      } else if (nm %in% unlist(info$range_dimensions)) {
        "range"
      } else if (nm %in% unlist(info$date_dimensions)) {
        "date"
      } else {
        NULL
      }
      if (!is.null(type)) {
        out[[length(out) + 1L]] <- list(
          table = tbl,
          dim = nm,
          type = type,
          label = info$labels[[nm]] %||% ""
        )
        break
      }
    }
  }
  out
}

# Which featured columns may take the pinned slot: the categorical ones on the
# parent table. Everything else stays on the shelf. With no parent to resolve
# (a single table, or a schema with no keys) every featured category qualifies.
crossfilter_pinnable <- function(featured_dims, parent = NULL) {
  keep <- vapply(
    featured_dims,
    function(f) {
      identical(f$type, "categorical") &&
        (is.null(parent) || identical(f$table, parent))
    },
    logical(1)
  )
  vapply(featured_dims[keep], `[[`, character(1), "dim")
}

#' Level order of a grouping column
#'
#' The order a column's levels take wherever they are listed: in the
#' crossfilter's group definition, in the stamped `Group` column and in the
#' tables built on it. A factor gives the levels that occur, in `levels()`
#' order. Anything else gives its distinct non-missing values, sorted.
#'
#' @param x A vector.
#' @return A character vector of levels.
#' @export
#' @examples
#' crossfilter_level_order(factor(c("b", "a"), levels = c("c", "b", "a")))
#' crossfilter_level_order(c("b", NA, "a", "b"))
crossfilter_level_order <- function(x) {
  if (is.factor(x)) {
    return(levels(x)[levels(x) %in% as.character(x)])
  }
  sort(unique(stats::na.omit(as.character(x))))
}

# Levels of `col` in the table the pinned column lives on, with the number of
# rows at each, in crossfilter_level_order(). Read from the block's INPUT, so
# the counts are those of the unfiltered parent table.
crossfilter_levels_with_n <- function(tbl, col) {
  if (!is.data.frame(tbl) || is.null(col) || !col %in% names(tbl)) {
    return(list())
  }
  x <- tbl[[col]]
  lv <- crossfilter_level_order(x)
  n <- as.vector(table(factor(as.character(x), levels = lv)))
  unname(Map(function(v, k) list(value = v, n = k), lv, n))
}

# The name a pool gets until someone types one. Mirrors poolDefaultName() in
# crossfilter-block.js, which is what the user sees; R only needs it when a
# pool arrives without a name.
crossfilter_pool_default_name <- function(members, levels) {
  members <- as.character(members)
  if (!length(members)) {
    return("New pool")
  }
  if (length(levels) && setequal(members, levels)) {
    return("All patients")
  }
  ordered <- c(intersect(levels, members), setdiff(members, levels))
  if (length(ordered) >= 2L) {
    words <- strsplit(trimws(ordered), "\\s+")
    shared <- character()
    for (i in seq_along(words[[1L]])) {
      w <- words[[1L]][[i]]
      if (!all(vapply(words, function(ws) length(ws) >= i && ws[[i]] == w,
                      logical(1)))) {
        break
      }
      shared <- c(shared, w)
    }
    if (length(shared)) {
      return(paste("All", paste(shared, collapse = " ")))
    }
  }
  paste(ordered, collapse = " + ")
}

# One column's group definition in its canonical R shape: `columns`, an
# unnamed list of list(name, members, custom) in table order. An empty name is
# a level shown on its own, one member; a name makes a pool. Accepts what Shiny
# delivers from the client (JS arrays arrive as lists), what a saved board
# gives back (length-1 arrays arrive as scalars) and the older `show` plus
# `pools` shape, which becomes its shown levels followed by its pools. A pool
# without members is kept: it is a pool being built. A pool of several
# members without a name gets the default one.
crossfilter_clean_group_def <- function(def, levels = character()) {
  chr <- function(x) {
    x <- as.character(unlist(x, use.names = FALSE))
    x[!is.na(x)]
  }
  cols <- if (is.null(def$columns) && (!is.null(def$show) ||
                                       !is.null(def$pools))) {
    c(
      lapply(unique(chr(def$show)), function(v) {
        list(name = "", members = v, custom = FALSE)
      }),
      lapply(unname(def$pools %||% list()), function(p) {
        name <- chr(p$name)
        # A pool always had a name; an empty one asked for the default.
        if (!length(name) || !nzchar(trimws(name[[1L]]))) {
          p$name <- crossfilter_pool_default_name(chr(p$members), levels)
          p$custom <- FALSE
        }
        p
      })
    )
  } else {
    unname(def$columns %||% list())
  }
  cols <- lapply(cols, function(p) {
    members <- unique(chr(p$members))
    name <- chr(p$name)
    name <- if (length(name)) trimws(name[[1L]]) else ""
    custom <- isTRUE(as.logical(unlist(p$custom))[1L])
    if (!nzchar(name) && length(members) != 1L) {
      name <- crossfilter_pool_default_name(members, levels)
      custom <- FALSE
    }
    list(name = name, members = members, custom = nzchar(name) && custom)
  })
  list(columns = cols)
}

# No entry for a column means untouched: every level on its own, in level
# order. A definition that says exactly that is not stored.
crossfilter_group_def_is_default <- function(def, levels) {
  cols <- def$columns
  all(vapply(cols, function(p) !nzchar(p$name), logical(1))) &&
    identical(
      unlist(lapply(cols, `[[`, "members"), use.names = FALSE) %||%
        character(),
      as.character(levels)
    )
}

crossfilter_clean_groups <- function(groups) {
  if (!length(groups) || is.null(names(groups))) {
    return(list())
  }
  lapply(groups, crossfilter_clean_group_def)
}

# `groups` as the client reads it: {COL: {columns: [{name, members: [...],
# custom}]}}. as.list() keeps a one-level vector an array.
crossfilter_groups_payload <- function(groups) {
  lapply(groups, function(def) {
    list(
      columns = lapply(unname(def$columns), function(p) {
        list(
          name = as.character(p$name),
          members = as.list(as.character(p$members)),
          custom = isTRUE(p$custom)
        )
      })
    )
  })
}

# The input as a dm, and whether it came as a single table (a data frame or
# a dm of one table), which the expression filters with dplyr::filter().
crossfilter_as_dm <- function(d) {
  if (is.data.frame(d)) {
    return(list(dm = dm::dm(.tbl = d), is_df = TRUE))
  }
  stopifnot(inherits(d, "dm"))
  list(dm = d, is_df = length(names(dm::dm_get_tables(d))) == 1L)
}

# Tables (parents before children), primary and foreign keys.
crossfilter_dm_info <- function(dm_obj) {
  table_names <- names(dm::dm_get_tables(dm_obj))
  tables <- stats::setNames(
    lapply(table_names, function(tbl) dm_obj[[tbl]]),
    table_names
  )
  pks <- dm::dm_get_all_pks(dm_obj)
  fks <- dm::dm_get_all_fks(dm_obj)
  n_fks <- vapply(table_names, function(tbl) {
    sum(fks$child_table == tbl)
  }, integer(1))
  list(
    table_names = table_names[order(n_fks)],
    tables = tables,
    pks = pks,
    fks = fks
  )
}

crossfilter_key_column <- function(info, tbl_name) {
  pk_row <- info$pks[info$pks$table == tbl_name, ]
  if (nrow(pk_row) > 0) return(pk_row$pk_col[[1]][[1]])
  fk_row <- info$fks[info$fks$child_table == tbl_name, ]
  if (nrow(fk_row) > 0) return(fk_row$child_fk_cols[[1]][[1]])
  NULL
}

# Per table, which columns are categorical, range and date dimensions, which
# are measures, and their labels. Keys are no dimensions.
crossfilter_column_info <- function(info) {
  result <- list()

  for (tbl_name in info$table_names) {
    df <- info$tables[[tbl_name]]
    if (!is.data.frame(df) || ncol(df) == 0) next

    is_numeric <- vapply(df, is.numeric, logical(1))
    is_date <- vapply(df, function(col) {
      inherits(col, c("Date", "POSIXct", "POSIXlt"))
    }, logical(1))
    is_low_cardinality <- vapply(seq_along(df), function(i) {
      col <- df[[i]]
      if (is.numeric(col)) {
        length(unique(col[is.finite(col)])) <= 10
      } else {
        length(unique(col)) <= 10
      }
    }, logical(1))
    names(is_low_cardinality) <- names(df)

    is_dimension <- (!is_numeric & !is_date) |
      (is_numeric & is_low_cardinality)
    is_range_dim <- is_numeric & !is_low_cardinality
    is_date_dim <- is_date

    key_col <- crossfilter_key_column(info, tbl_name)
    fk_rows <- info$fks[info$fks$child_table == tbl_name, ]
    all_fk_cols <- if (nrow(fk_rows) > 0) {
      unique(unlist(fk_rows$child_fk_cols))
    } else {
      character()
    }
    for (ec in unique(c(key_col, all_fk_cols))) {
      if (ec %in% names(is_dimension)) {
        is_dimension[ec] <- FALSE
        is_range_dim[ec] <- FALSE
        is_date_dim[ec] <- FALSE
      }
    }

    col_labels <- vapply(names(df), function(cn) {
      lbl <- attr(df[[cn]], "label")
      if (is.null(lbl)) "" else lbl
    }, character(1))

    # as.list(): the client indexes these as keyed objects and arrays
    result[[tbl_name]] <- list(
      dimensions = as.list(names(df)[is_dimension]),
      range_dimensions = as.list(names(df)[is_range_dim]),
      date_dimensions = as.list(names(df)[is_date_dim]),
      measures = as.list(names(df)[is_numeric & !is_low_cardinality]),
      labels = as.list(col_labels)
    )
  }
  result
}

# The featured columns, which of them may be pinned, and the pin and subgroup
# as the data allows them. A pin naming a column that is not (or no longer)
# pinnable is dropped, so a saved board whose study lost the column opens
# without one. The subgroup takes the same columns as the group, never the
# group's own.
crossfilter_pin_info <- function(featured, pinned, subgroup, col_info, info) {
  parent <- crossfilter_parent_table(info)
  feat <- crossfilter_featured_dims(featured, col_info, parent)
  pinnable <- crossfilter_pinnable(feat, parent)
  pin <- intersect(pinned, pinnable)
  pin_tbl <- NULL
  if (length(pin)) {
    for (f in feat) {
      if (identical(f$dim, pin[[1L]])) {
        pin_tbl <- f$table
        break
      }
    }
  }
  sub <- intersect(subgroup, setdiff(pinnable, pin))
  list(
    featured = unname(feat),
    pinnable = as.list(pinnable),
    pinned = if (length(pin)) pin[[1L]] else NULL,
    pinned_table = pin_tbl,
    subgroup = if (length(pin) && length(sub)) sub[[1L]] else NULL
  )
}

# A column's levels and row counts, read from the unfiltered input: on
# `tbl_name` if given, else the parent table, then the rest.
crossfilter_column_levels <- function(info, column, tbl_name = NULL) {
  search <- unique(c(
    tbl_name, crossfilter_parent_table(info), info$table_names
  ))
  for (tbl in search) {
    df <- info$tables[[tbl]]
    if (is.data.frame(df) && column %in% names(df)) {
      return(crossfilter_levels_with_n(df, column))
    }
  }
  list()
}

cf_chr <- function(x) as.character(unlist(x, use.names = FALSE))

#' Data the crossfilter block draws from
#'
#' The answer to the crossfilter block's `payload` request: the lookup rows
#' the browser crossfilters (dictionary-encoded, compressed unless
#' `options(blockr.dm.crossfilter_gzip = FALSE)`), the column catalogue, and
#' the featured, pinned and subgroup columns resolved against the data, with
#' the levels of the latter two. A function of the input and the parts of the
#' state that shape it (`active_dims`, `measure`, `agg_func`, `featured`,
#' `pinned`, `subgroup`); the filters and groups travel with the state. blockx
#' answers the same request with its own DuckDB code.
#'
#' @param data The block's input, a data frame or a dm.
#' @param state The block state, as a list.
#' @param subject_unit What the header counts the parent table's keys in
#'   ("patients"), or `NULL` to count rows.
#' @return A list, sent to the client as JSON.
#' @keywords internal
#' @export
crossfilter_payload <- function(data, state, subject_unit = NULL) {
  dm_obj <- crossfilter_as_dm(data)$dm
  info <- crossfilter_dm_info(dm_obj)
  col_info <- crossfilter_column_info(info)

  active <- lapply(state$active_dims %||% list(), cf_chr)
  active <- active[lengths(active) > 0L]
  measure <- cf_chr(state$measure)
  measure <- if (length(measure) && nzchar(measure[[1L]])) measure[[1L]] else ".count"
  agg_func <- cf_chr(state$agg_func)
  agg_func <- if (length(agg_func)) agg_func[[1L]] else "sum"

  pin <- crossfilter_pin_info(
    cf_chr(state$featured), cf_chr(state$pinned), cf_chr(state$subgroup),
    col_info, info
  )

  out <- list(
    lookups = stats::setNames(list(), character()),
    dim_source = stats::setNames(list(), character()),
    parent_key = NULL,
    parent_table = NULL,
    child_fk_cols = stats::setNames(list(), character()),
    column_info = col_info,
    all_columns = col_info,
    measure = measure,
    agg_func = agg_func,
    featured = pin$featured,
    pinnable = pin$pinnable,
    pinned = pin$pinned,
    pinned_levels = if (!is.null(pin$pinned)) {
      crossfilter_column_levels(info, pin$pinned, pin$pinned_table)
    } else {
      list()
    },
    subgroup = pin$subgroup,
    subgroup_levels = if (!is.null(pin$subgroup)) {
      crossfilter_column_levels(info, pin$subgroup, pin$pinned_table)
    } else {
      list()
    },
    note = crossfilter_note(dm_obj)
  )

  if (!length(active)) {
    return(out)
  }

  measure_col <- if (measure != ".count") measure else NULL
  # the star-schema builder, then a flat one for multi-parent schemas, then
  # independent per-table lookups when there are no keys at all
  lookup_info <- build_crossfilter_lookups(
    info$tables, active, info$pks, info$fks, measure_col
  )
  if (!is.null(lookup_info) &&
        !all(unlist(active, use.names = FALSE) %in%
               names(lookup_info$dim_source))) {
    lookup_info <- NULL
  }
  if (is.null(lookup_info)) {
    lookup_info <- build_lookups_flat(dm_obj, active, measure_col)
  }
  if (is.null(lookup_info)) {
    lookup_info <- build_lookups_independent(info$tables, active, measure_col)
  }
  if (is.null(lookup_info)) {
    return(out)
  }

  # Dictionary-encode char/factor columns, serialize as columnar JSON and
  # gzip it unless a deployment opts out: the websocket never compresses.
  enc <- encode_crossfilter_payload(lookup_info$lookups)
  use_gzip <- isTRUE(getOption("blockr.dm.crossfilter_gzip", TRUE))

  out$lookups <- if (use_gzip) {
    compress_crossfilter_lookups(enc$lookups)
  } else {
    enc$lookups
  }
  # "deflate" is the Compression Streams API name for the zlib framing
  # memCompress(type = "gzip") writes
  out$compression <- if (use_gzip) "deflate"
  out$levels <- enc$levels
  out$dim_source <- lookup_info$dim_source
  out$parent_key <- lookup_info$parent_key
  out$parent_table <- lookup_info$parent_table
  out$child_fk_cols <- lookup_info$child_fk_cols
  # only when the block names what its subjects are: the header then counts
  # them, otherwise rows
  if (!is.null(subject_unit)) {
    out$parent_n <- lookup_info$parent_n
    out$subject_unit <- subject_unit
  }
  out
}

# The expression: nothing selected is identity(data), else dm::dm_filter()
# per table (dplyr::filter() for a single table), with a filter-trail clause.
# It reads the data for the column types the literals are cast to.
crossfilter_expr <- function(state, data) {
  cat_filters <- state$filters %||% list()
  rng_filters <- state$range_filters %||% list()

  if (length(cat_filters) == 0 && length(rng_filters) == 0) {
    return(quote(identity(data)))
  }

  input <- crossfilter_as_dm(data$data)
  info <- crossfilter_dm_info(input$dm)
  table_conditions <- list()

  for (tbl in unique(c(names(cat_filters), names(rng_filters)))) {
    conditions <- list()
    tbl_df <- info$tables[[tbl]]
    col_of <- function(dim) {
      if (is.data.frame(tbl_df) && dim %in% names(tbl_df)) {
        tbl_df[[dim]]
      }
    }

    tbl_cat <- cat_filters[[tbl]]
    for (dim in names(tbl_cat)) {
      cond <- crossfilter_cat_condition(dim, tbl_cat[[dim]], col_of(dim))
      if (!is.null(cond)) {
        conditions <- c(conditions, list(cond))
      }
    }

    tbl_rng <- rng_filters[[tbl]]
    for (dim in names(tbl_rng)) {
      rng <- as.numeric(unlist(tbl_rng[[dim]]))
      if (length(rng) == 2) {
        is_date_col <- dim %in% names(tbl_df) &&
          inherits(tbl_df[[dim]], c("Date", "POSIXct", "POSIXlt"))
        if (is_date_col) {
          lo_val <- call(
            "as.Date",
            as.character(as.Date(rng[1], origin = "1970-01-01"))
          )
          hi_val <- call(
            "as.Date",
            as.character(as.Date(rng[2], origin = "1970-01-01"))
          )
        } else {
          lo_val <- rng[1]
          hi_val <- rng[2]
        }
        lo <- call(">=", as.name(dim), lo_val)
        hi <- call("<=", as.name(dim), hi_val)
        conditions <- c(conditions, list(call("&", lo, hi)))
      }
    }

    if (length(conditions) > 0) {
      table_conditions[[tbl]] <- Reduce(
        function(a, b) call("&", a, b),
        conditions
      )
    }
  }

  if (length(table_conditions) == 0) {
    return(quote(identity(data)))
  }

  # What this block contributes to the filter trail (see `?filter_trail`).
  # The `identity(data)` returns above are not wrapped: identity() hands the
  # object back, so an incoming trail passes through them.
  clause <- crossfilter_clause(cat_filters, rng_filters)

  if (input$is_df) {
    combined <- Reduce(
      function(a, b) call("&", a, b),
      unname(table_conditions)
    )
    return(
      trail_expr(
        as.call(list(quote(dplyr::filter), quote(data), combined)),
        trail_key(),
        clause
      )
    )
  }

  expr <- call("dm_filter", quote(data))
  for (tbl in names(table_conditions)) {
    expr[[tbl]] <- table_conditions[[tbl]]
  }
  expr[[1]] <- quote(dm::dm_filter)
  trail_expr(expr, trail_key(), clause)
}


# -- Flat lookup builder (multi-parent / snowflake schemas) ------------------

#' Build crossfilter lookups using dm_flatten_to_tbl for arbitrary schemas.
#' Falls back to a single flat table when the standard star-schema builder
#' returns NULL.
#' @keywords internal
build_lookups_flat <- function(dm_obj, active_dims, measure_col = NULL) {
  if (!inherits(dm_obj, "dm")) return(NULL)

  table_names <- names(dm::dm_get_tables(dm_obj))
  pks <- dm::dm_get_all_pks(dm_obj)
  fks <- dm::dm_get_all_fks(dm_obj)

  # Need at least one FK to flatten
  if (nrow(fks) == 0) return(NULL)

  # Find the "fact table" — the table with most outgoing FKs
  fk_counts <- vapply(table_names, function(tbl) {
    sum(fks$child_table == tbl)
  }, integer(1))
  fact_table <- names(which.max(fk_counts))
  if (is.null(fact_table) || fk_counts[fact_table] == 0) return(NULL)

  # Flatten the dm starting from the fact table.
  # Non-recursive joins direct parents; if that misses dims, try recursive.
  flat <- tryCatch(
    dm::dm_flatten_to_tbl(dm_obj, !!rlang::sym(fact_table),
                          .join = dplyr::left_join),
    error = function(e) NULL
  )
  # If some active dims are missing, try recursive flatten
  if (!is.null(flat)) {
    all_dims <- unlist(active_dims, use.names = FALSE)
    missing <- setdiff(all_dims, names(flat))
    if (length(missing) > 0) {
      flat_r <- tryCatch(
        dm::dm_flatten_to_tbl(dm_obj, !!rlang::sym(fact_table),
                              .recursive = TRUE, .join = dplyr::left_join),
        error = function(e) NULL
      )
      if (!is.null(flat_r)) flat <- flat_r
    }
  }
  if (is.null(flat) || nrow(flat) == 0) return(NULL)

  # Select only the columns we need: active dims + key + measure.
  # Strip the measure's `<table>.` prefix using known table names — a regex
  # like `^[^.]+\\.` fails when the table name itself starts with a dot
  # (e.g. data.frame inputs wrapped as `dm(.tbl = df)`).
  measure_cn <- NULL
  if (!is.null(measure_col)) {
    for (tbl in table_names) {
      prefix <- paste0(tbl, ".")
      if (startsWith(measure_col, prefix)) {
        measure_cn <- substr(
          measure_col, nchar(prefix) + 1, nchar(measure_col)
        )
        break
      }
    }
  }
  keep_cols <- intersect(
    unique(c(all_dims, measure_cn)),
    names(flat)
  )
  if (length(keep_cols) == 0) return(NULL)

  # Find a key column (PK of fact table or first available PK)
  fact_pk <- NULL
  pk_row <- pks[pks$table == fact_table, ]
  if (nrow(pk_row) > 0) fact_pk <- pk_row$pk_col[[1]][[1]]

  if (!is.null(fact_pk) && fact_pk %in% names(flat)) {
    keep_cols <- unique(c(fact_pk, keep_cols))
  }

  lookup <- flat[, keep_cols, drop = FALSE]

  # Build dim_source: map each dim to its original table
  dim_source <- list()
  for (tbl in names(active_dims)) {
    for (d in active_dims[[tbl]]) {
      dim_source[[d]] <- tbl
    }
  }

  # Determine parent table (table with PK, no outgoing FKs)
  parent_candidates <- setdiff(
    pks$table,
    fks$child_table
  )
  parent_table <- if (length(parent_candidates) > 0) {
    parent_candidates[1]
  } else {
    pks$table[1]
  }
  if (!is.null(fact_pk)) {
    parent_key <- fact_pk
  } else {
    pk_row <- pks[pks$table == parent_table, ]
    parent_key <- if (nrow(pk_row) > 0) {
      pk_row$pk_col[[1]][[1]]
    } else {
      NULL
    }
  }

  list(
    lookups = stats::setNames(list(lookup), fact_table),
    dim_source = dim_source,
    parent_key = parent_key,
    # as.list(): named list, not named vector — keeps the keyed-object
    # JSON under future jsonlite (see column_info_per_table()).
    child_fk_cols = as.list(stats::setNames(parent_key, fact_table)),
    parent_table = parent_table,
    child_tables = fact_table
  )
}


# -- Independent lookup builder (no FK relationships) -----------------------

#' Build per-table lookups when there are no FK relationships.
#' Each table gets its own lookup with only its active dims. No cross-table
#' filtering is possible (no shared key), but within-table filtering works.
#' @keywords internal
build_lookups_independent <- function(
  tables, active_dims, measure_col = NULL
) {
  lookups <- list()
  dim_source <- list()

  for (tbl in names(active_dims)) {
    dims <- active_dims[[tbl]]
    if (length(dims) == 0) next
    df <- tables[[tbl]]
    if (is.null(df) || !is.data.frame(df)) next

    # Select active dims + measure column. Match the measure's `<table>.`
    # prefix against the current table name — a regex like `^[^.]+\\.`
    # fails when the table name itself starts with a dot (e.g. data.frame
    # inputs wrapped as `dm(.tbl = df)`).
    keep <- intersect(dims, names(df))
    if (!is.null(measure_col)) {
      prefix <- paste0(tbl, ".")
      if (startsWith(measure_col, prefix)) {
        mc <- substr(measure_col, nchar(prefix) + 1, nchar(measure_col))
        if (mc %in% names(df)) keep <- unique(c(keep, mc))
      }
    }
    if (length(keep) == 0) next

    lookups[[tbl]] <- df[, keep, drop = FALSE]
    for (d in dims) {
      if (d %in% names(df)) dim_source[[d]] <- tbl
    }
  }

  if (length(lookups) == 0) return(NULL)

  # No parent/child structure — each table is independent.
  # Use empty strings for FK cols (no cross-table join possible).
  child_fk_cols <- as.list(stats::setNames(
    rep("", length(lookups)),
    names(lookups)
  ))

  list(
    lookups = lookups,
    dim_source = dim_source,
    parent_key = "",
    child_fk_cols = child_fk_cols,
    parent_table = "",
    child_tables = names(lookups)
  )
}


# -- UI ----------------------------------------------------------------------

# The one line of text a data source can put under the cards, e.g. which
# study and extract the board is looking at. It is the `note` field of the
# `blockr_provenance` list (`?provenance`), which rides on a TABLE rather
# than on the dm: every dm verb (filter, key edits, dm_mutate_tbl) rebuilds
# the dm and drops its attributes, while a table's attributes come through,
# the way `blockr_kinds` does.
crossfilter_note <- function(dm_obj) {
  note <- provenance(dm_obj)$note
  if (is.character(note) && length(note) == 1L && !is.na(note) &&
      nzchar(note)) {
    note
  }
}
