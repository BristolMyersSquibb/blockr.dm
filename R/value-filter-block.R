#' Value filter block
#'
#' Minimal value filter for data frames or `dm` objects. The columns to filter
#' on are chosen behind the gear icon (top-right of the block). Each column
#' can be toggled between single-select (always constrains — auto-picks first
#' value) and multi-select (empty selection passes through).
#'
#' When the upstream input is a `dm`, the gear popover gains a table selector
#' so columns can be picked from any of the dm's tables. The emitted filter
#' goes through `dm::dm_filter()`, so the restriction cascades through foreign
#' keys to related tables.
#'
#' Gear/popover UX, select widget, and click-through pill styling are reused
#' from `blockr.dplyr` to match the look-and-feel of the crossfilter and dplyr
#' transform blocks.
#'
#' This is the value-selection member of the relational filter family,
#' alongside [new_dm_filter_block()] (expression-style) and
#' [new_crossfilter_block()]. It lives in `blockr.dm` because its `dm`
#' path depends on `dm`; it is fully usable in a dashboard by composing it
#' onto a board.
#'
#' @param state List with `columns` — a list of column-object entries. Each
#'   entry has `name` (column name), `table` (source table; only when input
#'   is a `dm`), `mode` (`"single"` or `"multi"`), and `values` (character
#'   vector of selected values). Old-style state
#'   (`list(columns=character, modes=list, values=list)`) is auto-migrated
#'   via [migrate_value_filter_state()] for backward compatibility.
#' @param ... Additional arguments forwarded to [blockr.core::new_transform_block()].
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dm)
#'   serve(
#'     new_value_filter_block(
#'       state = list(
#'         columns = list(
#'           list(name = "Species", mode = "single", values = "setosa")
#'         )
#'       )
#'     ),
#'     data = list(data = iris)
#'   )
#' }
#'
#' @importFrom blockr.dplyr blockr_core_js_dep blockr_blocks_css_dep blockr_select_dep
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList
#' @importFrom htmltools htmlDependency
#'
#' @export
new_value_filter_block <- function(
  state = list(columns = list()),
  ...
) {
  state <- migrate_value_filter_state(state)
  blockr.core::new_transform_block(
    # -- server ---------------------------------------------------------------
    function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        r_state <- shiny::reactiveVal(state)

        self_write <- new.env(parent = emptyenv())
        self_write$active <- FALSE

        # Send column metadata + per-column unique values on data change.
        shiny::observeEvent(data(), {
          d <- data()
          meta <- build_column_options(d)
          if (is.null(meta)) return()
          session$sendCustomMessage(
            "bi-filter-columns",
            list(
              id      = ns("filter_input"),
              columns = meta$columns,
              values  = meta$values,
              is_dm   = meta$is_dm
            )
          )
          # Re-apply single-select rule against fresh data.
          s <- enforce_single_rule(r_state(), d)
          if (!identical(s, r_state())) {
            self_write$active <- FALSE
            r_state(s)
          }
        })

        # JS -> R: user changed state.
        shiny::observeEvent(input$filter_input, {
          self_write$active <- TRUE
          incoming <- migrate_value_filter_state(input$filter_input)
          s <- enforce_single_rule(incoming, shiny::isolate(data()))
          r_state(s)
        })

        # R -> JS: external control or server-side rewrite.
        shiny::observeEvent(r_state(), {
          if (self_write$active) {
            self_write$active <- FALSE
          } else {
            session$sendCustomMessage(
              "bi-filter-update",
              list(id    = ns("filter_input"),
                   state = normalize_state_for_json(r_state()))
            )
          }
        })

        list(
          expr = shiny::reactive({
            make_filter_block_expr(
              r_state()$columns %||% list(),
              shiny::isolate(data())
            )
          }),
          state = list(state = r_state)
        )
      })
    },
    # -- ui -------------------------------------------------------------------
    function(id) {
      shiny::tagList(
        blockr.dplyr::blockr_core_js_dep(),
        blockr.dplyr::blockr_blocks_css_dep(),
        blockr.dplyr::blockr_select_dep(),
        value_filter_block_dep(),
        shiny::div(
          class = "block-container",
          shiny::div(
            id = shiny::NS(id, "filter_input"),
            class = "bi-filter-container"
          )
        )
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data) && !inherits(data, "dm")) {
        stop("Input must be a data frame or a dm")
      }
    },
    class = "value_filter_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

#' Migrate a pre-column-object filter state into the new column-object shape.
#'
#' Old shape:
#'   `list(columns = c("A", "B"), modes = list(A = "single", B = "multi"),
#'         values = list(A = "x", B = c("y","z")))`
#' New shape:
#'   `list(columns = list(list(name="A", mode="single", values="x"),
#'                        list(name="B", mode="multi",  values=c("y","z"))))`
#'
#' Idempotent: passing a new-shape state returns it unchanged.
#'
#' @param state Filter state (old or new shape).
#' @return Filter state in the new column-object shape.
#'
#' @examples
#' migrate_value_filter_state(
#'   list(columns = "Species",
#'        modes   = list(Species = "single"),
#'        values  = list(Species = "setosa"))
#' )
#'
#' @export
migrate_value_filter_state <- function(state) {
  if (is.null(state)) {
    return(list(columns = list()))
  }
  cols <- state$columns
  # Already new shape: `columns` is a list of column-object entries.
  if (is.list(cols) && length(cols) > 0L &&
      is.list(cols[[1L]]) && !is.null(cols[[1L]]$name)) {
    return(state)
  }
  if (is.list(cols) && length(cols) == 0L && is.null(state$modes) &&
      is.null(state$values)) {
    return(list(columns = list()))
  }
  cols_vec <- as.character(cols %||% character())
  modes <- state$modes %||% list()
  values <- state$values %||% list()
  entries <- lapply(cols_vec, function(cn) {
    list(
      name   = cn,
      mode   = modes[[cn]] %||% "single",
      values = as.character(values[[cn]] %||% character())
    )
  })
  list(columns = entries)
}

#' Build column metadata + per-column value options for the JS side.
#'
#' Returns a list with `columns` (metadata entries) and `values` (per-column
#' option lists), plus `is_dm` so the JS side can render the dm variant.
#' For a `dm` input, column metadata entries include `table` and `column`
#' fields and the `values` dict is keyed by `"table.column"`.
#'
#' @noRd
build_column_options <- function(data) {
  if (inherits(data, "dm")) {
    build_column_options_dm(data)
  } else if (is.data.frame(data) && ncol(data) > 0L) {
    build_column_options_df(data)
  } else {
    NULL
  }
}

#' @noRd
build_column_options_df <- function(df) {
  cols <- lapply(names(df), function(cn) {
    lbl <- attr(df[[cn]], "label", exact = TRUE)
    list(
      value = cn,
      label = if (is.null(lbl)) "" else as.character(lbl)[1L]
    )
  })
  vals <- lapply(names(df), function(cn) unique_value_options(df[[cn]]))
  names(vals) <- names(df)
  list(columns = cols, values = vals, is_dm = FALSE)
}

#' @noRd
build_column_options_dm <- function(dm_obj) {
  if (!requireNamespace("dm", quietly = TRUE)) {
    stop("Package 'dm' is required for dm input. ",
         "Install with install.packages('dm').")
  }
  tbls <- dm::dm_get_tables(dm_obj)
  cols <- list()
  vals <- list()
  for (tbl_nm in names(tbls)) {
    tbl <- as.data.frame(tbls[[tbl_nm]])
    if (ncol(tbl) == 0L) next
    for (cn in names(tbl)) {
      lbl <- attr(tbl[[cn]], "label", exact = TRUE)
      key <- paste0(tbl_nm, ".", cn)
      cols[[length(cols) + 1L]] <- list(
        value  = key,
        label  = if (is.null(lbl)) "" else as.character(lbl)[1L],
        table  = tbl_nm,
        column = cn
      )
      vals[[key]] <- unique_value_options(tbl[[cn]])
    }
  }
  list(columns = cols, values = vals, is_dm = TRUE)
}

#' Distinct, stably-ordered value options for one column.
#'
#' Honors haven-style `labels` attributes by emitting `{value, label}` pairs.
#' Otherwise returns a plain list of stringified unique values.
#' @noRd
unique_value_options <- function(col) {
  labs <- attr(col, "labels", exact = TRUE)
  uv <- unique(col)
  uv <- uv[!is.na(uv)]
  if (length(uv) == 0L) return(list())
  if (is.factor(col)) {
    uv <- uv[order(match(as.character(uv), levels(col)))]
  } else if (is.numeric(uv) || is.logical(uv)) {
    uv <- sort(uv)
  } else {
    uv <- sort(as.character(uv))
  }
  if (!is.null(labs) && is.vector(labs) && !is.null(names(labs))) {
    lab_names <- names(labs)
    lapply(uv, function(v) {
      idx <- match(v, labs)
      list(
        value = as.character(v),
        label = if (is.na(idx)) "" else as.character(lab_names[idx])
      )
    })
  } else {
    as.list(as.character(uv))
  }
}

#' Look up the source vector for a column-object entry.
#'
#' Returns `NULL` if the entry's table or column no longer exists in the
#' upstream data (data frame or dm).
#' @noRd
column_source <- function(data, entry) {
  if (inherits(data, "dm")) {
    if (!requireNamespace("dm", quietly = TRUE)) return(NULL)
    tbl_nm <- entry$table %||% ""
    if (!nzchar(tbl_nm)) return(NULL)
    tbls <- dm::dm_get_tables(data)
    if (!tbl_nm %in% names(tbls)) return(NULL)
    tbl <- as.data.frame(tbls[[tbl_nm]])
    if (!entry$name %in% names(tbl)) return(NULL)
    tbl[[entry$name]]
  } else if (is.data.frame(data)) {
    if (!entry$name %in% names(data)) return(NULL)
    data[[entry$name]]
  } else {
    NULL
  }
}

#' First (sorted, non-NA) value of a source vector, as character.
#' @noRd
first_value <- function(src) {
  if (is.null(src)) return(NULL)
  uv <- unique(src)
  uv <- uv[!is.na(uv)]
  if (length(uv) == 0L) return(NULL)
  if (is.factor(src)) {
    uv <- uv[order(match(as.character(uv), levels(src)))]
  } else if (is.numeric(uv) || is.logical(uv)) {
    uv <- sort(uv)
  } else {
    uv <- sort(as.character(uv))
  }
  as.character(uv[[1L]])
}

#' Enforce "single-select always has a value" and drop schema-missing entries.
#' @noRd
enforce_single_rule <- function(state, data) {
  if (is.null(state)) return(list(columns = list()))
  cols <- state$columns %||% list()
  if (length(cols) == 0L) return(list(columns = cols))
  # Drop entries whose source is missing in the upstream data.
  cols <- Filter(function(entry) {
    !is.null(column_source(data, entry))
  }, cols)
  for (i in seq_along(cols)) {
    entry <- cols[[i]]
    mode <- entry$mode %||% "single"
    if (!identical(mode, "single")) next
    v <- entry$values
    if (length(v) > 0L && !is.null(v)) next
    src <- column_source(data, entry)
    fv <- first_value(src)
    if (!is.null(fv)) cols[[i]]$values <- fv
  }
  list(columns = cols)
}

#' Build the filter expression — branches on input type.
#'
#' Data frame: `dplyr::filter(data, <combined-cond>)`. Empty state =
#' `dplyr::filter(data, TRUE)`.
#'
#' dm: chained `dm::dm_filter()` calls, one per table, with same-table
#' conditions joined by `&`. Empty state = `dm::dm_filter(data)` (identity).
#' @noRd
make_filter_block_expr <- function(columns, data) {
  if (length(columns) == 0L) {
    if (inherits(data, "dm")) return(bquote(dm::dm_filter(data)))
    return(bquote(dplyr::filter(data, TRUE)))
  }
  if (inherits(data, "dm")) {
    make_dm_filter_expr(columns, data)
  } else {
    make_df_filter_expr(columns, data)
  }
}

#' @noRd
make_df_filter_expr <- function(columns, df) {
  exprs <- list()
  for (entry in columns) {
    cond <- column_condition_expr(entry, df)
    if (!is.null(cond)) exprs[[length(exprs) + 1L]] <- cond
  }
  if (length(exprs) == 0L) {
    return(bquote(dplyr::filter(data, TRUE)))
  }
  combined <- combine_conds_and(exprs)
  as.call(list(quote(dplyr::filter), quote(data), combined))
}

#' @noRd
make_dm_filter_expr <- function(columns, dm_obj) {
  if (!requireNamespace("dm", quietly = TRUE)) {
    stop("Package 'dm' is required for dm input. ",
         "Install with install.packages('dm').")
  }
  tbls_dm <- dm::dm_get_tables(dm_obj)
  by_table <- list()
  for (entry in columns) {
    tbl <- entry$table %||% ""
    if (!nzchar(tbl) || !tbl %in% names(tbls_dm)) next
    src_tbl <- as.data.frame(tbls_dm[[tbl]])
    cond <- column_condition_expr(entry, src_tbl)
    if (is.null(cond)) next
    by_table[[tbl]] <- c(by_table[[tbl]], list(cond))
  }
  if (length(by_table) == 0L) {
    return(bquote(dm::dm_filter(data)))
  }
  # Build nested dm::dm_filter() calls. Table name becomes a named argument
  # — this is how dm::dm_filter() targets a table via tidy-eval.
  result <- quote(data)
  for (tbl in names(by_table)) {
    cond <- combine_conds_and(by_table[[tbl]])
    cl <- call("dm_filter", result)
    cl[[tbl]] <- cond
    cl[[1L]] <- quote(dm::dm_filter)
    result <- cl
  }
  result
}

#' Build one `<col> %in% <values>` condition for a single column-object entry.
#' Coerces string values to the source column's type when safe (numeric,
#' logical, integer).
#' @noRd
column_condition_expr <- function(entry, src_df) {
  v <- entry$values
  if (is.null(v) || length(v) == 0L) return(NULL)
  v <- as.character(v)
  col <- entry$name
  if (is.null(col) || !nzchar(col)) return(NULL)
  casted <- v
  if (is.data.frame(src_df) && col %in% names(src_df)) {
    src <- src_df[[col]]
    if (is.integer(src)) {
      intv <- suppressWarnings(as.integer(v))
      if (!any(is.na(intv))) casted <- intv
    } else if (is.numeric(src)) {
      num <- suppressWarnings(as.numeric(v))
      if (!any(is.na(num))) casted <- num
    } else if (is.logical(src)) {
      bool <- as.logical(v)
      if (!any(is.na(bool))) casted <- bool
    }
  }
  sym <- as.name(col)
  bquote(.(sym) %in% .(casted))
}

#' Combine N condition expressions with `&`.
#' @noRd
combine_conds_and <- function(exprs) {
  combined <- exprs[[1L]]
  if (length(exprs) > 1L) {
    for (i in seq.int(2L, length(exprs))) {
      combined <- bquote(.(combined) & .(exprs[[i]]))
    }
  }
  combined
}

#' Normalize state for JSON transport.
#'
#' Per-column-entry `values` need `as.list()` so length-1 vectors survive
#' `toJSON(auto_unbox = TRUE)`. The outer `columns` list survives as-is.
#' @noRd
normalize_state_for_json <- function(s) {
  if (is.null(s)) s <- list(columns = list())
  cols <- s$columns %||% list()
  cols_norm <- lapply(cols, function(e) {
    out <- list(
      name   = if (is.null(e$name)) "" else as.character(e$name)[1L],
      mode   = e$mode %||% "single",
      values = as.list(as.character(e$values %||% character()))
    )
    if (!is.null(e$table) && nzchar(e$table)) out$table <- e$table
    out
  })
  list(columns = cols_norm)
}

#' Render the block output preview.
#'
#' Mirrors the crossfilter block's pattern: delegate to
#' `block_output.dm_block` when the result is a dm (gives the interactive
#' diagram + click-to-preview-table UX), otherwise fall through to the
#' default transform-block renderer (which blockr.extra overrides into a
#' paginated HTML table when `blockr.html_table_preview = TRUE`).
#'
#' @method block_output value_filter_block
#' @export
block_output.value_filter_block <- function(x, result, session) {
  if (inherits(result, "dm")) {
    dm_method <- utils::getS3method(
      "block_output", "dm_block", optional = TRUE
    )
    if (!is.null(dm_method)) {
      return(dm_method(x, result, session))
    }
  }
  NextMethod()
}

#' HTML dependency for the value filter block JS + CSS
#' @noRd
value_filter_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "blockr-value-filter-js",
      version = utils::packageVersion("blockr.dm"),
      src = system.file("js", package = "blockr.dm"),
      script = "value-filter-block.js"
    ),
    htmltools::htmlDependency(
      name = "blockr-value-filter-css",
      version = utils::packageVersion("blockr.dm"),
      src = system.file("css", package = "blockr.dm"),
      stylesheet = "value-filter-block.css"
    )
  )
}
