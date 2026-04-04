# Server-side paginated HTML table preview.
#
# DUPLICATED: identical file lives in blockr.extra/R/table-preview.R and
# blockr.dm/R/table-preview.R. Keep both in sync until this moves to a
# shared package (blockr.core or blockr.ui).

#' @keywords internal
apply_table_sort <- function(data, sort_col, sort_dir) {
  if (is.null(sort_col) || is.null(sort_dir) || sort_dir == "none") {
    return(data)
  }
  if (!sort_col %in% names(data)) {
    return(data)
  }
  if (sort_dir == "desc") {
    dplyr::arrange(data, dplyr::desc(.data[[sort_col]]))
  } else if (sort_dir == "na") {
    dplyr::arrange(data, !is.na(.data[[sort_col]]), .data[[sort_col]])
  } else {
    dplyr::arrange(data, .data[[sort_col]])
  }
}

#' @keywords internal
col_type_label <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    "<dttm>"
  } else if (inherits(x, "Date")) {
    "<date>"
  } else if (is.factor(x)) {
    "<fct>"
  } else if (is.logical(x)) {
    "<lgl>"
  } else if (is.integer(x)) {
    "<int>"
  } else if (is.numeric(x)) {
    "<dbl>"
  } else if (is.character(x)) {
    "<chr>"
  } else if (is.list(x)) {
    "<list>"
  } else {
    paste0("<", class(x)[1], ">")
  }
}

#' @keywords internal
format_column_inner <- function(x, max_chars = 50) {
  if (is.character(x)) {
    x
  } else {
    shaft <- pillar::pillar_shaft(x)
    trimws(format(shaft, width = max_chars))
  }
}

#' @keywords internal
build_html_table <- function(dat, total_rows, sort_state = NULL, ns = NULL,
                             page = 1L, page_size = 5L) {
  n_showing <- nrow(dat)
  n_cols <- ncol(dat)

  sort_col <- sort_state$col
  sort_dir <- sort_state$dir

  sort_input_id <- if (!is.null(ns)) ns("blockr_table_sort") else "blockr_table_sort"
  page_input_id <- if (!is.null(ns)) ns("blockr_table_page") else "blockr_table_page"

  # Handle empty data frame
  if (n_cols == 0) {
    return(
      shiny::tagList(
        shiny::tags$div(
          class = "blockr-table-container",
          `data-sort-input` = sort_input_id,
          `data-page-input` = page_input_id,
          shiny::tags$div(
            class = "blockr-table-footer",
            shiny::tags$span(class = "blockr-table-range", "Empty data frame (0 columns)")
          )
        ),
        table_preview_css(),
        table_sort_js(),
        table_pagination_js()
      )
    )
  }

  col_names <- names(dat)

  # Extract column labels (e.g. from ADaM datasets)
  col_labels <- vapply(dat, function(x) {
    lbl <- attr(x, "label")
    if (is.null(lbl)) "" else lbl
  }, character(1))
  has_labels <- any(nzchar(col_labels))

  # Pre-compute column metadata
  col_is_numeric <- vapply(dat, is.numeric, logical(1))
  col_types <- vapply(dat, col_type_label, character(1))

  # Pre-format all columns
  old_opts <- options(cli.num_colors = 1)
  on.exit(options(old_opts), add = TRUE)

  formatted <- lapply(dat, format_column_inner)

  # Pre-compute NA and negative masks
  col_na <- lapply(dat, is.na)
  col_neg <- Map(function(vec, is_num) {
    if (is_num) !is.na(vec) & vec < 0 else rep(FALSE, length(vec))
  }, dat, col_is_numeric)

  # Build header row
  header_cells <- vector("list", n_cols + 1L)
  header_cells[[1L]] <- shiny::tags$th(class = "blockr-row-number", "")
  for (j in seq_along(col_names)) {
    col_name <- col_names[j]

    # Determine sort class for this column
    header_class <- "blockr-sortable"
    sort_icon_class <- "blockr-sort-icon"
    if (!is.null(sort_col) && sort_col == col_name && sort_dir != "none") {
      sort_class_suffix <- switch(
        sort_dir,
        asc = " blockr-sort-asc",
        desc = " blockr-sort-desc",
        na = " blockr-sort-na",
        ""
      )
      header_class <- paste0(header_class, sort_class_suffix)
      icon_class_suffix <- switch(
        sort_dir,
        asc = " blockr-sort-icon-asc",
        desc = " blockr-sort-icon-desc",
        na = " blockr-sort-icon-na",
        ""
      )
      sort_icon_class <- paste0(sort_icon_class, icon_class_suffix)
    }

    label_tag <- if (has_labels && nzchar(col_labels[j])) {
      is_truncated <- nchar(col_labels[j]) > 20
      display_text <- if (is_truncated) {
        paste0(substr(col_labels[j], 1, 18), "\u2026")
      } else {
        col_labels[j]
      }
      label_args <- list(
        class = "blockr-col-label",
        display_text
      )
      if (is_truncated) {
        label_args[["data-full-label"]] <- col_labels[j]
      }
      do.call(shiny::tags$span, label_args)
    }

    name_width <- nchar(col_name) * 8 + 32
    label_width <- if (has_labels && nzchar(col_labels[j])) {
      min(nchar(col_labels[j]), 20) * 7 + 32
    } else {
      0
    }
    min_width <- min(max(name_width, label_width, 60), 250)
    th_style <- sprintf("min-width: %dpx;", min_width)

    header_cells[[j + 1L]] <- shiny::tags$th(
      class = header_class,
      style = th_style,
      `data-column` = col_name,
      shiny::tags$span(class = "blockr-col-name", col_name),
      label_tag,
      shiny::tags$span(
        class = "blockr-type-row",
        shiny::tags$span(class = "blockr-type-label", col_types[j]),
        shiny::tags$span(class = sort_icon_class)
      )
    )
  }

  # Build body rows
  body_rows <- vector("list", n_showing)
  start_row_num <- (page - 1L) * page_size

  for (i in seq_len(n_showing)) {
    row_cells <- vector("list", n_cols + 1L)
    row_cells[[1L]] <- shiny::tags$td(class = "blockr-row-number", start_row_num + i)

    for (j in seq_along(col_names)) {
      is_na <- col_na[[j]][i]
      is_neg <- col_neg[[j]][i]

      cell_class <- if (col_is_numeric[j]) {
        if (is_neg) "blockr-td-numeric blockr-negative" else "blockr-td-numeric"
      } else {
        NULL
      }

      content <- if (is_na) {
        shiny::tags$span(class = "blockr-na", "NA")
      } else {
        formatted[[j]][i]
      }

      cell_title <- if (!is_na) formatted[[j]][i] else NULL
      row_cells[[j + 1L]] <- shiny::tags$td(class = cell_class, title = cell_title, content)
    }

    body_rows[[i]] <- do.call(shiny::tags$tr, row_cells)
  }

  # Build pagination info
  max_page <- max(1L, ceiling(total_rows / page_size))
  start_row <- (page - 1L) * page_size + 1L
  end_row <- min(page * page_size, total_rows)

  range_text <- if (total_rows == 0) {
    "No rows"
  } else {
    sprintf("%d\u2013%d of %d", start_row, end_row, total_rows)
  }

  footer <- shiny::tags$div(
    class = "blockr-table-footer",
    shiny::tags$span(class = "blockr-table-range", range_text),
    shiny::tags$div(
      class = "blockr-table-nav",
      shiny::tags$button(
        class = if (page == 1L) "blockr-nav-btn disabled" else "blockr-nav-btn",
        disabled = if (page == 1L) "disabled" else NULL,
        `data-direction` = "prev",
        shiny::HTML("&#x2039;")
      ),
      shiny::tags$button(
        class = if (page >= max_page) "blockr-nav-btn disabled" else "blockr-nav-btn",
        disabled = if (page >= max_page) "disabled" else NULL,
        `data-direction` = "next",
        shiny::HTML("&#x203A;")
      )
    )
  )

  shiny::tagList(
    shiny::tags$div(
      class = "blockr-table-container",
      `data-sort-input` = sort_input_id,
      `data-page-input` = page_input_id,
      `data-current-page` = page,
      `data-max-page` = max_page,
      shiny::tags$div(
        class = "blockr-table-wrapper",
        shiny::tags$table(
          class = "blockr-table",
          shiny::tags$thead(
            do.call(shiny::tags$tr, header_cells)
          ),
          do.call(shiny::tags$tbody, body_rows)
        )
      ),
      footer
    ),
    table_preview_css(),
    table_sort_js(),
    table_pagination_js(),
    table_tooltip_js()
  )
}

#' @keywords internal
table_preview_css <- function() {
  shiny::tags$style(shiny::HTML("
    .blockr-table-container {
      background: white;
      width: 100%;
    }

    .blockr-table-wrapper {
      max-height: 400px;
      overflow-y: auto;
      overflow-x: auto;
    }

    .blockr-table {
      border-collapse: collapse;
      width: 100%;
      font-size: var(--blockr-font-size-base, 0.875rem);
    }

    .blockr-table thead {
      position: sticky;
      top: 0;
      background: white;
      z-index: 1;
    }

    .blockr-table thead tr {
      border-bottom: 1px solid var(--blockr-color-border, #e5e7eb);
    }

    .blockr-table th {
      text-align: left;
      padding: 10px 16px;
      font-weight: var(--blockr-font-weight-medium, 500);
      color: var(--blockr-color-text-primary, #111827);
      vertical-align: bottom;
      overflow: hidden;
    }

    .blockr-table th.blockr-row-number {
      width: 64px;
      text-align: center;
    }

    .blockr-col-name {
      display: inline-block;
      max-width: 100%;
      font-size: 14px;
      font-weight: var(--blockr-font-weight-medium, 500);
      color: var(--blockr-color-text-primary, #111827);
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      user-select: text;
      cursor: text;
    }

    .blockr-col-label {
      display: block;
      font-size: 11px;
      font-weight: 400;
      color: var(--blockr-color-text-muted, #6b7280);
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      max-width: 120px;
      margin-top: 1px;
    }

    .blockr-label-tooltip {
      position: fixed;
      background: #1f2937;
      color: #fff;
      font-size: 11px;
      font-weight: 400;
      padding: 4px 8px;
      border-radius: 4px;
      white-space: nowrap;
      z-index: 9999;
      pointer-events: none;
    }

    .blockr-type-row {
      display: flex;
      align-items: center;
      gap: 4px;
      margin-top: 2px;
    }

    .blockr-type-label {
      font-size: 11px;
      font-weight: var(--blockr-font-weight-normal, 400);
      color: var(--blockr-color-text-subtle, #b0b7c3);
    }

    .blockr-table tbody tr {
      border-bottom: 1px solid var(--blockr-grey-100, #f3f4f6);
      transition: background-color 0.15s ease;
    }

    .blockr-table tbody tr:hover {
      background-color: var(--blockr-color-bg-subtle, #f9fafb);
    }

    .blockr-table td {
      padding: 10px 16px;
      font-size: var(--blockr-font-size-base, 0.875rem);
      color: var(--blockr-color-text-primary, #111827);
      max-width: 200px;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
    }

    .blockr-table .blockr-row-number {
      text-align: center;
      font-size: var(--blockr-font-size-xs, 0.75rem);
      color: var(--blockr-color-text-subtle, #9ca3af);
    }

    .blockr-table .blockr-td-numeric {
      text-align: right;
      font-variant-numeric: tabular-nums;
    }

    .blockr-table .blockr-negative {
      color: var(--blockr-color-negative, #F43F5E);
    }

    .blockr-table .blockr-na {
      color: var(--blockr-color-text-subtle, #9ca3af);
      font-size: var(--blockr-font-size-xs, 0.75rem);
    }

    .blockr-table-footer {
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 12px 16px;
      border-top: 1px solid var(--blockr-color-border, #e5e7eb);
    }

    .blockr-table-range {
      font-size: 12px;
      color: #6B7280;
    }

    .blockr-table-nav {
      display: flex;
      align-items: center;
      gap: 4px;
    }

    .blockr-nav-btn {
      padding: 4px;
      border: none;
      background: transparent;
      border-radius: 4px;
      cursor: pointer;
      color: #4B5563;
      font-size: 16px;
      line-height: 1;
    }

    .blockr-nav-btn:hover:not(.disabled) {
      background-color: #F3F4F6;
    }

    .blockr-nav-btn.disabled {
      opacity: 0.3;
      cursor: not-allowed;
    }

    .blockr-table th.blockr-sortable {
      cursor: pointer;
      user-select: none;
      transition: background-color 0.15s ease;
    }

    .blockr-table th.blockr-sortable:hover {
      background-color: var(--blockr-color-bg-subtle, #f9fafb);
    }

    .blockr-sort-icon {
      display: inline-block;
      width: 12px;
      height: 12px;
      font-size: 10px;
      line-height: 12px;
      text-align: center;
    }

    .blockr-sort-icon-asc::after {
      content: '\\2191';
      color: #374151;
    }

    .blockr-sort-icon-desc::after {
      content: '\\2193';
      color: #374151;
    }

    .blockr-sort-icon-na {
      width: auto;
    }

    .blockr-sort-icon-na::after {
      content: 'NA\\2191';
      color: #374151;
      font-size: 9px;
      font-weight: 600;
      letter-spacing: -0.5px;
      white-space: nowrap;
    }

    .shiny-html-output.recalculating:has(.blockr-table-container) {
      --_shiny-fade-opacity: 1;
      opacity: 1 !important;
    }
  "))
}

#' @keywords internal
table_sort_js <- function() {
  shiny::tags$script(shiny::HTML("
    if (!window.blockrTableInit) {
      window.blockrTableInit = true;
      window.blockrScrollRestore = {};
      window.blockrColumnWidths = {};
      new MutationObserver(function(mutations) {
        mutations.forEach(function(m) {
          var output = m.target.closest('.shiny-html-output');
          if (!output || !output.id) return;
          var key = output.id;
          var wrapper = output.querySelector('.blockr-table-wrapper');
          if (!wrapper) return;
          var table = wrapper.querySelector('.blockr-table');
          if (!table || table.dataset.widthsLocked) return;
          var allThs = table.querySelectorAll('thead th');
          if (allThs.length === 0) return;
          var dataThs = table.querySelectorAll('thead th[data-column]');
          var colKey = Array.from(dataThs).map(function(th) {
            return th.dataset.column;
          }).join(',');
          var stored = window.blockrColumnWidths[key];
          if (stored && stored.colKey === colKey) {
            table.style.tableLayout = 'fixed';
            table.style.width = stored.totalWidth + 'px';
            allThs.forEach(function(th, i) {
              th.style.width = stored.widths[i] + 'px';
            });
            table.dataset.widthsLocked = '1';
          } else {
            requestAnimationFrame(function() {
              var widths = Array.from(allThs).map(function(th) {
                return th.offsetWidth;
              });
              window.blockrColumnWidths[key] = {
                colKey: colKey,
                widths: widths,
                totalWidth: table.offsetWidth
              };
            });
          }
          var saved = window.blockrScrollRestore[key];
          if (saved) {
            if (saved.col) {
              var th = wrapper.querySelector('th[data-column=\"' + saved.col + '\"]');
              if (th) {
                wrapper.scrollLeft = th.offsetLeft - saved.visualOffset;
              } else {
                wrapper.scrollLeft = saved.scrollLeft;
              }
            } else {
              wrapper.scrollLeft = saved.scrollLeft;
            }
            delete window.blockrScrollRestore[key];
          }
        });
      }).observe(document.body, { childList: true, subtree: true });
    }
    if (!window.blockrSortInit) {
      window.blockrSortInit = true;
      document.addEventListener('click', function(e) {
        if (e.target.closest('.blockr-col-name')) return;
        var header = e.target.closest('.blockr-sortable');
        if (!header) return;
        e.preventDefault();
        e.stopPropagation();
        var container = header.closest('.blockr-table-container');
        var inputId = container ? container.dataset.sortInput : null;
        if (!inputId) return;
        var col = header.dataset.column;
        var wrapper = container.querySelector('.blockr-table-wrapper');
        var output = container.closest('.shiny-html-output');
        if (wrapper && output) {
          window.blockrScrollRestore[output.id] = {
            scrollLeft: wrapper.scrollLeft,
            col: col,
            visualOffset: header.offsetLeft - wrapper.scrollLeft
          };
        }
        var currentDir = header.classList.contains('blockr-sort-asc') ? 'asc' :
                         header.classList.contains('blockr-sort-desc') ? 'desc' :
                         header.classList.contains('blockr-sort-na') ? 'na' : 'none';
        var newDir = currentDir === 'none' ? 'asc' :
                     currentDir === 'asc' ? 'desc' :
                     currentDir === 'desc' ? 'na' : 'none';
        Shiny.setInputValue(inputId, {col: col, dir: newDir}, {priority: 'event'});
        var pageInputId = container.dataset.pageInput;
        if (pageInputId) {
          Shiny.setInputValue(pageInputId, 1, {priority: 'event'});
        }
      });
    }
  "))
}

#' @keywords internal
table_pagination_js <- function() {
  shiny::tags$script(shiny::HTML("
    if (!window.blockrPaginationInit) {
      window.blockrPaginationInit = true;
      document.addEventListener('click', function(e) {
        var btn = e.target.closest('.blockr-nav-btn');
        if (!btn || btn.classList.contains('disabled')) return;
        e.preventDefault();
        e.stopPropagation();
        var container = btn.closest('.blockr-table-container');
        var inputId = container ? container.dataset.pageInput : null;
        if (!inputId) return;
        var wrapper = container.querySelector('.blockr-table-wrapper');
        var output = container.closest('.shiny-html-output');
        if (wrapper && output) {
          window.blockrScrollRestore[output.id] = {
            scrollLeft: wrapper.scrollLeft,
            col: null
          };
        }
        var currentPage = parseInt(container.dataset.currentPage) || 1;
        var maxPage = parseInt(container.dataset.maxPage) || 1;
        var direction = btn.dataset.direction;
        var newPage = direction === 'prev' ? Math.max(1, currentPage - 1) :
                      Math.min(maxPage, currentPage + 1);
        Shiny.setInputValue(inputId, newPage, {priority: 'event'});
      });
    }
  "))
}

#' @keywords internal
table_tooltip_js <- function() {
  shiny::tags$script(shiny::HTML("
    if (!window.blockrTooltipInit) {
      window.blockrTooltipInit = true;
      var tip = null;
      document.addEventListener('mouseover', function(e) {
        var label = e.target.closest('[data-full-label]');
        if (!label) return;
        if (tip) tip.remove();
        tip = document.createElement('div');
        tip.className = 'blockr-label-tooltip';
        tip.textContent = label.dataset.fullLabel;
        document.body.appendChild(tip);
        var rect = label.getBoundingClientRect();
        tip.style.left = rect.left + 'px';
        tip.style.top = (rect.bottom + 4) + 'px';
      });
      document.addEventListener('mouseout', function(e) {
        var label = e.target.closest('[data-full-label]');
        if (!label || !tip) return;
        tip.remove();
        tip = null;
      });
    }
  "))
}
