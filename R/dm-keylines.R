# ============================================================
# Key lines — data-model viewer (replaces the dm_draw graph)
#
# Join keys are drawn as thin vertical colored lines down a left
# gutter; each table is a single row that picks up a node on every
# line whose key it carries. Source (PK owner) nodes are filled,
# referencing (FK) nodes are open. Click a row to preview its data.
#
# Ported from the design handoff (dm-viewer.js `renderRails2`,
# `keyColumns`, `orderTables`) onto real `dm` metadata. Minimal
# layout + fill selection are the baked-in defaults.
# ============================================================

# per-key line palette, assigned by descending member count
dm_keylines_palette <- function() {
  c("#2563eb", "#0d9488", "#7c3aed", "#b45309", "#be185d", "#4338ca")
}

#' HTML-escape a scalar for inline diagram markup
#' @keywords internal
dm_esc <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' Extract Key-lines metadata from a dm object
#'
#' Derives, purely from `dm` metadata (no graph engine): the table
#' rows, the set of join keys (column names that are a PK/FK in >= 2
#' tables), each key's colour and member tables, and a depth-first
#' table order that keeps each key's members vertically adjacent.
#'
#' @param dm_obj A dm object
#' @return A list with `tables`, `keys` and `order`
#' @keywords internal
dm_keylines_meta <- function(dm_obj) {
  tbls <- dm::dm_get_tables(dm_obj)
  table_names <- names(tbls)

  pks <- tryCatch(dm::dm_get_all_pks(dm_obj), error = function(e) NULL)
  fks <- tryCatch(dm::dm_get_all_fks(dm_obj), error = function(e) NULL)

  # pk columns per table
  pk_cols <- stats::setNames(vector("list", length(table_names)), table_names)
  if (!is.null(pks) && nrow(pks) > 0) {
    for (i in seq_len(nrow(pks))) {
      tn <- pks$table[i]
      pk_cols[[tn]] <- unique(c(pk_cols[[tn]], as.character(pks$pk_col[[i]])))
    }
  }

  # fk columns per table (and their parent)
  fk_cols <- stats::setNames(vector("list", length(table_names)), table_names)
  fk_parent <- list() # "table\rcol" -> parent table
  if (!is.null(fks) && nrow(fks) > 0) {
    for (i in seq_len(nrow(fks))) {
      child <- fks$child_table[i]
      parent <- fks$parent_table[i]
      cols <- as.character(fks$child_fk_cols[[i]])
      for (cc in cols) {
        fk_cols[[child]] <- unique(c(fk_cols[[child]], cc))
        fk_parent[[paste0(child, "\r", cc)]] <- parent
      }
    }
  }

  # per-table descriptor: label, row count, and the key columns it carries
  tables <- lapply(table_names, function(tn) {
    raw <- tbls[[tn]]
    label <- attr(raw, "label")
    if (is.null(label) || is.na(label)) label <- ""
    # Only count rows for in-memory frames (nrow is O(1)). For lazy /
    # remote tables (tbl_lazy / tbl_sql) nrow() would force a COUNT(*)
    # query per table on every render, so leave it blank instead.
    rows <- if (is.data.frame(raw)) {
      tryCatch(as.integer(nrow(raw)), error = function(e) NA_integer_)
    } else {
      NA_integer_
    }
    carries <- list() # col name -> "pk" | "fk"
    for (cc in pk_cols[[tn]]) carries[[cc]] <- "pk"
    for (cc in fk_cols[[tn]]) if (is.null(carries[[cc]])) carries[[cc]] <- "fk"
    list(id = tn, name = tn, label = label, rows = rows, carries = carries)
  })
  names(tables) <- table_names

  # keys = column names that are a key in >= 2 tables
  key_map <- list()
  for (tn in table_names) {
    for (cc in names(tables[[tn]]$carries)) {
      if (is.null(key_map[[cc]])) {
        key_map[[cc]] <- list(name = cc, tables = character(0), owner = NULL)
      }
      key_map[[cc]]$tables <- union(key_map[[cc]]$tables, tn)
      if (identical(tables[[tn]]$carries[[cc]], "pk")) key_map[[cc]]$owner <- tn
    }
  }
  keys <- Filter(function(k) length(k$tables) >= 2, key_map)
  # order by descending member count (stable on name)
  if (length(keys)) {
    ord <- order(-vapply(keys, function(k) length(k$tables), integer(1)),
                 vapply(keys, function(k) k$name, character(1)))
    keys <- keys[ord]
    palette <- dm_keylines_palette()
    for (i in seq_along(keys)) {
      keys[[i]]$color <- palette[((i - 1L) %% length(palette)) + 1L]
    }
  }

  order_ids <- dm_keylines_order(table_names, tables, keys)

  list(tables = tables, keys = keys, order = order_ids)
}

#' Depth-first table order so each key's members sit close together
#' @keywords internal
dm_keylines_order <- function(table_names, tables, keys) {
  owner_of <- list()
  for (k in keys) if (!is.null(k$owner)) owner_of[[k$owner]] <- k

  seen <- character(0)
  out <- character(0)
  visit <- function(id) {
    if (id %in% seen || !id %in% table_names) return(invisible())
    seen[[length(seen) + 1L]] <<- id
    out[[length(out) + 1L]] <<- id
    t <- tables[[id]]
    # recurse into FK targets, then members of the key this table owns
    for (cc in names(t$carries)) {
      if (identical(t$carries[[cc]], "fk")) {
        for (k in keys) if (identical(k$name, cc)) for (m in k$tables) visit(m)
      }
    }
    k <- owner_of[[id]]
    if (!is.null(k)) for (m in k$tables) visit(m)
  }
  for (k in keys) if (!is.null(k$owner)) visit(k$owner)
  for (k in keys) for (m in k$tables) visit(m)
  for (tn in table_names) visit(tn)
  out
}

#' Render the Key-lines schematic for a dm object
#'
#' @param meta Output of [dm_keylines_meta()]
#' @param root_id DOM id for the schematic root (the click target)
#' @keywords internal
dm_keylines_html <- function(meta, root_id) {
  keys <- meta$keys
  order_ids <- meta$order
  tables <- meta$tables

  left_pad <- 22L
  col_w <- 22L
  right_pad <- 18L
  row_h <- 40L
  n_keys <- length(keys)
  gutter <- if (n_keys > 0) left_pad + (n_keys - 1L) * col_w + right_pad else left_pad
  header_h <- if (n_keys > 0) n_keys * 30L + 20L else 0L
  n_h <- length(order_ids) * row_h

  row_idx <- stats::setNames(seq_along(order_ids) - 1L, order_ids)
  key_x <- if (n_keys > 0) {
    stats::setNames(left_pad + (seq_len(n_keys) - 1L) * col_w,
                    vapply(keys, function(k) k$name, character(1)))
  } else numeric(0)

  fmt_rows <- function(n) {
    if (is.na(n)) "" else format(n, big.mark = ",", trim = TRUE,
                                  scientific = FALSE)
  }

  # --- line caps: each key button sits at the head of its own line -----------
  caps <- ""
  if (n_keys > 0) {
    caps_inner <- vapply(seq_along(keys), function(i) {
      k <- keys[[i]]
      cx <- key_x[[k$name]]
      cy <- 15L + (i - 1L) * 30L
      sprintf(
        paste0(
          '<button class="r2key" type="button" data-key="%s" ',
          'data-owner="%s" style="--line:%s;left:%spx;top:%spx">',
          '<span class="r2key__sw"></span>',
          '<code class="r2key__name">%s</code>',
          '<span class="r2key__n num">%d</span></button>'
        ),
        dm_esc(k$name), dm_esc(k$owner %||% ""), k$color,
        cx - 14L, cy - 11L, dm_esc(k$name), length(k$tables)
      )
    }, character(1))
    caps <- sprintf('<div class="rails2__caps" style="height:%spx">%s</div>',
                    header_h, paste(caps_inner, collapse = ""))
  }

  # --- svg wires: one colored line per key + a node per member table ---------
  wires <- ""
  if (n_keys > 0) {
    wires <- paste(vapply(seq_along(keys), function(i) {
      k <- keys[[i]]
      x <- key_x[[k$name]]
      cap_y <- 15L + (i - 1L) * 30L
      idxs <- sort(unname(row_idx[k$tables]))
      y_last <- header_h + idxs[length(idxs)] * row_h + row_h / 2
      nodes <- paste(vapply(k$tables, function(id) {
        cyn <- header_h + row_idx[[id]] * row_h + row_h / 2
        owner <- identical(id, k$owner)
        sprintf(
          '<circle class="r2node%s" data-table="%s" cx="%s" cy="%s" r="%s"/>',
          if (owner) " r2node--src" else "", dm_esc(id), x, cyn,
          if (owner) 5 else 4
        )
      }, character(1)), collapse = "")
      sprintf(
        paste0(
          '<g class="r2wire" data-key="%s" style="--line:%s">',
          '<line class="r2line" x1="%s" y1="%s" x2="%s" y2="%s"/>%s</g>'
        ),
        dm_esc(k$name), k$color, x, cap_y, x, y_last, nodes
      )
    }, character(1)), collapse = "")
  }

  svg <- sprintf(
    paste0(
      '<svg class="rails2__wire" width="%s" height="%s" ',
      'viewBox="0 0 %s %s">%s</svg>'
    ),
    gutter, header_h + n_h, gutter, header_h + n_h, wires
  )

  # --- rows: one full-width row per table ------------------------------------
  rows_inner <- vapply(order_ids, function(id) {
    t <- tables[[id]]
    tk <- names(t$carries)[vapply(names(t$carries), function(cc) {
      any(vapply(keys, function(k) identical(k$name, cc), logical(1)))
    }, logical(1))]
    sprintf(
      paste0(
        '<button class="r2row" type="button" data-table="%s" ',
        'data-keys="%s" aria-pressed="false" style="height:%spx">',
        '<span class="r2row__id"><span class="r2row__idtxt">',
        '<span class="r2row__name">%s</span></span></span>',
        '<span class="r2row__desc">%s</span>',
        '<span class="r2row__rows num">%s</span></button>'
      ),
      dm_esc(id), dm_esc(paste(tk, collapse = " ")), row_h,
      dm_esc(t$name), dm_esc(t$label), fmt_rows(t$rows)
    )
  }, character(1))
  rows <- sprintf(
    '<div class="rails2__rows" style="padding-left:%spx;padding-top:%spx">%s</div>',
    gutter, header_h, paste(rows_inner, collapse = "")
  )

  body <- sprintf(
    paste0(
      '<div class="dmv-rails2" id="%s" data-variant="minimal" data-sel="fill" ',
      'style="--gutter:%spx;--r2w:2.5px">',
      '<div class="rails2__body" style="height:%spx">%s%s%s</div></div>'
    ),
    root_id, gutter, header_h + n_h, caps, svg, rows
  )

  shiny::tags$div(
    class = "dmv__schemawrap",
    shiny::tags$div(class = "dmv__schemahead", "Join keys as lines"),
    shiny::HTML(body)
  )
}

#' CSS for the Key-lines schematic + table preview
#' @keywords internal
dm_keylines_css <- function() {
  shiny::tags$style(shiny::HTML("
    .dm-output-container {
      --dmv-ink-1:#111827; --dmv-ink-2:#5b6573; --dmv-ink-3:#9aa3b0;
      --dmv-surface-1:#ffffff; --dmv-surface-2:#f1f3f6; --dmv-surface-2-soft:#f6f8fa;
      --dmv-hair:#e8ebef; --dmv-hair-strong:#dde1e7;
      --dmv-accent:#2563eb; --dmv-accent-soft:rgba(37,99,235,.09);
      --dmv-mono:'Geist Mono',ui-monospace,'SF Mono',Menlo,Consolas,monospace;
    }
    .dmv__schemahead {
      display:flex; align-items:center; gap:8px; padding:15px 4px 9px;
      font-size:10.5px; font-weight:600; letter-spacing:.07em;
      text-transform:uppercase; color:var(--dmv-ink-3);
    }
    .dmv-rails2 { padding:4px 4px 12px; position:relative; }
    .rails2__body { position:relative; }
    .rails2__caps { position:absolute; top:0; left:0; right:0; z-index:2;
      pointer-events:none; }
    .r2key { position:absolute; pointer-events:auto; white-space:nowrap;
      display:inline-flex; align-items:center; gap:7px; padding:4px 8px 4px 9px;
      border:1px solid var(--dmv-hair-strong); border-radius:999px;
      background:var(--dmv-surface-1); cursor:pointer; font:inherit;
      color:var(--dmv-ink-2); box-shadow:0 1px 2px rgba(16,24,40,.05);
      transition:border-color .14s,color .14s,background .14s; }
    .r2key:hover { border-color:var(--line); color:var(--dmv-ink-1);
      background:color-mix(in srgb,var(--line) 7%,var(--dmv-surface-1)); }
    .r2key__sw { width:10px; height:10px; border-radius:50%; background:var(--line);
      flex:none; }
    .r2key__name { font-family:var(--dmv-mono); font-size:12px; color:inherit; }
    .r2key__n { font-size:10.5px; color:var(--dmv-ink-3);
      background:var(--dmv-surface-2); border-radius:999px; padding:0 6px;
      line-height:1.7; }
    .rails2__wire { position:absolute; left:0; top:0; overflow:visible;
      pointer-events:none; }
    .r2line { stroke:var(--line); stroke-width:var(--r2w,2.5); opacity:.45;
      stroke-linecap:round; }
    .r2node { fill:#fff; stroke:var(--line); stroke-width:var(--r2w,2.5); }
    .r2node--src { fill:var(--line); }
    .r2node.is-selected { fill:var(--line); stroke:var(--line); r:6px; }
    .dmv-rails2.keyhot .r2wire { opacity:.1; transition:opacity .14s; }
    .dmv-rails2.keyhot .r2wire.is-hot { opacity:1; }
    .dmv-rails2.keyhot .r2wire.is-hot .r2line { opacity:.85; stroke-width:3; }
    .rails2__rows { position:relative; display:flex; flex-direction:column; }
    .r2row { position:relative; display:flex; align-items:center; gap:14px;
      width:100%; text-align:left; background:none; border:0;
      border-bottom:1px solid var(--dmv-hair); cursor:pointer; font:inherit;
      color:inherit; padding:0 12px 0 14px; }
    .r2row:last-child { border-bottom:0; }
    .r2row > * { position:relative; z-index:1; }
    .r2row::before { content:''; position:absolute; left:8px; right:6px; top:4px;
      bottom:4px; border-radius:10px; background:transparent;
      transition:background .12s; z-index:0; pointer-events:none; }
    .r2row:hover::before { background:var(--dmv-accent-soft); }
    .r2row.is-member::before {
      background:color-mix(in srgb,var(--dmv-surface-2) 80%,transparent); }
    .r2row.is-selected::before { background:var(--dmv-accent-soft); }
    .r2row.is-selected .r2row__name { color:var(--dmv-accent); }
    .r2row__id { display:flex; align-items:center; gap:10px; min-width:150px;
      flex:none; }
    .r2row__idtxt { display:flex; flex-direction:column; min-width:0; }
    .r2row__name { font-family:var(--dmv-mono); font-size:13px; font-weight:500;
      color:var(--dmv-ink-1); }
    .r2row__desc { flex:1; min-width:0; font-size:12.5px; color:var(--dmv-ink-2);
      overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
    .r2row__rows { font-size:11.5px; color:var(--dmv-ink-3); flex:none;
      white-space:nowrap; }
    .r2row__rows:not(:empty)::after { content:' rows'; }
    .dmv-rails2 .num { font-variant-numeric:tabular-nums; }
    /* preview chrome: connect to the diagram with an accent top border */
    .dm-table-preview { margin-top:8px; border-top:2px solid var(--dmv-accent);
      position:relative; padding-top:10px; }
    .dm-table-preview::before { content:''; position:absolute; top:-7px; left:34px;
      width:12px; height:12px; background:var(--dmv-surface-1);
      border-left:2px solid var(--dmv-accent); border-top:2px solid var(--dmv-accent);
      transform:rotate(45deg); }
    @media (prefers-reduced-motion:reduce) {
      .dmv-rails2 *, .dm-table-preview * { transition:none !important; }
    }
  "))
}

#' JS for the Key-lines schematic: selection, key-cap isolation + click
#'
#' @param root_id DOM id of the schematic root
#' @param input_id Fully-namespaced Shiny input id to receive the click
#' @keywords internal
dm_keylines_js <- function(root_id, input_id) {
  shiny::tags$script(shiny::HTML(sprintf("
    (function() {
      var root = document.getElementById('%s');
      if (!root || root.__dmvWired) return;
      root.__dmvWired = true;
      var inputId = '%s';
      var selected = null;
      function send(id) {
        if (window.Shiny) {
          Shiny.setInputValue(inputId, {id: id || '', nonce: Math.random()},
            {priority: 'event'});
        }
      }
      function sync() {
        root.querySelectorAll('.r2row').forEach(function(r) {
          r.classList.toggle('is-selected',
            r.getAttribute('data-table') === selected);
        });
        root.querySelectorAll('.r2node').forEach(function(n) {
          n.classList.toggle('is-selected',
            n.getAttribute('data-table') === selected);
        });
        root.classList.toggle('has-selection', !!selected);
      }
      function choose(id) {
        selected = (selected === id) ? null : id;
        sync();
        send(selected);
      }
      root.querySelectorAll('.r2row').forEach(function(row) {
        row.addEventListener('click', function() {
          choose(row.getAttribute('data-table'));
        });
      });
      root.querySelectorAll('.r2key').forEach(function(chip) {
        var key = chip.getAttribute('data-key');
        chip.addEventListener('mouseenter', function() {
          root.classList.add('keyhot');
          root.querySelectorAll('.r2wire').forEach(function(g) {
            g.classList.toggle('is-hot', g.getAttribute('data-key') === key);
          });
          root.querySelectorAll('.r2row').forEach(function(rr) {
            rr.classList.toggle('is-member',
              (' ' + rr.getAttribute('data-keys') + ' ')
                .indexOf(' ' + key + ' ') !== -1);
          });
        });
        chip.addEventListener('mouseleave', function() {
          root.classList.remove('keyhot');
          root.querySelectorAll('.r2wire.is-hot').forEach(function(g) {
            g.classList.remove('is-hot');
          });
          root.querySelectorAll('.r2row.is-member').forEach(function(rr) {
            rr.classList.remove('is-member');
          });
        });
        chip.addEventListener('click', function(e) {
          e.stopPropagation();
          var o = chip.getAttribute('data-owner');
          if (o) choose(o);
        });
      });
    })();
  ", root_id, input_id)))
}
