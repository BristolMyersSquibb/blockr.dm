# ============================================================
# Key lines — data-model viewer (replaces the dm_draw graph)
#
# Join keys are drawn as thin vertical colored lines down a left
# gutter; each table is a single row that picks up a node on every
# line whose key it carries. Source (PK owner) nodes are filled,
# referencing (FK) nodes are open. Click a row to preview its data.
#
# Ported from the design handoff (dm-viewer.js `keyLines`,
# `selfRefsOf`, `computeDepths`, `renderRails2`) onto real `dm`
# metadata. Lines are built from the FK edges (parent table + parent
# key columns), NOT column-name coincidence, so renamed FKs
# (`origin -> faa`) and composite keys (`origin + time_hour`) render.
# The schematic is the routed "Linked" layout (parents above children,
# lines jogging columns at the row that owns them) with Smart row
# detail (a tag for only the mappings the gutter can't show). Markup is
# built with htmltools tag builders, which escape interpolated values.
# ============================================================

# per-key line palette, assigned by descending member count
dm_keylines_palette <- function() {
  c("#2563eb", "#0d9488", "#7c3aed", "#b45309", "#be185d", "#4338ca")
}

#' Read the FK edges of a dm as a plain list
#'
#' Each edge is `list(child, child_cols, parent, parent_cols)` with the
#' column tuples as character vectors. This is the single source the whole
#' Key-lines derivation hangs off (mirrors `getEdges` in the design).
#' @keywords internal
dm_keylines_edges <- function(dm_obj) {
  fks <- tryCatch(dm::dm_get_all_fks(dm_obj), error = function(e) NULL)
  if (is.null(fks) || nrow(fks) == 0) return(list())
  lapply(seq_len(nrow(fks)), function(i) {
    list(
      child = as.character(fks$child_table[i]),
      child_cols = as.character(fks$child_fk_cols[[i]]),
      parent = as.character(fks$parent_table[i]),
      parent_cols = as.character(fks$parent_key_cols[[i]])
    )
  })
}

#' Group FK edges into key lines (one line per distinct parent key)
#'
#' Every edge pointing at the same `parent` + `parent_cols` becomes one
#' line; the parent is the line owner (filled node), every child attaches
#' as a referencing node regardless of the child column's name. Lines with
#' fewer than two member tables are dropped. Colours are assigned by
#' descending member count.
#' @keywords internal
dm_keylines_lines <- function(edges) {
  lines <- list()
  ord <- character(0) # preserve first-seen order for stable colouring ties
  for (e in edges) {
    if (identical(e$child, e$parent)) next # self-ref: handled as a row tag
    id <- paste0(e$parent, "|", paste(e$parent_cols, collapse = ","))
    if (is.null(lines[[id]])) {
      lines[[id]] <- list(
        id = id, parent = e$parent, parent_cols = e$parent_cols,
        name = paste(e$parent_cols, collapse = " + "),
        composite = length(e$parent_cols) > 1,
        members = e$parent, entries = list()
      )
      ord <- c(ord, id)
    }
    lines[[id]]$members <- union(lines[[id]]$members, c(e$parent, e$child))
    lines[[id]]$entries <- c(lines[[id]]$entries, list(list(
      child = e$child, child_cols = e$child_cols,
      differs = paste(e$child_cols, collapse = ",") !=
        paste(e$parent_cols, collapse = ",")
    )))
  }
  lines <- lines[ord]
  lines <- Filter(function(L) length(L$members) >= 2, lines)
  if (!length(lines)) return(list())

  # order by descending member count (stable on first-seen), assign colour + lid
  ord2 <- order(-vapply(lines, function(L) length(L$members), integer(1)),
                seq_along(lines))
  lines <- lines[ord2]
  palette <- dm_keylines_palette()
  for (i in seq_along(lines)) {
    lines[[i]]$color <- palette[((i - 1L) %% length(palette)) + 1L]
    lines[[i]]$lid <- paste0("L", i - 1L)
  }
  lines
}

#' Self-referential FKs (`child == parent`): no lane, surfaced as a row tag
#' @keywords internal
dm_keylines_selfrefs <- function(edges) {
  out <- list()
  for (e in edges) {
    if (identical(e$child, e$parent)) {
      out[[e$child]] <- c(out[[e$child]],
        list(list(child_cols = e$child_cols, parent_cols = e$parent_cols)))
    }
  }
  out
}

#' FK depth per table (parents above children) — drives the routed layout
#' @keywords internal
dm_keylines_depths <- function(table_names, edges) {
  parents <- stats::setNames(vector("list", length(table_names)), table_names)
  for (e in edges) {
    if (e$child %in% table_names && !(e$parent %in% parents[[e$child]])) {
      parents[[e$child]] <- c(parents[[e$child]], e$parent)
    }
  }
  memo <- list()
  stack <- list()
  d <- function(id) {
    if (!is.null(memo[[id]])) return(memo[[id]])
    if (isTRUE(stack[[id]])) return(0L) # cycle guard
    stack[[id]] <<- TRUE
    mx <- 0L
    for (p in parents[[id]]) mx <- max(mx, 1L + d(p))
    stack[[id]] <<- FALSE
    memo[[id]] <<- mx
    mx
  }
  for (tn in table_names) d(tn)
  vapply(table_names, function(tn) as.integer(memo[[tn]]), integer(1))
}

#' Line memberships of one table (for its relationship tags)
#' @keywords internal
dm_keylines_memberships <- function(lines, table_id) {
  out <- list()
  for (L in lines) {
    if (identical(L$parent, table_id)) {
      out <- c(out, list(list(role = "parent", color = L$color, name = L$name,
        local_cols = L$parent_cols, composite = L$composite, self = FALSE)))
    }
    for (e in L$entries) {
      if (identical(e$child, table_id)) {
        out <- c(out, list(list(role = "child", color = L$color, name = L$name,
          local_cols = e$child_cols, differs = e$differs,
          self = identical(e$child, L$parent))))
      }
    }
  }
  out
}

#' Distinct line ids a table sits on (gutter membership / data-keys)
#' @keywords internal
dm_keylines_lids <- function(lines, table_id) {
  vapply(Filter(function(L) table_id %in% L$members, lines),
         function(L) L$lid, character(1))
}

#' Count of distinct FKs a table has to one parent key (>1 = `xN` badge)
#' @keywords internal
dm_keylines_childcount <- function(L, table_id) {
  sum(vapply(L$entries, function(e) {
    identical(e$child, table_id) && !identical(e$child, L$parent)
  }, logical(1)))
}

#' Extract Key-lines metadata from a dm object
#'
#' Derives, purely from `dm` metadata (no graph engine): the table rows,
#' the FK-edge-driven key lines, self-referential keys, per-table FK depth,
#' and the table + lane order for the routed layout (parents above children,
#' widest key leftmost).
#'
#' @param dm_obj A dm object
#' @return A list with `tables`, `lines`, `self_refs`, `order` and
#'   `lane_order`.
#' @keywords internal
dm_keylines_meta <- function(dm_obj) {
  tbls <- dm::dm_get_tables(dm_obj)
  table_names <- names(tbls)

  tables <- lapply(table_names, function(tn) {
    raw <- tbls[[tn]]
    label <- attr(raw, "label")
    if (is.null(label) || (length(label) == 1L && is.na(label))) label <- ""
    # Only count rows for in-memory frames (nrow is O(1)). For lazy /
    # remote tables (tbl_lazy / tbl_sql) nrow() would force a COUNT(*)
    # query per table on every render, so leave it blank instead.
    rows <- if (is.data.frame(raw)) {
      tryCatch(as.integer(nrow(raw)), error = function(e) NA_integer_)
    } else {
      NA_integer_
    }
    list(id = tn, name = tn, label = label, rows = rows)
  })
  names(tables) <- table_names

  edges <- dm_keylines_edges(dm_obj)
  lines <- dm_keylines_lines(edges)
  self_refs <- dm_keylines_selfrefs(edges)
  depth <- dm_keylines_depths(table_names, edges)

  if (length(lines)) {
    oi <- stats::setNames(seq_along(table_names) - 1L, table_names)
    # rows + lanes ordered by FK-hierarchy depth (root key leftmost / topmost)
    order_ids <- table_names[order(depth[table_names], oi[table_names])]
    lane_order <- lines[order(
      vapply(lines, function(L) depth[[L$parent]], integer(1)),
      -vapply(lines, function(L) length(L$members), integer(1)),
      vapply(lines, function(L) L$name, character(1))
    )]
  } else {
    order_ids <- table_names
    lane_order <- lines
  }

  list(tables = tables, lines = lines, self_refs = self_refs,
       order = order_ids, lane_order = lane_order)
}

#' Render the Key-lines schematic for a dm object
#'
#' @param meta Output of [dm_keylines_meta()]
#' @param root_id DOM id for the schematic root (the click target)
#' @keywords internal
dm_keylines_html <- function(meta, root_id) {
  lines <- meta$lines
  lane_order <- meta$lane_order
  order_ids <- meta$order
  tables <- meta$tables
  self_refs <- meta$self_refs
  no_lines <- length(lines) == 0L

  n_lines <- length(lines)
  left_pad <- if (no_lines) 0L else 22L
  right_pad <- if (no_lines) 0L else 18L
  row_h <- 40L
  # adaptive lane gap: tighten above 6 lanes so the gutter never crowds rows
  col_w <- if (n_lines > 6L) max(15, 22 - (n_lines - 6) * 1.2) else 22
  gutter <- left_pad + max(0L, n_lines - 1L) * col_w + right_pad
  header_h <- if (no_lines) 0L else n_lines * 40L + 20L
  n_h <- length(order_ids) * row_h

  row_idx <- stats::setNames(seq_along(order_ids) - 1L, order_ids)
  key_x <- stats::setNames(
    if (n_lines) left_pad + (seq_len(n_lines) - 1L) * col_w else numeric(0),
    vapply(lane_order, function(L) L$lid, character(1))
  )

  fmt_rows <- function(n) {
    if (is.na(n)) "" else format(n, big.mark = ",", trim = TRUE,
                                 scientific = FALSE)
  }
  # build an SVG element (htmltools has no svg/circle/line/... helpers); it
  # preserves attribute names verbatim, so camelCase attrs like viewBox survive
  svg_tag <- function(name, ...) htmltools::tag(name, list(...))

  # --- line caps: each key button sits at the head of its own line -----------
  caps <- NULL
  if (!no_lines) {
    caps_inner <- lapply(seq_along(lane_order), function(i) {
      L <- lane_order[[i]]
      cx <- key_x[[L$lid]]
      cy <- 16L + (i - 1L) * 40L
      shiny::tags$button(
        class = paste0("r2key", if (L$composite) " r2key--comp" else ""),
        type = "button", `data-key` = L$lid, `data-owner` = L$parent,
        style = sprintf("--line:%s;left:%spx;top:%spx", L$color, cx - 14L, cy - 11L),
        title = L$name,
        shiny::tags$span(class = "r2key__sw"),
        shiny::tags$span(class = "r2key__name", L$name),
        shiny::tags$span(class = "r2key__n num", length(L$members))
      )
    })
    caps <- shiny::tags$div(class = "rails2__caps",
                            style = sprintf("height:%spx", header_h), caps_inner)
  }

  # --- svg wires: one colored line per key, routed jogs, nodes per member ----
  wires <- NULL
  if (!no_lines) {
    wires <- lapply(seq_along(lane_order), function(i) {
      L <- lane_order[[i]]
      x <- key_x[[L$lid]]
      cap_y <- 16L + (i - 1L) * 40L
      idxs <- sort(unname(row_idx[L$members]))
      y_last <- header_h + idxs[length(idxs)] * row_h + row_h / 2

      # routed interchange: at this key's OWNER row, jog in from each parent
      # lane whose child IS this owner — the line switches columns there.
      y_o <- header_h + row_idx[[L$parent]] * row_h + row_h / 2
      jogs <- lapply(lane_order, function(P) {
        if (identical(P$lid, L$lid)) return(NULL)
        feeds <- any(vapply(P$entries,
          function(e) identical(e$child, L$parent), logical(1)))
        if (!feeds) return(NULL)
        svg_tag("path", class = "r2jog",
                d = sprintf("M%s %s L%s %s", key_x[[P$lid]], y_o, x, y_o))
      })

      nodes <- lapply(L$members, function(id) {
        cyn <- header_h + row_idx[[id]] * row_h + row_h / 2
        owner <- identical(id, L$parent)
        mult <- dm_keylines_childcount(L, id)
        # hover tooltip carries the PK/FK meaning (replaces the legend): the
        # filled node owns the key, hollow nodes reference it (showing the
        # renamed mapping where the child columns differ).
        tip <- if (owner) {
          paste0("primary key · ", L$name)
        } else {
          maps <- vapply(
            Filter(function(e) identical(e$child, id), L$entries),
            function(e) {
              lc <- paste(e$child_cols, collapse = " + ")
              if (isTRUE(e$differs)) paste0(lc, " → ", L$name) else lc
            }, character(1)
          )
          paste0("foreign key · ", paste(unique(maps), collapse = ", "))
        }
        node <- svg_tag("circle",
          class = paste0("r2node", if (owner) " r2node--src" else "",
                         if (mult > 1L) " r2node--multi" else ""),
          `data-table` = id, cx = x, cy = cyn, r = if (owner) 5 else 4,
          svg_tag("title", tip))
        if (mult > 1L) {
          list(node, svg_tag("text", class = "r2nodect",
                             x = x + 7, y = cyn + 3, mult))
        } else node
      })

      svg_tag("g", class = "r2wire", `data-key` = L$lid,
              style = sprintf("--line:%s", L$color),
              svg_tag("line", class = "r2line",
                      x1 = x, y1 = cap_y, x2 = x, y2 = y_last),
              jogs, nodes)
    })
  }

  svg <- svg_tag("svg", class = "rails2__wire",
                 width = gutter, height = header_h + n_h,
                 viewBox = sprintf("0 0 %s %s", gutter, header_h + n_h),
                 wires)

  # --- rows: one full-width row per table ------------------------------------
  # Smart row detail (default): the description plus a tag for ONLY the mappings
  # the gutter can't show — renamed FKs, self-references. Same-name FKs and the
  # owner PK stay implicit (cap label + filled/open nodes convey them).
  rows_inner <- lapply(order_ids, function(id) {
    t <- tables[[id]]
    lids <- dm_keylines_lids(lines, id)
    mem <- dm_keylines_memberships(lines, id)
    selfs <- self_refs[[id]] %||% list()

    key_tags <- list()
    # self-referential keys first (always shown — no lane represents them)
    for (s in selfs) {
      key_tags <- c(key_tags, list(shiny::tags$span(
        class = "r2tag r2tag--self",
        shiny::tags$span(class = "r2tag__loop", "↺"),
        shiny::tags$span(class = "r2tag__t", paste(s$child_cols, collapse = " + ")),
        shiny::tags$span(class = "r2tag__arrow", "→"),
        shiny::tags$span(class = "r2tag__to", paste(s$parent_cols, collapse = " + "))
      )))
    }
    for (m in mem) {
      if (isTRUE(m$self)) next # shown by the dedicated self tag
      local <- paste(m$local_cols, collapse = " + ")
      if (identical(m$role, "parent")) {
        next # smart: PK conveyed by the filled gutter node
      } else if (isTRUE(m$differs)) {
        key_tags <- c(key_tags, list(shiny::tags$span(
          class = "r2tag r2tag--fk r2tag--map",
          style = sprintf("--line:%s", m$color),
          shiny::tags$span(class = "r2tag__dot r2tag__dot--fk"),
          shiny::tags$span(class = "r2tag__t", local),
          shiny::tags$span(class = "r2tag__arrow", "→"),
          shiny::tags$span(class = "r2tag__to", m$name)
        )))
      }
      # same-name simple FK: implicit in smart mode (cap + open node convey it)
    }
    keys <- if (length(key_tags)) {
      shiny::tags$span(class = "r2row__keys", key_tags)
    } else NULL

    shiny::tags$button(
      class = "r2row", type = "button",
      `data-table` = id, `data-keys` = paste(lids, collapse = " "),
      `aria-pressed` = "false", style = sprintf("height:%spx", row_h),
      shiny::tags$span(class = "r2row__id",
        shiny::tags$span(class = "r2row__idtxt",
          shiny::tags$span(class = "r2row__name", t$name))),
      shiny::tags$span(class = "r2row__desc", t$label),
      keys,
      shiny::tags$span(class = "r2row__rows num", fmt_rows(t$rows))
    )
  })
  rows <- shiny::tags$div(class = "rails2__rows",
    style = sprintf("padding-left:%spx;padding-top:%spx", gutter, header_h),
    rows_inner)

  # --- zero-FK banner -------------------------------------------------------
  # No standing legend: the filled/hollow node convention is conveyed on hover
  # (SVG <title> tooltips on the nodes), keeping the surface uncluttered.
  banner <- if (no_lines) {
    shiny::tags$div(class = "rails2__banner", sprintf(
      paste0("No foreign keys in this model — %d independent tables. ",
             "Select one to preview its rows."), length(tables)))
  } else NULL

  shiny::tags$div(
    class = "dmv__schemawrap",
    shiny::tags$div(
      class = paste0("dmv-rails2", if (no_lines) " dmv-rails2--nolines" else ""),
      id = root_id, style = sprintf("--gutter:%spx;--r2w:2px", gutter),
      banner,
      shiny::tags$div(class = "rails2__body",
                      style = sprintf("height:%spx", header_h + n_h),
                      caps, svg, rows)
    )
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
    }
    .dmv-rails2 { padding:10px 4px 12px; position:relative; }
    .rails2__banner { margin:0 0 2px; padding:2px 2px 10px; font-size:12px;
      color:var(--dmv-ink-3); text-wrap:pretty; }
    .rails2__body { position:relative; }
    .rails2__caps { position:absolute; top:0; left:0; right:0; z-index:2;
      pointer-events:none; }
    .r2key { position:absolute; pointer-events:auto; white-space:nowrap;
      display:inline-flex; align-items:center; gap:7px; padding:4px 8px 4px 9px;
      border:1px solid var(--dmv-hair-strong); border-radius:7px;
      background:var(--dmv-surface-1); cursor:pointer; font:inherit;
      color:var(--dmv-ink-2); box-shadow:0 1px 2px rgba(16,24,40,.05);
      transition:border-color .14s,color .14s,background .14s; }
    .r2key:hover { border-color:var(--line); color:var(--dmv-ink-1);
      background:color-mix(in srgb,var(--line) 7%,var(--dmv-surface-1)); }
    .r2key__sw { width:9px; height:9px; border-radius:2px; background:var(--line);
      flex:none; }
    /* composite key: doubled offset swatch reads as a combined key */
    .r2key--comp .r2key__sw { border-radius:2px;
      box-shadow:2px 0 0 -0.5px var(--dmv-surface-1),3px 0 0 var(--line); }
    .r2key__name { font-size:12px; color:inherit; max-width:180px;
      overflow:hidden; text-overflow:ellipsis; }
    .r2key__n { font-size:10.5px; color:var(--dmv-ink-3);
      background:var(--dmv-surface-2); border-radius:4px; padding:0 6px;
      line-height:1.7; }
    .rails2__wire { position:absolute; left:0; top:0; overflow:visible;
      pointer-events:none; }
    .r2line { stroke:var(--line); stroke-width:var(--r2w,2); opacity:.45;
      stroke-linecap:round; }
    .r2jog { fill:none; stroke:var(--line); stroke-width:var(--r2w,2);
      opacity:.55; stroke-linecap:round; }
    .r2node { fill:#fff; stroke:var(--line); stroke-width:var(--r2w,2); }
    .r2node--src { fill:var(--line); }
    .r2node.is-selected { fill:var(--line); stroke:var(--line); r:6px; }
    .r2nodect { font-size:8px; font-weight:700; fill:var(--line); }
    .dmv-rails2.keyhot .r2wire { opacity:.1; transition:opacity .14s; }
    .dmv-rails2.keyhot .r2wire.is-hot { opacity:1; }
    .dmv-rails2.keyhot .r2wire.is-hot .r2line { opacity:.85; stroke-width:3; }
    .rails2__rows { position:relative; display:flex; flex-direction:column; }
    .dmv-rails2--nolines .rails2__rows { padding-left:2px!important; }
    .r2row { position:relative; display:flex; align-items:center; gap:14px;
      width:100%; text-align:left; background:none; border:0;
      border-bottom:1px solid var(--dmv-hair); cursor:pointer; font:inherit;
      color:inherit; padding:0 12px 0 14px; }
    .r2row:last-child { border-bottom:0; }
    .r2row > * { position:relative; z-index:1; }
    .r2row::before { content:''; position:absolute; left:8px; right:6px; top:4px;
      bottom:4px; border-radius:10px; background:transparent;
      transition:background .12s; z-index:0; pointer-events:none; }
    .r2row:hover::before { background:var(--dmv-surface-2-soft); }
    .r2row.is-member::before {
      background:color-mix(in srgb,var(--dmv-surface-2) 80%,transparent); }
    .r2row.is-selected::before { background:var(--dmv-accent-soft); }
    .r2row.is-selected .r2row__name { color:var(--dmv-accent); }
    .r2row__id { display:flex; align-items:center; gap:10px; min-width:150px;
      flex:none; }
    .r2row__idtxt { display:flex; flex-direction:column; min-width:0; }
    .r2row__name { font-size:13px; font-weight:500;
      color:var(--dmv-ink-1); }
    .r2row__desc { flex:1; min-width:0; font-size:12.5px; color:var(--dmv-ink-2);
      overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
    /* relationship tags (Smart row detail) mirror the gutter nodes */
    .r2row__keys { display:flex; flex-wrap:nowrap; gap:6px; flex:1; min-width:0;
      overflow:hidden;
      -webkit-mask-image:linear-gradient(90deg,#000 88%,transparent);
      mask-image:linear-gradient(90deg,#000 88%,transparent); }
    .r2row__keys+.r2row__rows,.r2row__desc+.r2row__rows { margin-left:8px; }
    .r2tag { display:inline-flex; align-items:center; gap:4px; flex:none;
      padding:2px 7px; border-radius:6px;
      background:color-mix(in srgb,var(--line) 10%,transparent);
      white-space:nowrap; }
    .r2tag__dot { width:8px; height:8px; flex:none; border-radius:50%; }
    .r2tag__dot--pk { background:var(--line); }
    .r2tag__dot--fk { background:var(--dmv-surface-1);
      box-shadow:inset 0 0 0 1.6px var(--line); }
    .r2tag__t { font-size:11px; font-weight:500;
      color:color-mix(in srgb,var(--line) 72%,var(--dmv-ink-1)); }
    .r2tag__to { font-size:11px; }
    .r2tag--fk { background:transparent;
      box-shadow:inset 0 0 0 1px var(--dmv-hair-strong); }
    .r2tag--fk .r2tag__t { color:var(--dmv-ink-2); font-weight:500; }
    .r2tag--map .r2tag__arrow { font-size:11px;
      color:color-mix(in srgb,var(--line) 60%,var(--dmv-ink-2)); }
    .r2tag--map .r2tag__to { font-size:11px; font-weight:600;
      color:color-mix(in srgb,var(--line) 72%,var(--dmv-ink-1)); }
    .r2tag--self { background:var(--dmv-surface-2);
      box-shadow:inset 0 0 0 1px var(--dmv-hair-strong); }
    .r2tag--self .r2tag__loop { font-size:12px; font-weight:700;
      color:var(--dmv-ink-2); line-height:1; }
    .r2tag--self .r2tag__t,.r2tag--self .r2tag__to { color:var(--dmv-ink-2);
      font-weight:500; }
    .r2tag--self .r2tag__arrow { font-size:11px; color:var(--dmv-ink-3); }
    .r2row__rows { font-size:11.5px; color:var(--dmv-ink-3); flex:none;
      white-space:nowrap; }
    .r2row__rows:not(:empty)::after { content:' rows'; }
    .dmv-rails2 .num { font-variant-numeric:tabular-nums; }
    /* nodes carry their PK/FK meaning on hover (SVG <title>), so no legend */
    .r2node { cursor:default; }
    /* preview chrome: connect to the diagram with a neutral hairline + notch */
    .dm-table-preview { margin-top:8px; border-top:1px solid var(--dmv-hair-strong);
      position:relative; padding-top:10px; }
    .dm-table-preview::before { content:''; position:absolute; top:-6px; left:34px;
      width:11px; height:11px; background:var(--dmv-surface-1);
      border-left:1px solid var(--dmv-hair-strong);
      border-top:1px solid var(--dmv-hair-strong);
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
