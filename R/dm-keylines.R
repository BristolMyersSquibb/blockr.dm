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
# Baked-in defaults: Linked layout + fill selection + Smart row detail.
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

#' Table order for the non-routed layouts (DFS keeping line members adjacent)
#' @keywords internal
dm_keylines_dfs_order <- function(table_names, lines, edges) {
  owner_of <- list()
  for (L in lines) owner_of[[L$parent]] <- L
  fk_targets <- stats::setNames(vector("list", length(table_names)), table_names)
  for (e in edges) {
    fk_targets[[e$child]] <- c(fk_targets[[e$child]], e$parent)
  }
  seen <- character(0)
  out <- character(0)
  visit <- function(id) {
    if (id %in% seen || !id %in% table_names) return(invisible())
    seen[[length(seen) + 1L]] <<- id
    out[[length(out) + 1L]] <<- id
    for (tgt in fk_targets[[id]]) visit(tgt)
    L <- owner_of[[id]]
    if (!is.null(L)) for (m in L$members) visit(m)
  }
  for (L in lines) visit(L$parent)
  for (L in lines) for (m in L$members) visit(m)
  for (tn in table_names) visit(tn)
  out
}

#' Extract Key-lines metadata from a dm object
#'
#' Derives, purely from `dm` metadata (no graph engine): the table rows,
#' the FK-edge-driven key lines, self-referential keys, per-table FK depth,
#' and the table + lane order for the chosen layout variant.
#'
#' @param dm_obj A dm object
#' @param variant Layout variant: `linked` (default, routed interchange),
#'   `minimal`, `rails` or `cards`.
#' @return A list with `tables`, `lines`, `self_refs`, `order`,
#'   `lane_order` and `variant`.
#' @keywords internal
dm_keylines_meta <- function(dm_obj, variant = "linked") {
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

  if (identical(variant, "linked") && length(lines)) {
    oi <- stats::setNames(seq_along(table_names) - 1L, table_names)
    # rows + lanes ordered by FK-hierarchy depth (root key leftmost / topmost)
    order_ids <- table_names[order(depth[table_names], oi[table_names])]
    lane_order <- lines[order(
      vapply(lines, function(L) depth[[L$parent]], integer(1)),
      -vapply(lines, function(L) length(L$members), integer(1)),
      vapply(lines, function(L) L$name, character(1))
    )]
  } else {
    order_ids <- dm_keylines_dfs_order(table_names, lines, edges)
    lane_order <- lines
  }

  list(tables = tables, lines = lines, self_refs = self_refs,
       order = order_ids, lane_order = lane_order, variant = variant)
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
  variant <- meta$variant %||% "linked"
  linked <- identical(variant, "linked")
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

  # --- line caps: each key button sits at the head of its own line -----------
  caps <- ""
  if (!no_lines) {
    caps_inner <- vapply(seq_along(lane_order), function(i) {
      L <- lane_order[[i]]
      cx <- key_x[[L$lid]]
      cy <- 16L + (i - 1L) * 40L
      sprintf(
        paste0(
          '<button class="r2key%s" type="button" data-key="%s" ',
          'data-owner="%s" style="--line:%s;left:%spx;top:%spx" title="%s">',
          '<span class="r2key__sw"></span>',
          '<span class="r2key__name">%s</span>',
          '<span class="r2key__n num">%d</span></button>'
        ),
        if (L$composite) " r2key--comp" else "",
        dm_esc(L$lid), dm_esc(L$parent), L$color,
        cx - 14L, cy - 11L, dm_esc(L$name), dm_esc(L$name),
        length(L$members)
      )
    }, character(1))
    caps <- sprintf('<div class="rails2__caps" style="height:%spx">%s</div>',
                    header_h, paste(caps_inner, collapse = ""))
  }

  # --- svg wires: one colored line per key, jogs (linked), nodes per member --
  wires <- ""
  if (!no_lines) {
    wires <- paste(vapply(seq_along(lane_order), function(i) {
      L <- lane_order[[i]]
      x <- key_x[[L$lid]]
      cap_y <- 16L + (i - 1L) * 40L
      idxs <- sort(unname(row_idx[L$members]))
      y_last <- header_h + idxs[length(idxs)] * row_h + row_h / 2

      # routed interchange: at this key's OWNER row, jog in from each parent
      # lane whose child IS this owner — the line switches columns there.
      jogs <- ""
      if (linked) {
        y_o <- header_h + row_idx[[L$parent]] * row_h + row_h / 2
        jogs <- paste(vapply(lane_order, function(P) {
          if (identical(P$lid, L$lid)) return("")
          feeds <- any(vapply(P$entries,
            function(e) identical(e$child, L$parent), logical(1)))
          if (!feeds) return("")
          sprintf('<path class="r2jog" d="M%s %s L%s %s"/>',
                  key_x[[P$lid]], y_o, x, y_o)
        }, character(1)), collapse = "")
      }

      nodes <- paste(vapply(L$members, function(id) {
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
        node <- sprintf(
          paste0('<circle class="r2node%s%s" data-table="%s" cx="%s" cy="%s" ',
                 'r="%s"><title>%s</title></circle>'),
          if (owner) " r2node--src" else "",
          if (mult > 1L) " r2node--multi" else "",
          dm_esc(id), x, cyn, if (owner) 5 else 4, dm_esc(tip)
        )
        if (mult > 1L) {
          node <- paste0(node, sprintf(
            '<text class="r2nodect" x="%s" y="%s">%d</text>',
            x + 7, cyn + 3, mult))
        }
        node
      }, character(1)), collapse = "")

      sprintf(
        paste0(
          '<g class="r2wire" data-key="%s" style="--line:%s">',
          '<line class="r2line" x1="%s" y1="%s" x2="%s" y2="%s"/>%s%s</g>'
        ),
        dm_esc(L$lid), L$color, x, cap_y, x, y_last, jogs, nodes
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
  # Smart row detail (default): the description plus a tag for ONLY the mappings
  # the gutter can't show — renamed FKs, self-references. Same-name FKs and the
  # owner PK stay implicit (cap label + filled/open nodes convey them).
  rows_inner <- vapply(order_ids, function(id) {
    t <- tables[[id]]
    lids <- dm_keylines_lids(lines, id)
    mem <- dm_keylines_memberships(lines, id)
    selfs <- self_refs[[id]] %||% list()

    tag_html <- ""
    # self-referential keys first (always shown — no lane represents them)
    for (s in selfs) {
      tag_html <- paste0(tag_html, sprintf(
        paste0(
          '<span class="r2tag r2tag--self"><span class="r2tag__loop">&#8634;</span>',
          '<span class="r2tag__t">%s</span>',
          '<span class="r2tag__arrow">&#8594;</span>',
          '<span class="r2tag__to">%s</span></span>'
        ),
        dm_esc(paste(s$child_cols, collapse = " + ")),
        dm_esc(paste(s$parent_cols, collapse = " + "))
      ))
    }
    for (m in mem) {
      if (isTRUE(m$self)) next # shown by the dedicated self tag
      local <- paste(m$local_cols, collapse = " + ")
      if (identical(m$role, "parent")) {
        next # smart: PK conveyed by the filled gutter node
      } else if (isTRUE(m$differs)) {
        tag_html <- paste0(tag_html, sprintf(
          paste0(
            '<span class="r2tag r2tag--fk r2tag--map" style="--line:%s">',
            '<span class="r2tag__dot r2tag__dot--fk"></span>',
            '<span class="r2tag__t">%s</span>',
            '<span class="r2tag__arrow">&#8594;</span>',
            '<span class="r2tag__to">%s</span></span>'
          ),
          m$color, dm_esc(local), dm_esc(m$name)
        ))
      }
      # same-name simple FK: implicit in smart mode (cap + open node convey it)
    }
    tags <- if (nzchar(tag_html)) {
      sprintf('<span class="r2row__keys">%s</span>', tag_html)
    } else ""

    sprintf(
      paste0(
        '<button class="r2row" type="button" data-table="%s" ',
        'data-keys="%s" aria-pressed="false" style="height:%spx">',
        '<span class="r2row__id"><span class="r2row__idtxt">',
        '<span class="r2row__name">%s</span></span></span>',
        '<span class="r2row__desc">%s</span>%s',
        '<span class="r2row__rows num">%s</span></button>'
      ),
      dm_esc(id), dm_esc(paste(lids, collapse = " ")), row_h,
      dm_esc(t$name), dm_esc(t$label), tags, fmt_rows(t$rows)
    )
  }, character(1))
  rows <- sprintf(
    '<div class="rails2__rows" style="padding-left:%spx;padding-top:%spx">%s</div>',
    gutter, header_h, paste(rows_inner, collapse = "")
  )

  # --- zero-FK banner -------------------------------------------------------
  # No standing legend: the filled/hollow node convention is conveyed on hover
  # (SVG <title> tooltips on the nodes), keeping the surface uncluttered.
  banner <- if (no_lines) sprintf(
    paste0('<div class="rails2__banner">No foreign keys in this model ',
           '&#8212; %d independent tables. Select one to preview its rows.</div>'),
    length(tables)
  ) else ""

  body <- sprintf(
    paste0(
      '<div class="dmv-rails2%s" id="%s" data-variant="%s" data-sel="fill" ',
      'style="--gutter:%spx;--r2w:2px">%s',
      '<div class="rails2__body" style="height:%spx">%s%s%s</div></div>'
    ),
    if (no_lines) " dmv-rails2--nolines" else "",
    root_id, variant, gutter, banner, header_h + n_h, caps, svg, rows
  )

  shiny::tags$div(
    class = "dmv__schemawrap",
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
