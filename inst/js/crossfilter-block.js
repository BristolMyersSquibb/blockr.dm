(() => {
  'use strict';

  // Monotonic id for the re-init handshake (see the binding's initialize).
  let readyCounter = 0;

  // =========================================================================
  // SVG icons (blockr design system)
  // =========================================================================

  // Only the icons blockr.ui has no counterpart for. The gear, the tick and
  // both x's come from its list, Blockr.icons, which controls_dep() puts on
  // the page, and are read where they are drawn.

  const ICON_RESET = '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor"><path fill-rule="evenodd" d="M8 3a5 5 0 1 0 4.546 2.914.5.5 0 1 1 .908-.418A6 6 0 1 1 8 2v1z"/><path d="M8 4.466V.534a.25.25 0 0 1 .41-.192l2.36 1.966c.12.1.12.284 0 .384L8.41 4.658A.25.25 0 0 1 8 4.466z"/></svg>';

  // The search field's magnifier.
  const ICON_SEARCH = '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor"><path d="M11.742 10.344a6.5 6.5 0 1 0-1.397 1.398h-.001q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1zM12 6.5a5.5 5.5 0 1 1-11 0 5.5 5.5 0 0 1 11 0"/></svg>';

  // A card shows its search above this many values, as a menu shows its
  // filter box.
  const SEARCH_MIN_VALUES = 8;

  // Missing and empty values, shown as NA and (empty), and sorted last.
  const isMissingKey = (v) => v === '__NA__' || v === '__EMPTY__';

  // Bootstrap arrow-down-up: the subgroup's "switch with the group" button.
  // The "…" tool on a row of the groups list (design system, Block lists).
  const ICON_DOTS = '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor"><circle cx="3" cy="8" r="1.4"/><circle cx="8" cy="8" r="1.4"/><circle cx="13" cy="8" r="1.4"/></svg>';
  const ICON_SWAP = '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor"><path fill-rule="evenodd" d="M11.5 15a.5.5 0 0 0 .5-.5V2.707l3.146 3.147a.5.5 0 0 0 .708-.708l-4-4a.5.5 0 0 0-.708 0l-4 4a.5.5 0 1 0 .708.708L11 2.707V14.5a.5.5 0 0 0 .5.5m-7-14a.5.5 0 0 1 .5.5v11.793l3.146-3.147a.5.5 0 0 1 .708.708l-4 4a.5.5 0 0 1-.708 0l-4-4a.5.5 0 0 1 .708-.708L4 13.293V1.5a.5.5 0 0 1 .5-.5"/></svg>';


  // =========================================================================
  // Helpers
  // =========================================================================

  // Ensure value is always an array (R length-1 vectors serialize as scalars)
  function asArray(v) {
    if (v == null) return [];
    return Array.isArray(v) ? v : [v];
  }

  // Pivot columnar object {col: [...]} to array of row objects [{col: val}, ...]
  // Inflate a base64(zlib(JSON)) lookup shipped by R (see
  // compress_crossfilter_lookups). R's memCompress(type = "gzip") actually
  // emits ZLIB (RFC 1950, 0x78 0x9C header), which the Compression Streams
  // API calls 'deflate' -- 'gzip' here fails on the header. Native
  // DecompressionStream: no vendored inflate library. Async by nature --
  // setData serializes its ingests.
  async function inflateBase64(b64) {
    const bytes = Uint8Array.from(atob(b64), c => c.charCodeAt(0));
    const stream = new Blob([bytes]).stream()
      .pipeThrough(new DecompressionStream('deflate'));
    return JSON.parse(await new Response(stream).text());
  }

  function columnsToRows(cols) {
    const keys = Object.keys(cols);
    if (keys.length === 0) return [];
    const n = Array.isArray(cols[keys[0]]) ? cols[keys[0]].length : 1;
    const rows = new Array(n);
    for (let i = 0; i < n; i++) {
      const row = {};
      for (const k of keys) row[k] = Array.isArray(cols[k]) ? cols[k][i] : cols[k];
      rows[i] = row;
    }
    return rows;
  }

  // Kernel density estimation (Gaussian kernel)
  function kde(values, min, max, nGrid = 64) {
    if (values.length === 0) return Array.from({ length: nGrid }, (_, i) => ({
      x: min + i * ((max - min) / (nGrid - 1)), y: 0
    }));
    const bw = (max - min) / 20;
    const step = (max - min) / (nGrid - 1);
    const grid = new Array(nGrid);
    for (let i = 0; i < nGrid; i++) {
      const x = min + i * step;
      let y = 0;
      for (let j = 0; j < values.length; j++) {
        const z = (x - values[j]) / bw;
        y += Math.exp(-0.5 * z * z);
      }
      grid[i] = { x, y };
    }
    return grid;
  }

  function kdeToSvgPath(grid, min, max, maxY, svgW, svgH) {
    if (maxY <= 0 || !grid || !grid.length) return '';
    const scaleX = (x) => ((x - min) / (max - min)) * svgW;
    const scaleY = (y) => svgH - (y / maxY) * svgH * 0.9;
    let d = `M${scaleX(grid[0].x).toFixed(1)},${svgH}`;
    for (const p of grid) {
      d += ` L${scaleX(p.x).toFixed(1)},${scaleY(p.y).toFixed(1)}`;
    }
    d += ` L${scaleX(grid[grid.length - 1].x).toFixed(1)},${svgH} Z`;
    return d;
  }

  // -- Density helpers ------------------------------------------------------
  // The blue overlay is the GRAY curve, cut at the handles. It is NOT a fresh
  // KDE of the filtered rows: that re-smooths a truncated sample, so the
  // curve sagged inside the selection and bled roughly one bandwidth
  // (range/20) past both handles -- blue and gray disagreed exactly where the
  // user is reading the cut.
  //
  // Nothing is scaled, because there is nothing to scale: gray is already
  // "every row the OTHER filters leave" (_rebuildRangeDensity clears this
  // dim's own filter before measuring), and inside the cut this dim's filter
  // drops nothing. A per-cell survival ratio was tried here and is worse than
  // useless -- with 30-odd rows over 64 cells it is counts like 1/2 and 0/1,
  // and it drew a jagged blue curve under a smooth gray one.

  // Cut a grid at [lo, hi], interpolating the two end points so the path
  // closes with a vertical edge exactly under each handle.
  function clipGrid(grid, lo, hi) {
    const inside = grid.filter(p => p.x >= lo && p.x <= hi);
    if (!inside.length) return [];
    const at = (x) => {
      for (let i = 1; i < grid.length; i++) {
        if (grid[i].x >= x) {
          const a = grid[i - 1], b = grid[i];
          const t = b.x === a.x ? 0 : (x - a.x) / (b.x - a.x);
          return { x, y: a.y + (b.y - a.y) * t };
        }
      }
      return { x, y: grid[grid.length - 1].y };
    };
    const out = [];
    if (inside[0].x > lo) out.push(at(lo));
    out.push(...inside);
    if (inside[inside.length - 1].x < hi) out.push(at(hi));
    return out;
  }

  function fmtCount(n) {
    if (n >= 1e6) return (n / 1e6).toFixed(1) + 'M';
    if (n >= 1e3) return (n / 1e3).toFixed(1) + 'K';
    return String(n);
  }

  function fmtNum(v) {
    if (Math.abs(v) >= 1e6) return (v / 1e6).toFixed(1) + 'M';
    if (Math.abs(v) >= 1e3) return (v / 1e3).toFixed(1) + 'K';
    return Number.isInteger(v) ? String(v) : v.toFixed(1);
  }

  function fmtDate(days) {
    return new Date(days * 86400000).toISOString().slice(0, 10);
  }

  // Coerce a raw date value (ISO string, or null/"" for a missing value) to
  // epoch-days. Missing/unparseable values become NaN — crossfilter excludes
  // NaN from a dimension's sorted index, so they neither poison the
  // bottom(1)/top(1) bounds nor match any range predicate (mirroring R, where
  // `col >= lo & col <= hi` drops NA rows).
  function toEpochDay(v) {
    if (v == null || v === '') return NaN;
    // Date columns ship as epoch-day numbers (R: as.integer(unclass(date)));
    // ISO strings still parse below so old cached payloads and POSIXct
    // datetimes keep working.
    if (typeof v === 'number') return v;
    const t = new Date(v).getTime();
    return Number.isNaN(t) ? NaN : t / 86400000;
  }

  // A pool's name until someone types one, recomputed whenever its members
  // change. `levels` is [{value, n}] in level order; members are named in
  // that order too, whatever order they were picked in. Mirrored in R by
  // crossfilter_pool_default_name().
  function poolDefaultName(members, levels) {
    if (!members.length) return 'New pool';
    const all = levels.map(l => l.value);
    const picked = new Set(members);
    if (all.length && all.length === picked.size &&
        all.every(v => picked.has(v))) {
      return 'All patients';
    }
    const ordered = all.filter(v => picked.has(v))
      .concat(members.filter(v => !all.includes(v)));
    if (ordered.length >= 2) {
      const words = ordered.map(m => String(m).trim().split(/\s+/));
      const shared = [];
      for (let i = 0; i < words[0].length; i++) {
        const w = words[0][i];
        if (!words.every(ws => ws[i] === w)) break;
        shared.push(w);
      }
      if (shared.length) return 'All ' + shared.join(' ');
    }
    return ordered.join(' + ');
  }

  // A default name must not take another column's name: the board splits on
  // these names, and two columns called "Placebo" cannot both exist.
  function uniqueName(base, taken) {
    if (!taken.has(base)) return base;
    for (let k = 2; ; k++) {
      const next = base + ' ' + k;
      if (!taken.has(next)) return next;
    }
  }

  function el(tag, cls, html) {
    const e = document.createElement(tag);
    if (cls) e.className = cls;
    if (html !== undefined) e.innerHTML = html;
    return e;
  }

  // The design system's light-card tooltip (Blockr.tooltip, blockr.ui's
  // controls_dep()), for icon-only buttons and names that need one. No
  // native `title` anywhere in the block.
  function tip(node, content) {
    window.Blockr.tooltip.set(node, content);
  }

  // A column's label on hover; the name is already on screen. No label, or
  // one equal to the name, gives no tooltip.
  function setDimTitle(node, dim, label) {
    if (label && label !== dim) tip(node, label);
  }

  // =========================================================================
  // CrossfilterBlock
  // =========================================================================

  class CrossfilterBlock {
    constructor(root) {
      this.el = root;
      this.instances = {};
      this.dimensions = {};
      this.groups = {};
      this.dimSource = {};
      this.dimChild = {};
      this.parentKey = null;
      this.parentTable = null;
      this.parentN = null;       // subjects in the parent table, from R
      this.subjectUnit = 'rows'; // what the header calls one of them
      this._keptKeys = null;     // subject keys every filter keeps, or null
      this.childFkCols = {};
      this.keyDims = {};
      this.filters = {};
      this.levels = {};       // col -> [label, ...] (dictionary-encoded cols)
      this._levelIndex = {};  // col -> Map(label -> code), built lazily
      this.columnInfo = {};
      this.allColumns = {};   // full catalog for search
      this.activeDims = {};   // table -> [dim, ...]
      this.featured = [];     // [{table, dim, type, label}, ...] shelf chips
      this.pinnable = [];     // featured dims the pinned picker may offer
      this.pinned = null;     // dim held in the always-open card, or null
      this.subgroup = null;   // second split under the group, or null
      // Group definitions under the Group by field. `this.groups` is taken:
      // it holds the crossfilter groups the cards count with.
      this.groupDefs = {};    // col -> {columns} as R last sent them
      // One pools band per split, the group's and the subgroup's.
      const split = (role) => ({
        role,
        col: null,      // the column the band was built for
        levels: [],     // [{value, n}] for `col`
        edit: null,     // working definition for `col`
        local: false,   // the user has edited `edit` this session
        open: false,    // pools editor open, per session
        hiddenAt: new Map(), // hidden level -> id of the column it sat after
        selects: [],    // Blockr.Select handles inside the band
        el: null        // the band's container
      });
      this._splits = { group: split('group'), subgroup: split('subgroup') };
      this._subShown = false; // "+ Subgroup" clicked, nothing picked yet
      this._gPoolSeq = 0;
      this.measure = '.count';
      this.aggFunc = 'sum';
      this.panels = {};
      this._searchRows = [];  // [{row, tbl, dim}] for the open search list
      this._submitTimer = null;
      this._buildDOM();
    }

    // -- DOM skeleton -------------------------------------------------------

    _buildDOM() {
      this.el.innerHTML = '';

      // Group by: the board's split, on a field of its own above everything
      // else. It is a select because the group is exactly one and the filters
      // are many of many, and a select is the one-of-many control every user
      // already reads -- a mark invented on a pill has to teach itself first.
      // Its options are the columns crossfilter_pinnable() allows, so a column
      // that cannot split the board is absent rather than present-and-refusing.
      // The control itself is Blockr.Select, the same component the dm table
      // pickers and the dplyr blocks mount (blockr.dplyr::blockr_select_dep(),
      // pulled in by crossfilter_deps()).
      this.groupFieldEl = el('div', 'jscf-group-field');
      this.groupFieldEl.style.display = 'none';
      const groupLabel = el('label', 'blockr-label', 'Group by');
      this.groupFieldEl.appendChild(groupLabel);
      this.groupHostEl = el('div', 'jscf-group-select');
      this.groupFieldEl.appendChild(this.groupHostEl);
      // The pools of the pinned column, folded under the select. Inside the
      // field, so it hides with it.
      const bandEl = () => {
        const b = el('div', 'jscf-groups');
        b.style.display = 'none';
        return b;
      };
      this._splits.group.el = bandEl();
      this.groupFieldEl.appendChild(this._splits.group.el);
      // Show groups and Subgroup are rare, so at rest they are one line of "+"
      // links under the group. A link goes away while its section is on the
      // block, and comes back when the section is removed.
      this.splitAddEl = el('div', 'blockr-add-row jscf-split-add');
      this.groupFieldEl.appendChild(this.splitAddEl);
      // The second split, under the group's own settings, with pools of its
      // own and its own "+ Show subgroups" link. Same field, so it hides with it:
      // there is no subgroup without a group.
      this.subgroupFieldEl = el('div', 'jscf-subgroup-field');
      this.subgroupFieldEl.style.display = 'none';
      this.subgroupPickEl = el('div', 'jscf-subgroup-pick');
      this.subgroupFieldEl.appendChild(this.subgroupPickEl);
      this._splits.subgroup.el = bandEl();
      this.subgroupFieldEl.appendChild(this._splits.subgroup.el);
      this.subgroupAddEl = el('div', 'blockr-add-row jscf-split-add');
      this.subgroupFieldEl.appendChild(this.subgroupAddEl);
      this.groupFieldEl.appendChild(this.subgroupFieldEl);
      this.el.appendChild(this.groupFieldEl);

      // The filter section's header row: its name on the left, then the
      // subject count, which becomes Reset all while a filter is on, and the
      // gear. Reset lives here rather than under the panels because it
      // doubles as the "you are looking at a subset" signal -- below the
      // panels it was off screen on any board with more than two active
      // dimensions.
      const gearHeader = el('div', 'jscf-gear-header');
      // "Filter by" leads the row, the way "Group by" leads the field above.
      // The count, the reset and the gear all report or change filter state,
      // so they belong on this line and not on one of their own between the
      // two sections, where the reset sat above the filters it resets.
      gearHeader.appendChild(el('label', 'blockr-label', 'Filter by'));
      gearHeader.appendChild(el('span', 'jscf-topbar-spacer'));
      this.statusEl = el('span', 'jscf-status-text');
      gearHeader.appendChild(this.statusEl);

      // Reset all: the design system's 26px main button, the icon and the
      // count it would lift, "179 of 306 patients". It takes the status
      // text's place while a filter is on and is not there otherwise, so the
      // count and the way back to all of it are one thing. The tooltip names
      // the clause it would undo.
      this.resetBtn = el('button', 'jscf-reset-btn', ICON_RESET);
      this.resetBtn.type = 'button';
      this.resetLabelEl = el('span', 'jscf-reset-label');
      this.resetBtn.appendChild(this.resetLabelEl);
      this.resetBtn.style.display = 'none';
      this.resetBtn.addEventListener('click', () => this._resetAllFilters());
      tip(this.resetBtn, () => {
        const clause = this._filterClause();
        const label = this.resetLabelEl.textContent;
        return clause ? `${label}. Show all, clearing ${clause}` : null;
      });
      gearHeader.appendChild(this.resetBtn);

      this.gearBtn = el('button', 'blockr-gear-btn', window.Blockr.icons.gear);
      this.gearBtn.type = 'button';
      gearHeader.appendChild(this.gearBtn);

      // ---- The gear tray and the column menu, one job each ----------------
      // The gear's tray is the board's settings: which columns are held up
      // front, what a bar is long by, and how that measure aggregates. None
      // of it is needed to READ the block, which is what lets simplified mode
      // be nothing but dock's `display: none` on the gear -- the same rule it
      // uses on every other block, with no mode flag in here at all.
      //
      // The column menu opens from the pill row, where a reader can always
      // reach it. Adding a filter card is a question about this session;
      // naming the vocabulary is a decision for every reader of the board.
      // Two questions, two surfaces.
      //
      // The tray is the design system's (Blockr.gearTray): in flow under the
      // header row, closed by the gear and Escape only.
      this.settingsEl = el('div',
        'blockr-settings blockr-settings--beak jscf-settings');
      const grid = el('div', 'blockr-settings__grid');

      // Shown up front, as an ordinary multi-select of columns rather than a
      // chip shelf beside a search box: it has to be able to name a column
      // that no list on screen happens to be showing. Blockr.Select is what
      // every other block mounts for a set of columns, tag drag included, so
      // the pill order is edited the way column order is edited everywhere.
      const featuredField = el('div',
        'blockr-settings__field blockr-settings__field--full');
      featuredField.appendChild(el('label', 'blockr-label', 'Shown up front'));
      this.featuredHostEl = el('div', 'jscf-featured-select');
      featuredField.appendChild(this.featuredHostEl);
      featuredField.appendChild(el('p', 'jscf-settings-hint',
        'The tags above the cards, in this order. Drag a tag to reorder.'));
      grid.appendChild(featuredField);

      // What a bar is long by. A Select, mounted once the measures are known
      // (_updateMeasureUI); hidden when the data has none.
      this._measureSection = el('div', 'blockr-settings__field');
      this._measureSection.style.display = 'none';
      this._measureSection.appendChild(el('label', 'blockr-label', 'Measure'));
      this._measureHostEl = el('div', 'jscf-measure-select');
      this._measureSection.appendChild(this._measureHostEl);
      grid.appendChild(this._measureSection);

      // Two fixed values: a segmented control.
      this._aggSection = el('div', 'blockr-settings__field');
      this._aggSection.style.display = 'none';
      this._aggSection.appendChild(el('label', 'blockr-label', 'Aggregation'));
      this._aggControl = window.Blockr.segmented(
        [{ value: 'sum', label: 'Sum' }, { value: 'mean', label: 'Mean' }],
        this.aggFunc,
        (val) => this._setAggFunc(val),
        { label: 'Aggregation' });
      this._aggSection.appendChild(this._aggControl.el);
      grid.appendChild(this._aggSection);

      this.settingsEl.appendChild(grid);
      window.Blockr.gearTray(this.settingsEl, this.gearBtn,
        { label: 'Crossfilter settings' });

      // The column menu: every column of every table, with its type, opened
      // from "+ More filters". A menu on the design system's floating
      // surface, portalled to <body> while open and placed by Blockr.place.
      this.popoverEl = el('div', 'jscf-menu');
      this.popoverEl.setAttribute('role', 'dialog');
      this.popoverEl.setAttribute('aria-label', 'Filter on a column');
      this.searchInput = el('input', 'jscf-popover-search');
      this.searchInput.type = 'text';
      this.searchInput.placeholder = 'Search columns\u2026';
      this.searchInput.autocomplete = 'off';
      this.searchInput.spellcheck = false;
      this.searchInput.addEventListener('input', () => this._onSearchInput());
      this.searchInput.addEventListener('keydown', (e) => this._onMenuKey(e));
      this.popoverEl.appendChild(this.searchInput);

      this.searchResultsEl = el('div', 'jscf-popover-results');
      this.popoverEl.appendChild(this.searchResultsEl);

      // An outside click closes the menu. The menu and the control that
      // opens it are one thing for this purpose.
      document.addEventListener('click', (e) => {
        if (this._popoverOpen &&
            !this.popoverEl.contains(e.target) &&
            !(this.addBtn && this.addBtn.contains(e.target))) {
          this._closePopover();
        }
      });

      // The pill row comes first, above every card: it is the vocabulary the
      // cards below are drawn from. It carries its own label, so the block
      // reads as two named sections -- what the board is split by, then what
      // it is cut down by -- rather than a field followed by loose chrome.
      // The group's card is an ordinary card in the list, in the place it was
      // added: grouping by a column and filtering on it are separate facts.
      this.shelfSectionEl = el('div', 'jscf-filter-section');
      this.shelfSectionEl.appendChild(gearHeader);
      // The tray opens under the row its gear sits on, above the pills.
      this.shelfSectionEl.appendChild(this.settingsEl);
      this.shelfEl = el('div', 'jscf-shelf');
      this.shelfSectionEl.appendChild(this.shelfEl);
      this.el.appendChild(this.shelfSectionEl);

      // Filter panels container
      this.panelsEl = el('div', 'jscf-panels');
      this.el.appendChild(this.panelsEl);

      // The data source's note (R: `blockr_note` on a table), one grey line
      // under the cards. Below them on purpose: it is there for whoever looks
      // for it and scrolls away once several cards are open.
      this.noteEl = el('div', 'jscf-note');
      this.noteEl.style.display = 'none';
      this.el.appendChild(this.noteEl);
    }

    _togglePopover() {
      this._popoverOpen ? this._closePopover() : this._openPopover();
    }
    _openPopover() {
      document.body.appendChild(this.popoverEl);
      this._popoverOpen = true;
      if (this.addBtn) {
        this.addBtn.classList.add('jscf-shelf-add--open');
        this.addBtn.setAttribute('aria-expanded', 'true');
      }
      this.searchInput.value = '';
      this._onSearchInput();
      this._placeMenu = window.Blockr.place(this.popoverEl, this.addBtn,
        { width: { min: 260, max: 320 } });
      this.searchInput.focus();
    }
    _closePopover({ refocus = false } = {}) {
      if (!this._popoverOpen) return;
      this._popoverOpen = false;
      if (this._placeMenu) {
        this._placeMenu.stop();
        this._placeMenu = null;
      }
      this.popoverEl.remove();
      if (this.addBtn) {
        this.addBtn.classList.remove('jscf-shelf-add--open');
        this.addBtn.setAttribute('aria-expanded', 'false');
        if (refocus) this.addBtn.focus();
      }
    }

    // Arrows move the keyboard row, Enter toggles it, Escape closes. Focus
    // stays in the filter box, as in every menu.
    _onMenuKey(e) {
      const rows = this._searchRows || [];
      if (e.key === 'Escape') {
        e.preventDefault();
        e.stopPropagation();
        this._closePopover({ refocus: true });
        return;
      }
      if (!rows.length) return;
      if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
        e.preventDefault();
        const step = e.key === 'ArrowDown' ? 1 : -1;
        const cur = this._menuIndex == null ? -1 : this._menuIndex;
        this._setMenuIndex(
          Math.max(0, Math.min(rows.length - 1, cur + step)));
      } else if (e.key === 'Enter' && this._menuIndex != null) {
        e.preventDefault();
        rows[this._menuIndex].row.click();
      }
    }
    _setMenuIndex(i) {
      const rows = this._searchRows || [];
      if (this._menuIndex != null && rows[this._menuIndex]) {
        rows[this._menuIndex].row.classList.remove(
          'jscf-search-item--highlighted');
      }
      this._menuIndex = i;
      if (i == null || !rows[i]) return;
      rows[i].row.classList.add('jscf-search-item--highlighted');
      rows[i].row.scrollIntoView({ block: 'nearest' });
    }

    // -- Search bar ----------------------------------------------------------

    _onSearchInput() {
      const query = this.searchInput.value.trim().toLowerCase();

      // Every column, active ones included. They used to be filtered out,
      // which meant searching for a column you already filter on found
      // nothing and left no way to pin it. An active row says so and toggles
      // its card off, so this list is also the one place that removes a
      // filter you cannot see.
      const results = [];
      for (const [tbl, info] of Object.entries(this.allColumns)) {
        const activeDims = asArray(this.activeDims[tbl]);
        const dims = asArray(info.dimensions);
        const rangeDims = asArray(info.range_dimensions);
        const dateDims = asArray(info.date_dimensions);
        const allDims = [...dims, ...rangeDims, ...dateDims];
        for (const dim of allDims) {
          const label = (info.labels && info.labels[dim]) || '';
          const matchName = dim.toLowerCase().includes(query);
          const matchLabel = label.toLowerCase().includes(query);
          if (query === '' || matchName || matchLabel) {
            let type = 'categorical';
            if (rangeDims.includes(dim)) type = 'range';
            if (dateDims.includes(dim)) type = 'date';
            results.push({ tbl, dim, label, type,
                           active: activeDims.includes(dim) });
          }
        }
      }

      this.searchResultsEl.innerHTML = '';
      this._searchRows = [];
      this._menuIndex = null;
      if (results.length === 0) {
        this.searchResultsEl.appendChild(el('div', 'jscf-search-empty',
          'No matching columns'));
      } else {
        const grouped = {};
        for (const r of results) {
          (grouped[r.tbl] = grouped[r.tbl] || []).push(r);
        }
        const multiTable = Object.keys(grouped).length > 1;
        for (const [tbl, items] of Object.entries(grouped)) {
          if (multiTable) {
            this.searchResultsEl.appendChild(
              el('div', 'jscf-search-group-header', tbl)
            );
          }
          for (const item of items) {
            const row = el('div', 'jscf-search-item');
            if (item.active) row.classList.add('jscf-search-item--active');
            // A column with a card is a multi pick: a 14px tick in the accent,
            // in a slot every row keeps. A click toggles it.
            row.appendChild(el('span', 'jscf-search-item-check', window.Blockr.icons.confirm));

            // The name, then the label as muted meta, cut first.
            const nameEl = el('span', 'jscf-search-item-name', item.dim);
            if (item.label && item.label !== item.dim) {
              nameEl.appendChild(el('span', 'jscf-search-item-label', item.label));
            }
            row.appendChild(nameEl);

            // The type as a neutral badge: a type is read, not acted on.
            const badgeText = item.type === 'date' ? 'Date'
              : item.type === 'range' ? 'Numeric' : 'Categorical';
            row.appendChild(el('span', 'jscf-search-item-badge', badgeText));

            const entry = { row, tbl: item.tbl, dim: item.dim };
            this._searchRows.push(entry);
            this._setSearchRowState(entry, item.active);

            row.addEventListener('click', () => {
              // Read the state off the row, not off `item`: `item` is what was
              // true when this list was built, which a click ago.
              const add = !entry.row.classList.contains(
                'jscf-search-item--active');
              add ? this._addDimension(item.tbl, item.dim)
                  : this._removeDimension(item.tbl, item.dim);
              // Optimistic. R's answer is a round trip away and the row has to
              // answer the click now; _syncSearchRows() reconciles when it
              // lands. The band stays open either way -- picking three columns
              // is three clicks in one place.
              this._setSearchRowState(entry, add);
            });
            this.searchResultsEl.appendChild(row);
          }
        }
      }
    }

    _setSearchRowState(entry, active) {
      entry.row.classList.toggle('jscf-search-item--active', active);
      entry.row.setAttribute('aria-selected', String(active));
    }

    // The authoritative pass, run when R answers: state comes from
    // `active_dims`. It also catches a card closed from the card itself while
    // this list is open. In place rather than a rebuild -- rebuilding the
    // list would throw away the reader's scroll position.
    _syncSearchRows() {
      if (!this._searchRows) return;
      for (const entry of this._searchRows) {
        this._setSearchRowState(
          entry, asArray(this.activeDims[entry.tbl]).includes(entry.dim));
      }
    }

    // Shown up front, the board's vocabulary, mounted as the design system's
    // multi-select (blockr.dplyr::blockr_select_dep(), the same component the
    // Group by field above uses). It was a chip shelf next to the column
    // search, which put a board-wide edit one click from a reader's control
    // and could only offer columns the search happened to be listing. The
    // group is not chosen here: it has its own field at the top of the block,
    // and a second place to set it is a second thing to keep in sync.
    _renderFeaturedField() {
      if (!this.featuredHostEl) return;

      // Every column the block could hold up front, by name. Names are
      // unique across tables here on purpose: `featured` is a vocabulary of
      // column NAMES, resolved per table by crossfilter_featured_dims().
      const seen = new Set();
      const options = [];
      for (const info of Object.values(this.allColumns)) {
        const labels = info.labels || {};
        const dims = [
          ...asArray(info.dimensions),
          ...asArray(info.range_dimensions),
          ...asArray(info.date_dimensions)
        ];
        for (const dim of dims) {
          if (seen.has(dim)) continue;
          seen.add(dim);
          options.push({ value: dim, label: labels[dim] || '' });
        }
      }
      options.sort((a, b) => a.value.localeCompare(b.value));

      // Selected order is the PILL order, which is what the tag drag edits,
      // so it comes from `featured` and not from the sorted options.
      const selected = this.featured.map(f => f.dim);

      if (this._featuredSelect) {
        this._featuredSelect.setOptions(options, selected);
        return;
      }

      const Select = window.Blockr && window.Blockr.Select;
      if (!Select) {
        throw new Error(
          '[js-crossfilter] Blockr.Select is missing: the block UI has to ship ' +
          'blockr.dplyr::blockr_select_dep()');
      }
      this._featuredSelect = Select.multi(this.featuredHostEl, {
        options,
        selected,
        placeholder: 'Add a column\u2026',
        onChange: (values) => this._setFeatured(values)
      });
      this._featuredSelect.el.classList.add('blockr-select--bordered');
    }

    _setFeatured(dims) {
      const id = this.el.id;
      const nsBase = id.replace(/-crossfilter_input$/, '');
      Shiny.setInputValue(nsBase + '-set_featured', dims, { priority: 'event' });
    }

    _setMeasure(val) {
      const id = this.el.id;
      const nsBase = id.replace(/-crossfilter_input$/, '');
      Shiny.setInputValue(nsBase + '-measure_switch', val, { priority: 'event' });
    }

    _setAggFunc(val) {
      const id = this.el.id;
      const nsBase = id.replace(/-crossfilter_input$/, '');
      Shiny.setInputValue(nsBase + '-agg_func_switch', val, { priority: 'event' });
    }

    _updateMeasureUI() {
      // Select shows an option's value, so the options are the names a reader
      // knows ("Count", the column, `table.column` where there are several
      // tables), mapped to the measure keys R takes.
      const multiTable = Object.keys(this.allColumns).length > 1;
      const keyOf = { Count: '.count' };
      for (const [tbl, info] of Object.entries(this.allColumns)) {
        for (const m of asArray(info.measures)) {
          keyOf[multiTable ? tbl + '.' + m : m] = tbl + '.' + m;
        }
      }
      const names = Object.keys(keyOf);
      const nameOf = (key) => names.find(n => keyOf[n] === key) || 'Count';

      this._measureSection.style.display = names.length > 1 ? '' : 'none';
      if (names.length > 1) {
        if (this._measureSelect) {
          this._measureSelect.setOptions(names, nameOf(this.measure));
        } else {
          this._measureSelect = this._select().single(this._measureHostEl, {
            options: names,
            selected: nameOf(this.measure),
            onChange: (name) => {
              const key = keyOf[name];
              if (key && key !== this.measure) this._setMeasure(key);
            }
          });
          this._measureSelect.el.classList.add('blockr-select--bordered');
        }
      }
      this._aggControl.set(this.aggFunc);
      this._aggSection.style.display = (this.measure !== '.count') ? '' : 'none';
    }

    _addDimension(tbl, dim) {
      // Tell R to add this dimension
      const id = this.el.id;
      const nsBase = id.replace(/-crossfilter_input$/, '');
      Shiny.setInputValue(nsBase + '-add_filter', { table: tbl, dim: dim },
        { priority: 'event' });
    }

    _removeDimension(tbl, dim) {
      const id = this.el.id;
      const nsBase = id.replace(/-crossfilter_input$/, '');
      Shiny.setInputValue(nsBase + '-remove_filter', { table: tbl, dim: dim },
        { priority: 'event' });
    }

    _setSubgroup(dim) {
      const nsBase = this.el.id.replace(/-crossfilter_input$/, '');
      Shiny.setInputValue(nsBase + '-set_subgroup', dim, { priority: 'event' });
    }

    _setPinned(dim) {
      const id = this.el.id;
      const nsBase = id.replace(/-crossfilter_input$/, '');
      Shiny.setInputValue(nsBase + '-set_pinned', dim, { priority: 'event' });
    }

    // -- Featured pill row --------------------------------------------------
    // One pill per featured column, and one job: click it to open or close
    // that column's filter card. The group used to be a radio inside the same
    // pill, which made every pill answer a one-of-many and a many-of-many
    // question at once; it is a field of its own now. The pill's fill says a
    // card is open, and a column actually cutting rows takes a 2px accent
    // edge, which comes out of the pill's existing padding: no width changes
    // when a filter is applied, so picking a level can never re-wrap the row
    // underneath the pointer.
    _renderShelf() {
      if (!this.shelfEl) return;
      this.shelfEl.innerHTML = '';
      this._pillEls = {};

      // The row is drawn even with no vocabulary: it carries the way into the
      // column search, which is the only one there is once the gear is hidden.
      // A board written without `featured =` could otherwise not filter on
      // anything at all in simplified mode.

      for (const f of this.featured) {
        if (!f || !f.dim) continue;
        const pill = el('span', 'jscf-pill');
        pill.dataset.dim = f.dim;

        // The name opens and closes the column's own filter card, the group's
        // included: a column can group the board without filtering it.
        const name = el('button', 'jscf-pill-name', f.dim);
        name.type = 'button';
        setDimTitle(name, f.dim, f.label);
        name.addEventListener('click', () => {
          asArray(this.activeDims[f.table]).includes(f.dim)
            ? this._removeDimension(f.table, f.dim)
            : this._addDimension(f.table, f.dim);
        });
        pill.appendChild(name);

        this._pillEls[f.dim] = { pill, table: f.table };
        this.shelfEl.appendChild(pill);
      }

      // The way in, in the row it acts on, and the same in every mode. It is
      // the design system's "+" add link, like "+ Pool groups" and
      // "+ Subgroup" above: an add control, so it must not look like a
      // pill, whose solid tint (`.jscf-pill--filtering`) says a column is
      // cutting rows.
      const plus = window.Blockr.icons.plus;
      this.addBtn = el('button', 'blockr-add-link jscf-shelf-add',
        `<span class="blockr-add-icon">${plus}</span> More filters`);
      this.addBtn.type = 'button';
      this.addBtn.setAttribute('aria-haspopup', 'dialog');
      this.addBtn.setAttribute('aria-expanded', String(!!this._popoverOpen));
      this.addBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._togglePopover();
      });
      if (this._popoverOpen) this.addBtn.classList.add('jscf-shelf-add--open');
      this.shelfEl.appendChild(this.addBtn);

      this._syncShelfState();
      // An open menu hangs off the link it was opened from, which this
      // rebuild just replaced.
      if (this._popoverOpen) {
        this._placeMenu.stop();
        this._placeMenu = window.Blockr.place(this.popoverEl, this.addBtn,
          { width: { min: 260, max: 320 } });
      }
    }

    // Whether a column is cutting rows, as opposed to merely having a card:
    // `filters` holds an array for a categorical dim and {min,max} for a
    // range one, and an empty array is a card with nothing picked.
    _isFiltering(dim) {
      const v = this.filters[dim];
      if (!v) return false;
      return Array.isArray(v) ? v.length > 0 : v.min !== undefined;
    }

    // Two states, written as classes on pills that already exist. A filter
    // click must not rebuild the row: the pointer is on it.
    _syncShelfState() {
      for (const [dim, card] of Object.entries(this.panels)) {
        const btn = card.querySelector('.dm-cf-reset-btn');
        if (btn) btn.disabled = !this._isFiltering(dim);
      }
      if (!this._pillEls) return;
      for (const [dim, { pill, table }] of Object.entries(this._pillEls)) {
        pill.classList.toggle('jscf-pill--open',
          asArray(this.activeDims[table]).includes(dim));
        pill.classList.toggle('jscf-pill--filtering', this._isFiltering(dim));
      }
    }

    // -- Group by field -----------------------------------------------------
    // Mounted on the first pin payload, updated in place after that. The field
    // is absent, not empty, where nothing may group the board: an empty select
    // is a promise the block cannot keep.
    _renderGroupField() {
      if (!this.groupFieldEl) return;

      if (!this.pinnable.length) {
        this.groupFieldEl.style.display = 'none';
        return;
      }
      this.groupFieldEl.style.display = '';

      const labels = {};
      for (const f of this.featured) labels[f.dim] = f.label || '';
      const options = this.pinnable.map(
        dim => ({ value: dim, label: labels[dim] || '' }));
      const selected = this.pinned || '';

      if (this._groupSelect) {
        this._groupSelect.setOptions(options, selected);
        return;
      }

      const Select = this._select();
      // allowEmpty carries the ungrouped state a board starts in (`pinned` is
      // NULL by default). It is not offered back once a group exists: on a
      // board whose exhibits bind a stamped column by name, dropping the group
      // takes the column away with it.
      this._groupSelect = Select.single(this.groupHostEl, {
        options,
        selected,
        allowEmpty: true,
        placeholder: 'Not grouped',
        onChange: (value) => {
          if (value !== this.pinned) this._setPinned(value || '');
        }
      });
      this._groupSelect.el.classList.add('blockr-select--bordered');
    }

    // -- Subgroup by field ---------------------------------------------------
    // On the block only once someone asks for it ("+ Subgroup") or it is set.
    // The x and the "(none)" option both clear it and bring the link back.
    // "(none)" is the sentinel the picker block uses for an optional role.
    // Its pools are edited exactly as the group's, in a band of its own under
    // the select. The swap button trades the two columns; each takes its
    // pools along, because `groups` is keyed by column.
    _renderSubgroupField() {
      if (!this.subgroupFieldEl) return;
      if (this._subgroupSelect) {
        try { this._subgroupSelect.destroy(); } catch (_) {}
        this._subgroupSelect = null;
      }
      this.subgroupPickEl.innerHTML = '';
      const shown = !!this.pinned && (!!this.subgroup || this._subShown);
      this.subgroupFieldEl.style.display = shown ? '' : 'none';
      this._renderSplitAdd();
      if (!shown) return;

      const icons = window.Blockr.icons;
      const head = el('div', 'jscf-section-head');
      head.appendChild(el('label', 'blockr-label', 'Subgroup by'));
      head.appendChild(el('span', 'jscf-topbar-spacer'));
      if (this.subgroup) {
        const swap = el('button', 'jscf-section-swap', ICON_SWAP);
        swap.type = 'button';
        swap.setAttribute('aria-label', 'Switch group and subgroup');
        tip(swap, 'Switch group and subgroup');
        swap.addEventListener('click', () => this._swapSplit());
        head.appendChild(swap);
      }
      const rm = el('button', 'blockr-row-remove jscf-section-remove', icons.remove);
      rm.type = 'button';
      rm.setAttribute('aria-label', 'Remove the subgroup');
      tip(rm, 'Remove the subgroup');
      const clear = () => {
        this._subShown = false;
        if (this.subgroup) {
          this.subgroup = null;
          this._setSubgroup('');
        }
        this._renderSubgroupField();
      };
      rm.addEventListener('click', clear);
      head.appendChild(rm);
      this.subgroupPickEl.appendChild(head);

      const host = el('div', 'jscf-subgroup-select');
      this.subgroupPickEl.appendChild(host);
      const labels = {};
      for (const f of this.featured) labels[f.dim] = f.label || '';
      const NONE = '(none)';
      const options = [{ value: NONE, label: '' }].concat(
        this.pinnable.filter(dim => dim !== this.pinned)
          .map(dim => ({ value: dim, label: labels[dim] || '' })));
      this._subgroupSelect = this._select().single(host, {
        options,
        selected: this.subgroup || '',
        allowEmpty: true,
        placeholder: 'Pick a column',
        onChange: (value) => {
          if (value === NONE) {
            // After the select has finished its own pick handling: clear()
            // destroys it.
            setTimeout(clear, 0);
          } else if (value && value !== this.subgroup) {
            this.subgroup = value;
            this._setSubgroup(value);
          }
        }
      });
      this._subgroupSelect.el.classList.add('blockr-select--bordered');
    }

    // The "+" links for the sections that are not on the block: Show groups
    // and Subgroup under the group, Show subgroups under the subgroup.
    _renderSplitAdd() {
      const icons = window.Blockr.icons;
      // Each link says what it adds, so none has a tooltip.
      const link = (text, onClick) => {
        const a = el('span', 'blockr-add-link',
          `<span class="blockr-add-icon">${icons.plus}</span> ${text}`);
        a.setAttribute('role', 'button');
        a.tabIndex = 0;
        a.addEventListener('click', onClick);
        a.addEventListener('keydown', (e) => {
          if (e.key === 'Enter' || e.key === ' ') {
            e.preventDefault();
            onClick();
          }
        });
        return a;
      };
      const fill = (row, links) => {
        if (!row) return;
        row.innerHTML = '';
        row.style.display = links.length ? '' : 'none';
        for (const a of links) row.appendChild(a);
      };
      // Each link names the section it opens. The subgroup's says whose
      // levels it shows: the group's section sits right above it.
      const poolsLink = (s) => link(s.role === 'subgroup' ? 'Show subgroups' : 'Show groups',
        () => {
          s.open = true;
          this._renderPoolsBand(s);
        });

      const g = this._splits.group;
      const s = this._splits.subgroup;
      const groupLinks = [];
      if (this.pinned) {
        if (!this._poolsShown(g) && g.levels.length) groupLinks.push(poolsLink(g));
        if (!this.subgroup && !this._subShown) {
          groupLinks.push(link('Subgroup', () => {
            this._subShown = true;
            this._renderSubgroupField();
            const ctrl = this.subgroupFieldEl.querySelector('.blockr-select__control');
            if (ctrl) setTimeout(() => ctrl.click(), 0);
          }));
        }
      }
      fill(this.splitAddEl, groupLinks);
      fill(this.subgroupAddEl,
        this.pinned && this.subgroup && !this._poolsShown(s) && s.levels.length
          ? [poolsLink(s)] : []);
    }

    _swapSplit() {
      if (!this.pinned || !this.subgroup) return;
      const nsBase = this.el.id.replace(/-crossfilter_input$/, '');
      Shiny.setInputValue(nsBase + '-swap_split', Date.now(), { priority: 'event' });
    }

    // -- Pools bands --------------------------------------------------------
    // A split column's levels, regrouped: which levels keep a column of their
    // own, in what order, and which pools of levels get one besides. The
    // group and the subgroup each have a band, one `split` object each (see
    // the constructor). State in R is `groups` (see new_crossfilter_block()),
    // keyed by column; a band edits the entry for its column and sends it
    // whole on every change.
    //
    // The client owns the definition while it is being edited. R never
    // answers `set_groups`, and a data payload for the same column does not
    // overwrite a local edit: R handing the client its own edit back redrew
    // the band under the user and dropped the edit in progress.

    _ingestGroups(msg) {
      let defs = msg.groups;
      if (!defs || Array.isArray(defs) || typeof defs !== 'object') defs = {};
      const levelsOf = (x) => asArray(x).map(l => ({
        value: String(l.value),
        n: Number(l.n) || 0
      }));
      const next = [
        [this._splits.group, this.pinned, levelsOf(msg.pinned_levels)],
        [this._splits.subgroup, this.pinned ? this.subgroup : null,
          levelsOf(msg.subgroup_levels)]
      ];
      // Columns whose band holds a local edit keep it over R's copy.
      const keep = new Map();
      const rebuilt = [];
      for (const [s, col, levels] of next) {
        s.levels = levels;
        if (col === s.col && s.local) {
          keep.set(col, this.groupDefs[col]);
          continue;
        }
        if (col !== s.col) s.open = false;
        s.col = col;
        s.local = false;
        rebuilt.push(s);
      }
      this.groupDefs = Object.assign({}, defs);
      for (const [col, d] of keep) {
        if (d) this.groupDefs[col] = d;
        else delete this.groupDefs[col];
      }
      for (const s of rebuilt) {
        s.edit = s.col ? this._groupDefFor(s) : null;
        this._renderPoolsBand(s);
      }
      this._renderSplitAdd();
    }

    // The working copy for a split's column: R's entry, or the untouched
    // default (every level on its own, in level order). One list of
    // columns in table order; a column with an empty name is one level on
    // its own, a named one is a pool. `open` is the pool's fold, per session.
    _groupDefFor(s) {
      const single = (v) => ({ id: ++this._gPoolSeq, name: '', members: [v], custom: false, open: true });
      const d = this.groupDefs[s.col];
      if (!d) return { columns: s.levels.map(l => single(l.value)) };
      return {
        columns: asArray(d.columns).map(p => ({
          id: ++this._gPoolSeq,
          name: p.name == null ? '' : String(p.name),
          members: asArray(p.members).map(String),
          custom: p.custom === true,
          open: true
        }))
      };
    }

    _isDefaultGroupDef(s, def) {
      const all = s.levels.map(l => l.value);
      return def.columns.length === all.length &&
        def.columns.every((c, i) => c.name === '' && c.members.length === 1 &&
          c.members[0] === all[i]);
    }

    // The columns the definition produces, in table order.
    _groupColumns(s) {
      return s.edit.columns.filter(c => c.members.length).map(c => (
        c.name === ''
          ? { name: c.members[0], title: c.members[0] }
          : { name: c.name, title: `${c.name}: ${c.members.join(', ')}` }));
    }

    _levelN(s, members) {
      const n = {};
      for (const l of s.levels) n[l.value] = l.n;
      return members.reduce((sum, m) => sum + (n[m] || 0), 0);
    }

    // The section is on the block while it is being edited or holds a
    // definition that changes anything.
    _poolsShown(s) {
      return !!s.col && !!s.edit && s.levels.length > 0 &&
        (s.open || !this._isDefaultGroupDef(s, s.edit));
    }

    _groupsEdited(s) {
      s.local = true;
      const def = s.edit;
      const out = {
        columns: def.columns.map(c => ({
          name: c.name,
          members: c.members.slice(),
          custom: !!c.custom
        }))
      };
      if (this._isDefaultGroupDef(s, def)) delete this.groupDefs[s.col];
      else this.groupDefs[s.col] = out;
      this._syncOverlap(s);
      const nsBase = this.el.id.replace(/-crossfilter_input$/, '');
      Shiny.setInputValue(nsBase + '-set_groups',
        Object.assign({ column: s.col }, out), { priority: 'event' });
    }

    // A split keeps at least one column: an edit that would leave none is
    // refused, and the menu row that would make it is disabled.
    _leavesNoColumn(columns) {
      return !columns.some(c => c.members.length);
    }

    // Each level present in the data -> the columns it sits in. Mirrors
    // group_definition() in blockr.pharma, which counts only levels present
    // in the data.
    _levelPlaces(s) {
      const present = new Set(s.levels.map(l => l.value));
      const places = new Map();
      for (const c of s.edit.columns) {
        for (const v of new Set(c.members)) {
          if (!present.has(v)) continue;
          if (!places.has(v)) places.set(v, []);
          places.get(v).push(c);
        }
      }
      return places;
    }

    // Levels that sit in two columns of a split: on their own and in a pool,
    // or in two pools.
    _overlapLevels(s) {
      if (!s.edit) return [];
      const twice = [];
      for (const [v, cols] of this._levelPlaces(s)) if (cols.length > 1) twice.push(v);
      return twice;
    }

    // A group's columns may overlap: composer pools the outer split. A
    // subgroup's may not, and a table would fail far from here. So the
    // subgroup's list takes the amber cue and says why, while it overlaps,
    // however it got there (an edit, the swap, a saved board).
    _syncOverlap(s) {
      if (s.role !== 'subgroup' || !s.el) return;
      const twice = this._overlapLevels(s);
      s.el.classList.toggle('jscf-groups--overlap', twice.length > 0);
      if (!s.warnEl) return;
      if (!twice.length) {
        s.warnEl.textContent = '';
        return;
      }
      const shown = twice.slice(0, 3).join(', ') +
        (twice.length > 3 ? ` and ${twice.length - 3} more` : '');
      const verb = twice.length === 1 ? 'is' : 'are each';
      s.warnEl.textContent = `${shown} ${verb} in two columns. Tables can ` +
        'pool the group but not the subgroup: use each level once, or ' +
        'switch group and subgroup.';
    }

    _select() {
      const Select = window.Blockr && window.Blockr.Select;
      if (!Select) {
        throw new Error(
          '[js-crossfilter] Blockr.Select is missing: the block UI has to ship ' +
          'blockr.dplyr::blockr_select_dep()');
      }
      return Select;
    }

    // Structural rebuild: fold, add or remove a pool, any edit of the list.
    // Never called while a menu anchored in the band is open; those rebuild
    // when they close.
    _renderPoolsBand(s) {
      if (!s.el) return;
      if (s.menu) { try { s.menu.close(); } catch (_) {} s.menu = null; }
      // An edit redraws the list; it stays scrolled where it was.
      const scrollTop = s.listEl && s.listEl.isConnected ? s.listEl.scrollTop : 0;
      s.listEl = null;
      s.el.innerHTML = '';
      if (!this._poolsShown(s)) {
        s.el.style.display = 'none';
        this._renderSplitAdd();
        return;
      }
      s.el.style.display = '';

      const icons = (window.Blockr && window.Blockr.icons) || {};

      // Open: the list, and a fold that closes it. Closed but set: the
      // columns as tags and an Edit link. Either way an x puts every level
      // back and takes the section off the block.
      const head = el('div', 'jscf-section-head');
      head.appendChild(el('label', 'blockr-label',
        s.role === 'subgroup' ? 'Show subgroups' : 'Show groups'));
      // Past 8 rows the list scrolls; the count says how long it is.
      if (s.open && s.edit.columns.length > 8) {
        head.appendChild(el('span', 'jscf-cols-count', `${s.edit.columns.length} columns`));
      }
      head.appendChild(el('span', 'jscf-topbar-spacer'));
      if (s.open) {
        const fold = el('button', 'jscf-groups-fold jscf-groups-fold--open', icons.chevron || '');
        fold.type = 'button';
        fold.setAttribute('aria-label', 'Fold the columns');
        tip(fold, 'Done');
        fold.setAttribute('aria-expanded', 'true');
        fold.addEventListener('click', () => {
          s.open = false;
          this._renderPoolsBand(s);
        });
        head.appendChild(fold);
      } else {
        const edit = el('button', 'jscf-section-edit', 'Edit');
        // A quiet button: text only, muted, the hover wash.
        edit.type = 'button';
        edit.addEventListener('click', () => {
          s.open = true;
          this._renderPoolsBand(s);
        });
        head.appendChild(edit);
      }
      const rm = el('button', 'blockr-row-remove jscf-section-remove', icons.remove || '×');
      rm.type = 'button';
      const rmText = s.role === 'subgroup' ? 'Show every subgroup separately' : 'Show every group separately';
      rm.setAttribute('aria-label', rmText);
      tip(rm, rmText);
      rm.addEventListener('click', () => {
        const wasSet = !this._isDefaultGroupDef(s, s.edit);
        delete this.groupDefs[s.col];
        s.edit = this._groupDefFor(s);
        s.open = false;
        if (wasSet) this._groupsEdited(s);
        this._renderPoolsBand(s);
      });
      head.appendChild(rm);
      s.el.appendChild(head);
      // The overlap warning, filled by _syncOverlap (subgroup only).
      s.warnEl = el('div', 'jscf-groups-warning');
      s.warnEl.setAttribute('role', 'status');
      s.el.appendChild(s.warnEl);
      this._renderSplitAdd();

      if (!s.open) {
        const summary = el('div', 'jscf-groups-summary');
        for (const c of this._groupColumns(s)) {
          const tag = el('span', 'blockr-select__tag jscf-groups-tag');
          const label = el('span', 'blockr-select__tag-label');
          label.textContent = c.name;
          if (c.title !== c.name) tip(tag, c.title);
          tag.appendChild(label);
          summary.appendChild(tag);
        }
        s.el.appendChild(summary);
        this._syncOverlap(s);
        return;
      }

      // The list: one row per column, in table order (design system, Block
      // lists). A pool is a stack, its header a row and its members rows in
      // its band. Past 8 rows the list scrolls.
      const list = el('div', 'jscf-cols');
      list.setAttribute('role', 'list');
      const places = this._levelPlaces(s);
      for (const c of s.edit.columns) {
        list.appendChild(c.name === ''
          ? this._columnRow(s, c, places)
          : this._poolStack(s, c, places));
      }
      this._wireDrag(s, list);
      s.el.appendChild(list);
      s.listEl = list;
      list.scrollTop = scrollTop;

      // Levels in no column are not shown; a click brings one back on its
      // own, where it was when it was hidden.
      const hidden = s.levels.filter(l => !places.has(l.value));
      if (hidden.length) {
        const line = el('div', 'jscf-cols-hidden');
        line.appendChild(document.createTextNode('Not shown: '));
        hidden.forEach((l, i) => {
          if (i) line.appendChild(document.createTextNode(', '));
          const b = el('button', 'jscf-cols-hidden-level');
          b.type = 'button';
          b.textContent = l.value;
          tip(b, 'Show on its own');
          b.addEventListener('click', () => {
            const cols = s.edit.columns;
            const after = s.hiddenAt.get(l.value);
            // Hidden before this session, or its neighbour is gone: last.
            let i = cols.length;
            if (after === null) i = 0;
            else if (cols.some(x => x.id === after)) i = cols.findIndex(x => x.id === after) + 1;
            cols.splice(i, 0, this._newColumn('', [l.value]));
            s.hiddenAt.delete(l.value);
            this._groupsEdited(s);
            this._renderPoolsBand(s);
          });
          line.appendChild(b);
        });
        s.el.appendChild(line);
      }

      const add = el('button', 'blockr-btn blockr-btn--quiet blockr-btn--s jscf-cols-add',
        s.role === 'subgroup' ? 'Add subgroup pool' : 'Add pool');
      add.type = 'button';
      add.addEventListener('click', () => this._addPool(s));
      s.el.appendChild(add);

      this._syncOverlap(s);
    }

    _newColumn(name, members, custom) {
      return { id: ++this._gPoolSeq, name, members: members.slice(), custom: !!custom, open: true };
    }

    // "2×" on a level that is in two columns, its tooltip naming them.
    _twiceBadge(s, value, places) {
      const cols = places.get(value) || [];
      if (cols.length < 2) return null;
      const badge = el('span', 'jscf-col-badge', `${cols.length}×`);
      const names = cols.map(c => (c.name === '' ? 'its own' : c.name));
      tip(badge, `In ${cols.length} columns: ${names.join(', ')}. Counted in each.`);
      return badge;
    }

    // A row: the name, the "2×" badge where it applies, N, and the "…" tool
    // that takes N's place under the pointer. The whole row drags.
    _row(s, opts) {
      const row = el('div', 'jscf-col-row' + (opts.cls ? ' ' + opts.cls : ''));
      row.setAttribute('role', 'listitem');
      row.draggable = true;
      row.tabIndex = 0;
      row.dataset.col = String(opts.col.id);
      if (opts.member != null) row.dataset.member = opts.member;
      if (opts.lead) row.appendChild(opts.lead);
      const name = el('span', 'jscf-col-name');
      name.textContent = opts.label;
      row.appendChild(name);
      if (opts.meta) row.appendChild(el('span', 'jscf-col-meta', opts.meta));
      if (opts.badge) row.appendChild(opts.badge);
      const n = el('span', 'jscf-col-n');
      n.textContent = String(opts.n);
      row.appendChild(n);
      const dots = el('button', 'jscf-col-dots', ICON_DOTS);
      dots.type = 'button';
      dots.setAttribute('aria-label', 'Actions');
      dots.tabIndex = -1;
      dots.addEventListener('click', (e) => {
        e.stopPropagation();
        this._openRowMenu(s, dots, opts.menu());
      });
      row.appendChild(dots);
      row.addEventListener('keydown', (e) => {
        if (e.target !== row) return;
        if (e.altKey && (e.key === 'ArrowUp' || e.key === 'ArrowDown') && opts.member == null) {
          e.preventDefault();
          this._moveColumnBy(s, opts.col, e.key === 'ArrowUp' ? -1 : 1);
        } else if (e.key === 'Enter' || e.key === ' ') {
          e.preventDefault();
          this._openRowMenu(s, dots, opts.menu());
        }
      });
      if (name.scrollWidth > name.clientWidth || opts.label.length > 28) tip(name, opts.label);
      return row;
    }

    _columnRow(s, c, places) {
      const v = c.members[0];
      return this._row(s, {
        col: c,
        label: v,
        badge: this._twiceBadge(s, v, places),
        n: this._levelN(s, [v]),
        menu: () => this._singleMenu(s, c)
      });
    }

    _poolStack(s, c, places) {
      const stack = el('div', 'jscf-col-stack' + (c.open ? '' : ' jscf-col-stack--folded'));
      stack.dataset.col = String(c.id);
      const chev = el('span', 'jscf-col-chev', ((window.Blockr && window.Blockr.icons) || {}).chevron || '');
      const head = this._row(s, {
        col: c,
        cls: 'jscf-col-row--head',
        label: c.name,
        meta: c.members.length === 1 ? '1 level' : `${c.members.length} levels`,
        n: this._levelN(s, c.members),
        menu: () => this._poolMenu(s, c)
      });
      head.appendChild(chev);
      head.setAttribute('aria-expanded', c.open ? 'true' : 'false');
      // A click folds, a double-click renames (design system, Renaming in
      // place). The fold waits out the double-click.
      let clickTimer = null;
      head.addEventListener('click', (e) => {
        if (e.target.closest('.jscf-col-dots, input')) return;
        clearTimeout(clickTimer);
        clickTimer = setTimeout(() => {
          c.open = !c.open;
          this._renderPoolsBand(s);
        }, 220);
      });
      const nameEl = head.querySelector('.jscf-col-name');
      nameEl.setAttribute('data-blockr-editable', '');
      head.addEventListener('dblclick', (e) => {
        if (e.target.closest('.jscf-col-dots')) return;
        clearTimeout(clickTimer);
        this._renamePool(s, c, nameEl);
      });
      stack.appendChild(head);
      if (c.open) {
        for (const v of c.members) {
          stack.appendChild(this._row(s, {
            col: c,
            member: v,
            cls: 'jscf-col-row--member',
            label: v,
            badge: this._twiceBadge(s, v, places),
            n: this._levelN(s, [v]),
            menu: () => this._memberMenu(s, c, v)
          }));
        }
      }
      return stack;
    }

    // The pool's name turns into a field where it is. Enter or a click
    // elsewhere commits, Escape restores; an empty name or one another
    // column has is refused in place.
    _renamePool(s, c, nameEl) {
      const input = el('input', 'jscf-pool-name');
      input.type = 'text';
      input.value = c.name;
      input.spellcheck = false;
      input.autocomplete = 'off';
      input.setAttribute('aria-label', 'Pool name');
      const err = el('div', 'jscf-pool-error');
      err.style.display = 'none';
      const row = nameEl.closest('.jscf-col-row');
      row.draggable = false;
      nameEl.replaceWith(input);
      row.after(err);
      input.focus();
      input.select();
      const problem = (raw) => {
        const t = raw.trim();
        if (!t) return 'A pool needs a name.';
        if (this._takenNames(s, c).has(t)) return `"${t}" is already a column.`;
        return '';
      };
      const showErr = (text) => {
        err.textContent = text;
        err.style.display = text ? '' : 'none';
        input.classList.toggle('jscf-pool-name--invalid', !!text);
      };
      let done = false;
      const finish = (commit) => {
        if (done) return;
        if (commit) {
          const t = input.value.trim();
          if (t !== c.name) {
            const why = problem(input.value);
            if (why) { showErr(why); return; }
            c.name = t;
            c.custom = true;
            done = true;
            this._groupsEdited(s);
          }
        }
        done = true;
        this._renderPoolsBand(s);
      };
      input.addEventListener('input', () => {
        showErr(input.value.trim() === c.name ? '' : problem(input.value));
      });
      input.addEventListener('keydown', (e) => {
        e.stopPropagation();
        if (e.key === 'Enter') { e.preventDefault(); finish(true); }
        else if (e.key === 'Escape') { e.preventDefault(); finish(false); }
      });
      input.addEventListener('blur', () => {
        if (problem(input.value) && input.value.trim() !== c.name) finish(false);
        else finish(true);
      });
    }

    // Every column name but `col`'s own: the levels on their own and the
    // other pools.
    _takenNames(s, col) {
      const taken = new Set();
      for (const c of s.edit.columns) {
        if (c === col) continue;
        if (c.name === '') { if (c.members.length) taken.add(c.members[0]); }
        else taken.add(c.name);
      }
      return taken;
    }

    // A pool whose name was never typed follows its members.
    _renameDefault(s, c) {
      if (c.name === '' || c.custom) return;
      c.name = uniqueName(poolDefaultName(c.members, s.levels), this._takenNames(s, c));
    }

    _pools(s) {
      return s.edit.columns.filter(c => c.name !== '');
    }

    // -- The rows' "…" menus. Every drag has its path here too. ------------

    // The "…" shows only under the pointer. Its row holds that state while
    // the menu is open: a hidden "…" measures 0x0, and the menu, which
    // follows its anchor, jumps to the corner of the page.
    _openRowMenu(s, anchor, items) {
      const menu = window.Blockr && window.Blockr.menu;
      if (!menu) return;
      const row = anchor.closest('.jscf-col-row');
      if (row) row.classList.add('jscf-col-row--menu');
      menu(anchor, {
        items,
        align: 'end',
        onClose: () => { if (row) row.classList.remove('jscf-col-row--menu'); }
      });
    }

    // Apply `fn` to a copy of the columns; refused if it leaves none.
    _edit(s, fn) {
      const before = s.edit.columns;
      const next = before.map(c => Object.assign({}, c, { members: c.members.slice() }));
      fn(next);
      const kept = next.filter(c => c.name !== '' || c.members.length);
      if (this._leavesNoColumn(kept)) return false;
      // A pool lists its members in level order, however they arrived.
      const order = new Map(s.levels.map((l, i) => [l.value, i]));
      const rank = (v) => (order.has(v) ? order.get(v) : order.size);
      for (const c of kept) c.members.sort((a, b) => rank(a) - rank(b));
      this._rememberHidden(s, before, kept);
      s.edit.columns = kept;
      for (const c of kept) this._renameDefault(s, c);
      this._groupsEdited(s);
      this._renderPoolsBand(s);
      return true;
    }

    // A level an edit leaves in no column remembers the column it sat after,
    // so "Not shown" can put it back there. `null` is the top.
    _rememberHidden(s, before, after) {
      const shown = (cols) => new Set(cols.flatMap(c => c.members));
      const was = shown(before);
      const now = shown(after);
      const kept = new Set(after.map(c => c.id));
      for (const v of was) {
        if (now.has(v)) continue;
        let i = before.findIndex(c => c.members.includes(v)) - 1;
        while (i >= 0 && !kept.has(before[i].id)) i--;
        s.hiddenAt.set(v, i >= 0 ? before[i].id : null);
      }
    }

    _wouldLeaveNone(s, fn) {
      const next = s.edit.columns.map(c => Object.assign({}, c, { members: c.members.slice() }));
      fn(next);
      return this._leavesNoColumn(next);
    }

    // Pick a pool (or a new one) from a menu, then run `then(pool)`.
    _pickPool(s, anchor, title, then) {
      const pools = this._pools(s);
      const menu = window.Blockr && window.Blockr.menu;
      if (!menu) return;
      const items = pools.map(p => ({
        label: p.name,
        meta: `${p.members.length}`,
        onSelect: () => then(p.id)
      }));
      if (items.length) items.push({ divider: true });
      items.push({ label: 'New pool', onSelect: () => then(null) });
      setTimeout(() => menu(anchor, { items, align: 'end', caption: title }), 0);
    }

    _singleMenu(s, c) {
      const v = c.members[0];
      // The row, not its "…": that one is hidden once the pointer leaves the
      // row, and a menu anchored to it opens in the corner of the page.
      const dots = () => s.el.querySelector(
        `.jscf-col-row[data-col="${c.id}"]:not([data-member])`) || s.el;
      // A new pool opens its levels menu, since one level is rarely the
      // pool anyone wants.
      const toPool = (copy) => (poolId) => {
        let fresh = null;
        const ok = this._edit(s, (cols) => {
          const i = cols.findIndex(x => x.id === c.id);
          if (poolId == null) {
            fresh = this._newColumn('New pool', [v]);
            cols.splice(copy ? i + 1 : i, copy ? 0 : 1, fresh);
          } else {
            const p = cols.find(x => x.id === poolId);
            if (!p.members.includes(v)) p.members.push(v);
            if (!copy) cols.splice(i, 1);
          }
        });
        if (ok && fresh) {
          const pool = s.edit.columns.find(x => x.id === fresh.id);
          const head = s.el.querySelector(`.jscf-col-row--head[data-col="${fresh.id}"]`);
          if (pool && head) setTimeout(() => this._editLevels(s, pool, head), 0);
        }
      };
      const hide = (cols) => { cols.splice(cols.findIndex(x => x.id === c.id), 1); };
      return [
        { label: 'Move to pool…', onSelect: () => this._pickPool(s, dots(), `Move ${v} to`, toPool(false)) },
        { label: 'Also add to pool…', onSelect: () => this._pickPool(s, dots(), `Also add ${v} to`, toPool(true)) },
        { label: 'Move to top', disabled: s.edit.columns[0] === c,
          onSelect: () => this._edit(s, (cols) => {
            const i = cols.findIndex(x => x.id === c.id);
            cols.unshift(cols.splice(i, 1)[0]);
          }) },
        { divider: true },
        { label: 'Hide', disabled: this._wouldLeaveNone(s, hide),
          reason: 'A split keeps at least one column',
          onSelect: () => this._edit(s, hide) }
      ];
    }

    _memberMenu(s, c, v) {
      const own = s.edit.columns.some(x => x.name === '' && x.members[0] === v);
      const out = (copy) => (cols) => {
        const i = cols.findIndex(x => x.id === c.id);
        const p = cols[i];
        if (!copy) p.members = p.members.filter(m => m !== v);
        cols.splice(i + 1, 0, this._newColumn('', [v]));
        if (!p.members.length) cols.splice(i, 1);
      };
      const remove = (cols) => {
        const i = cols.findIndex(x => x.id === c.id);
        cols[i].members = cols[i].members.filter(m => m !== v);
        if (!cols[i].members.length) cols.splice(i, 1);
      };
      return [
        { label: 'Move out of the pool', onSelect: () => this._edit(s, out(false)) },
        { label: 'Also show on its own', disabled: own, reason: 'Already shown on its own',
          onSelect: () => this._edit(s, out(true)) },
        { divider: true },
        { label: 'Remove from the pool', disabled: this._wouldLeaveNone(s, remove),
          reason: 'A split keeps at least one column',
          onSelect: () => this._edit(s, remove) }
      ];
    }

    _poolMenu(s, c) {
      const ownOf = (cols, v) => cols.findIndex(x => x.name === '' && x.members[0] === v);
      const hasOwn = c.members.some(v => ownOf(s.edit.columns, v) >= 0);
      const head = () => s.el.querySelector(
        `.jscf-col-row--head[data-col="${c.id}"]`) || s.el;
      const removePool = (cols) => { cols.splice(cols.findIndex(x => x.id === c.id), 1); };
      return [
        { label: 'Rename', onSelect: () => {
          const nameEl = head().querySelector('.jscf-col-name');
          if (nameEl) setTimeout(() => this._renamePool(s, c, nameEl), 0);
        } },
        { label: 'Edit levels…', onSelect: () => setTimeout(() => this._editLevels(s, c, head()), 0) },
        { label: "Hide members' own rows", disabled: !hasOwn,
          reason: 'No member is shown on its own',
          onSelect: () => this._edit(s, (cols) => {
            for (const v of c.members) {
              const i = ownOf(cols, v);
              if (i >= 0) cols.splice(i, 1);
            }
          }) },
        { label: 'Ungroup', onSelect: () => this._edit(s, (cols) => {
          const i = cols.findIndex(x => x.id === c.id);
          const singles = c.members.filter(v => ownOf(cols, v) < 0)
            .map(v => this._newColumn('', [v]));
          cols.splice(i, 1, ...singles);
        }) },
        { divider: true },
        { label: 'Remove pool', disabled: this._wouldLeaveNone(s, removePool),
          reason: 'A split keeps at least one column',
          onSelect: () => this._edit(s, removePool) }
      ];
    }

    // A pool's members from a menu of every level, ticked (Select.menu,
    // multi). A tick adds: the level keeps its own row, which is how a
    // double count is made; each option says where the level already is.
    // The list redraws when the menu closes, so the anchor stays put.
    _editLevels(s, c, anchor, onDone) {
      const Select = this._select();
      const places = this._levelPlaces(s);
      const options = s.levels.map(l => {
        const where = (places.get(l.value) || []).filter(x => x !== c)
          .map(x => (x.name === '' ? 'own column' : x.name));
        return { value: l.value, label: where.concat(String(l.n)).join(' · ') };
      });
      const metaEl = anchor.querySelector && anchor.querySelector('.jscf-col-meta');
      const nEl = anchor.querySelector && anchor.querySelector('.jscf-col-n');
      s.menu = Select.menu(anchor, {
        mode: 'multi',
        title: c.name,
        options,
        selected: c.members.slice(),
        reorderable: false,
        onChange: (values) => {
          const order = s.levels.map(l => l.value);
          c.members = order.filter(v => values.includes(v))
            .concat(values.filter(v => !order.includes(v)));
          this._renameDefault(s, c);
          if (metaEl) metaEl.textContent = c.members.length === 1 ? '1 level' : `${c.members.length} levels`;
          if (nEl) nEl.textContent = String(this._levelN(s, c.members));
          this._groupsEdited(s);
        },
        onClose: () => {
          s.menu = null;
          // A pool left empty when its menu closes was never built.
          if (!c.members.length) {
            const rest = s.edit.columns.filter(x => x !== c);
            if (!this._leavesNoColumn(rest)) {
              s.edit.columns = rest;
              this._groupsEdited(s);
            }
          }
          this._renderPoolsBand(s);
          const back = s.el.querySelector(`.jscf-col-row--head[data-col="${c.id}"]`);
          if (back) back.scrollIntoView({ block: 'nearest' });
          if (onDone) onDone();
        }
      });
    }

    _addPool(s) {
      const pool = this._newColumn(
        uniqueName('New pool', this._takenNames(s, null)), []);
      s.edit.columns.push(pool);
      this._renderPoolsBand(s);
      const head = s.el.querySelector(`.jscf-col-row--head[data-col="${pool.id}"]`);
      if (head) {
        head.scrollIntoView({ block: 'nearest' });
        setTimeout(() => this._editLevels(s, pool, head), 0);
      }
    }

    _moveColumnBy(s, c, delta) {
      const cols = s.edit.columns;
      const i = cols.indexOf(c);
      const j = i + delta;
      if (i < 0 || j < 0 || j >= cols.length) return;
      cols.splice(j, 0, cols.splice(i, 1)[0]);
      this._groupsEdited(s);
      this._renderPoolsBand(s);
      const row = s.el.querySelector(`.jscf-col-row[data-col="${c.id}"]:not([data-member])`);
      if (row) row.focus();
    }

    // -- Drag ---------------------------------------------------------------
    // A drag moves; held with Alt it copies. Between two columns it places
    // the dragged thing there (a 2px accent line, as in block lists); onto
    // a level it pools the two, onto a pool it joins it (the row or band
    // takes the accent tint, as in the outline).

    _wireDrag(s, list) {
      let src = null;
      // One line, placed over the gap; inserting it into the list would move
      // the rows under the pointer.
      const line = el('div', 'jscf-col-dropline');
      line.style.display = 'none';
      const clear = () => {
        line.style.display = 'none';
        for (const t of list.querySelectorAll('.jscf-col--target')) t.classList.remove('jscf-col--target');
      };
      // Where a drop at clientY over `target` lands.
      const zone = (e) => {
        const row = e.target.closest && e.target.closest('.jscf-col-row');
        if (!row || !list.contains(row)) return null;
        const col = s.edit.columns.find(c => String(c.id) === row.dataset.col);
        if (!col) return null;
        if (row.dataset.member != null) return { kind: 'onto', col, el: row.closest('.jscf-col-stack') };
        const box = row.closest('.jscf-col-stack') || row;
        const r = row.getBoundingClientRect();
        const y = (e.clientY - r.top) / r.height;
        const isHead = row.classList.contains('jscf-col-row--head');
        if (y < 0.28) return { kind: 'before', col, el: box };
        if (y > 0.72 && !(isHead && col.open && col.members.length)) return { kind: 'after', col, el: box };
        return { kind: 'onto', col, el: col.name === '' ? row : box };
      };
      const valid = (z) => {
        if (!z || !src) return false;
        if (src.member == null && z.col === src.col) return false;
        if (src.member != null && z.kind === 'onto' && z.col === src.col) return false;
        // A pool dropped onto something merges; dropped onto itself it does not.
        return true;
      };
      list.addEventListener('dragstart', (e) => {
        const row = e.target.closest && e.target.closest('.jscf-col-row');
        if (!row) return;
        const col = s.edit.columns.find(c => String(c.id) === row.dataset.col);
        if (!col) return;
        src = { col, member: row.dataset.member != null ? row.dataset.member : null };
        e.dataTransfer.effectAllowed = 'copyMove';
        e.dataTransfer.setData('text/plain', src.member != null ? src.member : (col.name || col.members[0]));
        (row.dataset.member == null && row.closest('.jscf-col-stack') || row).classList.add('jscf-col--dragging');
      });
      list.addEventListener('dragover', (e) => {
        if (!src) return;
        const z = zone(e);
        clear();
        if (!valid(z)) return;
        e.preventDefault();
        e.dataTransfer.dropEffect = e.altKey ? 'copy' : 'move';
        if (z.kind === 'onto') {
          z.el.classList.add('jscf-col--target');
        } else {
          if (line.parentNode !== list) list.appendChild(line);
          const top = z.kind === 'before' ? z.el.offsetTop : z.el.offsetTop + z.el.offsetHeight;
          line.style.top = (top - 1) + 'px';
          line.style.display = '';
        }
      });
      list.addEventListener('dragleave', (e) => {
        if (!list.contains(e.relatedTarget)) clear();
      });
      list.addEventListener('drop', (e) => {
        if (!src) return;
        const z = zone(e);
        clear();
        if (!valid(z)) return;
        e.preventDefault();
        this._applyDrop(s, src, z, e.altKey);
        src = null;
      });
      list.addEventListener('dragend', () => {
        clear();
        src = null;
        for (const t of list.querySelectorAll('.jscf-col--dragging')) t.classList.remove('jscf-col--dragging');
      });
    }

    _applyDrop(s, src, z, copy) {
      this._edit(s, (cols) => {
        const find = (c) => cols.find(x => x.id === c.id);
        const from = find(src.col);
        const to = find(z.col);
        // What is dragged: one level (a level's row or a pool's member) or a
        // whole pool.
        const members = src.member != null ? [src.member] : from.members.slice();
        const take = () => {
          if (copy) return;
          if (src.member != null) {
            from.members = from.members.filter(m => m !== src.member);
            if (!from.members.length) cols.splice(cols.indexOf(from), 1);
          } else {
            cols.splice(cols.indexOf(from), 1);
          }
        };
        if (z.kind === 'onto') {
          if (to.name === '') {
            // Onto a level: the two become a pool where the level was. A
            // dragged pool keeps its name.
            // _edit() names a pool whose name was never typed.
            const keep = src.member == null && from.name !== '';
            const pool = this._newColumn(keep ? from.name : 'New pool',
              to.members.concat(members.filter(m => !to.members.includes(m))),
              keep && from.custom);
            cols.splice(cols.indexOf(to), 1, pool);
            take();
          } else {
            for (const m of members) if (!to.members.includes(m)) to.members.push(m);
            take();
          }
          return;
        }
        // Between columns: a level lands as its own row, a pool as itself.
        const moved = src.member != null || from.name === ''
          ? this._newColumn('', members)
          : (copy ? this._newColumn(from.name, members, from.custom) : from);
        if (copy && moved.custom) {
          moved.name = uniqueName(moved.name, this._takenNames({ edit: { columns: cols } }, null));
        }
        if (moved === from) cols.splice(cols.indexOf(from), 1);
        else take();
        let at = cols.indexOf(to);
        if (at < 0) at = cols.length;
        cols.splice(z.kind === 'before' ? at : at + 1, 0, moved);
      });
    }

    // -- Receive data from R ------------------------------------------------

    setData(msg) {
      // Set before the async chain: the first-bind rescue in initialize()
      // asks "did R's regular push reach me?", and a payload that is still
      // inflating counts as reached.
      this._dataSeen = true;
      // Serialize ingests: setData is async (gzip inflate) and a `_ready`
      // re-ship arriving during an in-flight decode must not interleave its
      // teardown/build with ours.
      this._setDataChain = (this._setDataChain || Promise.resolve())
        .then(() => this._setDataImpl(msg))
        .catch(e => console.error('[js-crossfilter] setData failed:', e));
      return this._setDataChain;
    }

    async _setDataImpl(msg) {
      const t0 = performance.now();
      this._teardown();

      // Dictionary levels for encoded (char/factor) columns: rows carry
      // 0-based int codes; `this.levels[col][code]` is the label. Decoding
      // happens per received message and never mutates the payload, so the
      // server's cached re-ship (`_ready` handshake) is idempotent. An
      // absent entry means the column is not encoded (numeric / logical /
      // date) and takes the legacy String(v) paths.
      const lv = msg.levels;
      this.levels = typeof lv === 'string' ? JSON.parse(lv) : (lv || {});
      if (Array.isArray(this.levels)) this.levels = {};
      this._levelIndex = {};

      this.dimSource = msg.dim_source || {};
      this.parentKey = msg.parent_key;
      this.parentTable = msg.parent_table;
      // Only the star-schema builder sends these; without them the header
      // counts lookup rows instead of subjects.
      this.parentN = typeof msg.parent_n === 'number' ? msg.parent_n : null;
      this.subjectUnit = typeof msg.subject_unit === 'string'
        ? msg.subject_unit : 'rows';
      this._keptKeys = null;
      this.childFkCols = msg.child_fk_cols || {};
      this.columnInfo = msg.column_info || {};
      this.allColumns = msg.all_columns || msg.column_info || {};
      this.activeDims = msg.active_dims || {};
      this.featured = asArray(msg.featured);
      this.pinnable = asArray(msg.pinnable);
      this.pinned = msg.pinned == null ? null : asArray(msg.pinned)[0];
      this.subgroup = msg.subgroup == null ? null : asArray(msg.subgroup)[0];
      this.measure = msg.measure || '.count';
      this.aggFunc = msg.agg_func || 'sum';

      const note = typeof msg.note === 'string' ? msg.note : '';
      this.noteEl.textContent = note;
      this.noteEl.style.display = note ? '' : 'none';

      // Create crossfilter instances from columnar data
      // Shiny delivers pre-serialized json verbatim as parsed objects
      for (const [childTable, colsOrRows] of Object.entries(msg.lookups || {})) {
        let rows;
        if (msg.compression === 'deflate' && typeof colsOrRows === 'string') {
          rows = columnsToRows(await inflateBase64(colsOrRows));
        } else if (typeof colsOrRows === 'string') {
          rows = columnsToRows(JSON.parse(colsOrRows));
        } else if (Array.isArray(colsOrRows)) {
          rows = colsOrRows;
        } else {
          rows = columnsToRows(colsOrRows);
        }
        const cf = crossfilter(rows);
        this.instances[childTable] = cf;
        const fkCol = this.childFkCols[childTable];
        if (fkCol) {
          this.keyDims[childTable] = cf.dimension(d => d[fkCol]);
        }
      }

      // Map dims to crossfilter instances. Prefer the dim's declared source
      // table when it's an instance — this keeps parent dims (ARM, SEX) on
      // the parent crossfilter, not a child whose row coverage of parent
      // keys may be incomplete (e.g. adae lacking subjects with no AEs).
      const childTables = Object.keys(this.instances);
      for (const dim of Object.keys(this.dimSource)) {
        const sourceTable = this.dimSource[dim];
        const sourceCf = sourceTable && this.instances[sourceTable];
        if (sourceCf && sourceCf.size() > 0 && dim in sourceCf.all()[0]) {
          this.dimChild[dim] = sourceTable;
          continue;
        }
        for (const ct of childTables) {
          const cf = this.instances[ct];
          if (cf.size() > 0 && dim in cf.all()[0]) {
            this.dimChild[dim] = ct;
            break;
          }
        }
      }

      // Create crossfilter dimensions and groups
      for (const [dim, sourceTable] of Object.entries(this.dimSource)) {
        const childTable = this.dimChild[dim];
        if (!childTable) continue;
        const cf = this.instances[childTable];
        // Date dims use a numeric (epoch-day) accessor. With the raw
        // string/null accessor an NA date (shipped as JSON null) lands at one
        // end of crossfilter's sort, so bottom(1)/top(1) return the null
        // record and `new Date(null)` collapses the bound to 1970-01-01 —
        // an inverted range and an unusable slider. As epoch-days, missing
        // values become NaN, which crossfilter drops from the sorted index.
        const dimType = this._getDimType(dim);
        this.dimensions[dim] = dimType === 'date'
          ? cf.dimension(d => toEpochDay(d[dim]))
          : cf.dimension(d => d[dim]);
        // Groups exist ONLY for categorical dims -- they are what the bar
        // cards render. Range/date cards never read a group, and creating
        // one is not just waste: a date dim with missing values has NaN
        // keys, and crossfilter's group machinery on NaN keys kills the
        // renderer (observed as a tab crash inside group(); the
        // NaN-is-dropped guarantee holds for the dimension index only).
        if (dimType === 'categorical') {
          this.groups[dim] = this._createGroup(dim, sourceTable);
        }
      }

      this._updateMeasureUI();
      this._buildPanels();
      this._renderShelf();
      this._renderGroupField();
      this._renderSubgroupField();
      this._ingestGroups(msg);
      this._renderFeaturedField();
      this._syncSearchRows();
      this._applyInitialFilters(msg.cat_filters, msg.rng_filters);
      this._updateAllCounts();
      // First setData has populated `this.filters` (either from
      // msg.cat_filters/rng_filters or as empty by default). It's now
      // safe to publish state to Shiny. See getValue() for why we held
      // back.
      this._ready = true;
      this._scheduleSubmit();

      const elapsed = Math.round(performance.now() - t0);
      this._lastSetDataMs = elapsed;
      console.log(`[js-crossfilter] setData: ${elapsed}ms (${childTables.length} tables, ${Object.keys(this.dimSource).length} dims)`);
      this._updateStatus();
    }

    // Apply filters that arrived with the initial setData payload
    // (constructor state at first render). After _buildPanels but
    // before we flip _ready and submit — the populated state is what
    // the InputBinding will then publish to Shiny.
    _applyInitialFilters(catFilters, rngFilters) {
      this._applyFilterStateFromR(catFilters, rngFilters);
    }

    // Apply filter state pushed from R (initial render, board restore,
    // AI-assistant write). Suppresses the JS->R round-trip because R
    // already has this value — we're catching the UI up, not making a
    // user-initiated change. Without this suppression the change would
    // race back through input$crossfilter_input and re-fire the same
    // R-side observers we just heard from.
    setExternalFilters(catFilters, rngFilters) {
      // Clear bars currently held by crossfilter so removed filters
      // actually release rows (just deleting from this.filters isn't
      // enough — the crossfilter.dimension still holds the predicate).
      for (const dim of Object.keys(this.filters)) {
        if (this.dimensions[dim]) {
          try { this.dimensions[dim].filterAll(); } catch (_) {}
        }
      }
      this.filters = {};
      this._suppressSubmit = true;
      try {
        this._applyFilterStateFromR(catFilters, rngFilters);
      } finally {
        this._suppressSubmit = false;
      }
      // Repaint counts + slider positions + active-dim chips.
      this._updateAllCounts();
      this._updateStatus();
    }

    _applyFilterStateFromR(catFilters, rngFilters) {
      if (catFilters && typeof catFilters === 'object') {
        for (const tbl of Object.keys(catFilters)) {
          const tblFilters = catFilters[tbl] || {};
          for (const dim of Object.keys(tblFilters)) {
            if (!this.dimensions[dim]) continue;
            const vals = asArray(tblFilters[dim]);
            if (vals.length === 0) continue;
            this._applyFilter(dim, vals);
          }
        }
      }
      if (rngFilters && typeof rngFilters === 'object') {
        for (const tbl of Object.keys(rngFilters)) {
          const tblFilters = rngFilters[tbl] || {};
          for (const dim of Object.keys(tblFilters)) {
            if (!this.dimensions[dim]) continue;
            const rng = asArray(rngFilters[tbl][dim]).map(Number);
            if (rng.length !== 2) continue;
            this._applyFilter(dim, { min: rng[0], max: rng[1] });
          }
        }
      }
    }

    _teardown() {
      for (const dim of Object.values(this.dimensions)) {
        try { dim.dispose(); } catch (_) {}
      }
      for (const dim of Object.values(this.keyDims)) {
        try { dim.dispose(); } catch (_) {}
      }
      this.instances = {};
      this.dimensions = {};
      this.groups = {};
      this.dimChild = {};
      this.keyDims = {};
      this.filters = {};
      this.levels = {};
      this._levelIndex = {};
      this.panels = {};
    }

    // -- Group creation (measure-aware) -------------------------------------

    _createGroup(dim, sourceTable) {
      const d = this.dimensions[dim];
      const hasMeasure = this.measure && this.measure !== '.count';
      const isParentDim = (sourceTable === this.parentTable);

      // Parse measure: "table.column" format. Match the prefix against known
      // table names — using `indexOf('.')` fails for table names that start
      // with a dot (e.g. data.frame inputs wrapped as `dm(.tbl = df)`), where
      // the first dot is at index 0 and the slice leaves `tbl.column`.
      let measureCol = null;
      if (hasMeasure) {
        for (const tbl of Object.keys(this.allColumns || {})) {
          const prefix = tbl + '.';
          if (this.measure.startsWith(prefix)) {
            measureCol = this.measure.slice(prefix.length);
            break;
          }
        }
        if (measureCol == null) {
          const dot = this.measure.indexOf('.');
          measureCol = dot >= 0 ? this.measure.slice(dot + 1) : this.measure;
        }
      }

      if (!hasMeasure) {
        // Count mode
        if (isParentDim && this.parentKey) {
          const pk = this.parentKey;
          return d.group().reduce(
            (p, v) => { const k = v[pk]; p.keys[k] = (p.keys[k] || 0) + 1; p.count = Object.keys(p.keys).length; return p; },
            (p, v) => { const k = v[pk]; p.keys[k] -= 1; if (p.keys[k] <= 0) delete p.keys[k]; p.count = Object.keys(p.keys).length; return p; },
            () => ({ keys: {}, count: 0 })
          );
        }
        return d.group().reduceCount();
      }

      if (this.aggFunc === 'mean') {
        // Mean: track sum + count, compute mean on read
        const col = measureCol;
        return d.group().reduce(
          (p, v) => { const val = +v[col] || 0; p.sum += val; p.count++; p.value = p.count ? p.sum / p.count : 0; return p; },
          (p, v) => { const val = +v[col] || 0; p.sum -= val; p.count--; p.value = p.count ? p.sum / p.count : 0; return p; },
          () => ({ sum: 0, count: 0, value: 0 })
        );
      }

      // Sum (default for measures)
      return d.group().reduceSum(r => +r[measureCol] || 0);
    }

    // Extract the display value from a group entry
    _getGroupValue(d) {
      const v = d.value;
      if (v == null) return 0;
      if (typeof v === 'object') return v.value ?? v.count ?? 0;
      return v;
    }

    // -- Dictionary decode (codes <-> labels) -------------------------------
    // Crossfilter rows, dimensions, group keys and predicates all hold int
    // codes for encoded columns; labels exist only at the edges: DOM text,
    // dataset.value, `this.filters`, and the R contract (getValue /
    // setExternalFilters are label-string shaped on both sides).

    _decodeStr(col, key) {
      const lv = this.levels[col];
      return lv ? lv[key] : String(key);
    }

    _levelIndexFor(col) {
      let idx = this._levelIndex[col];
      if (!idx) {
        idx = new Map();
        this.levels[col].forEach((label, code) => idx.set(label, code));
        this._levelIndex[col] = idx;
      }
      return idx;
    }

    // Categorical filter predicate over row values, from label strings.
    // Encoded dim: labels map to a numeric code Set (labels unknown to this
    // payload -- e.g. a board saved against older data -- are dropped and
    // match nothing, same as today's stale-filter behavior). Non-encoded
    // dim (logical / low-cardinality numeric): legacy String(v) matching.
    _makeCatPredicate(dim, labels) {
      if (this.levels[dim]) {
        const idx = this._levelIndexFor(dim);
        const set = new Set();
        for (const l of labels) {
          const code = idx.get(String(l));
          if (code !== undefined) set.add(code);
        }
        return v => set.has(v);
      }
      const set = new Set(labels.map(String));
      return v => set.has(String(v));
    }

    // -- Panel building (grouped by table) ----------------------------------

    _getDimType(dim) {
      const source = this.dimSource[dim];
      if (!source || !this.allColumns[source]) return 'categorical';
      const info = this.allColumns[source];
      if (asArray(info.date_dimensions).includes(dim)) return 'date';
      if (asArray(info.range_dimensions).includes(dim)) return 'range';
      return 'categorical';
    }

    _getDimLabel(dim) {
      const source = this.dimSource[dim];
      if (!source || !this.allColumns[source]) return '';
      const labels = this.allColumns[source].labels;
      return (labels && labels[dim] && labels[dim] !== '') ? labels[dim] : '';
    }

    _buildPanels() {
      this.panelsEl.innerHTML = '';
      const dims = Object.keys(this.dimSource);
      if (dims.length === 0) return;

      // Group dims by source table. The pinned dim is not lifted out: it is an
      // ordinary card and stays where it was added, so choosing a group never
      // moves a card the reader was looking at.
      const grouped = {};
      for (const dim of dims) {
        if (!this.dimensions[dim]) continue;
        const src = this.dimSource[dim];
        (grouped[src] = grouped[src] || []).push(dim);
      }

      const multiTable = Object.keys(grouped).length > 1;

      for (const [tbl, tblDims] of Object.entries(grouped)) {
        if (tblDims.length === 0) continue;
        const section = el('div', 'dm-cf-table-section');

        if (multiTable) {
          section.appendChild(el('div', 'dm-cf-table-header', tbl));
        }

        const wrap = el('div', 'dm-cf-dims-wrap');

        for (const dim of tblDims) {
          const type = this._getDimType(dim);
          const card = (type === 'range' || type === 'date')
            ? this._createRangeCard(dim, tbl, type)
            : this._createCategoricalCard(dim, tbl,
                { pinned: dim === this.pinned });
          wrap.appendChild(card);
          this.panels[dim] = card;
        }

        section.appendChild(wrap);
        this.panelsEl.appendChild(section);
      }
    }

    // A card's reset and remove: 26px icon buttons, always shown (the one
    // exception to "hidden until hover", see the design system's crossfilter).
    _cardActions(dim, tbl) {
      const actions = el('div', 'dm-cf-filter-card-actions');
      // Disabled while the card cuts no rows, like Reset all
      // (_syncShelfState keeps it current).
      const resetBtn = el('button', 'dm-cf-reset-btn', ICON_RESET);
      resetBtn.type = 'button';
      resetBtn.disabled = !this._isFiltering(dim);
      resetBtn.setAttribute('aria-label', `Reset the ${dim} filter`);
      tip(resetBtn, 'Reset filter');
      resetBtn.addEventListener('click', () => this._clearFilter(dim));
      actions.appendChild(resetBtn);
      actions._resetBtn = resetBtn;
      const removeBtn = el('button', 'dm-cf-remove-btn', window.Blockr.icons.x);
      removeBtn.type = 'button';
      removeBtn.setAttribute('aria-label', `Remove ${dim}`);
      tip(removeBtn, `Remove ${dim}`);
      removeBtn.addEventListener('click', () => this._removeDimension(tbl, dim));
      actions.appendChild(removeBtn);
      return actions;
    }

    // -- Categorical card ---------------------------------------------------

    _createCategoricalCard(dim, tbl, opts = {}) {
      const isPinned = !!opts.pinned;
      const card = el('div',
        isPinned ? 'dm-cf-filter-card jscf-pinned-card' : 'dm-cf-filter-card');
      card.dataset.dim = dim;

      // Header. Every card carries a plain label, the group's included: the
      // group is chosen in the field at the top of the block, and this card is
      // an ordinary filter that happens to be on the same column. Only the
      // label takes the primary colour, so the eye can pair the two.
      const header = el('div', 'dm-cf-filter-card-header');
      const labelEl = el('span', 'dm-cf-filter-card-label', dim);
      const sublabel = this._getDimLabel(dim);
      if (sublabel) {
        labelEl.appendChild(el('span', 'dm-cf-filter-card-sublabel', sublabel));
      }
      setDimTitle(labelEl, dim, sublabel);
      header.appendChild(labelEl);

      // Removing a card is a one-click job on the card itself. The group's
      // card is removable like any other -- closing it drops a filter, not
      // the grouping.
      header.appendChild(this._cardActions(dim, tbl));
      card.appendChild(header);

      // The card's search field (design system, "Search field"): a
      // magnifier, the input, a clear button while there is text. Shown
      // only above SEARCH_MIN_VALUES values (_renderCategoricalCounts).
      const searchWrap = el('label', 'dm-cf-tw-search');
      searchWrap.style.display = 'none';
      searchWrap.appendChild(el('span', 'dm-cf-tw-search-icon', ICON_SEARCH));
      const searchInput = el('input', 'dm-cf-tw-search-input');
      searchInput.type = 'text';
      searchInput.placeholder = 'Search\u2026';
      searchInput.autocomplete = 'off';
      searchInput.spellcheck = false;
      searchInput.setAttribute('aria-label', `Search ${dim}`);
      const clearBtn = el('button', 'dm-cf-tw-search-clear', window.Blockr.icons.remove);
      clearBtn.type = 'button';
      clearBtn.setAttribute('aria-label', 'Clear the search');
      tip(clearBtn, 'Clear');
      const applySearch = () => {
        card._query = searchInput.value.trim().toLowerCase();
        searchWrap.classList.toggle('dm-cf-tw-search--has', searchInput.value !== '');
        this._applyCardSearch(card);
      };
      searchInput.addEventListener('input', applySearch);
      searchInput.addEventListener('keydown', (e) => {
        if (e.key === 'Escape' && searchInput.value) {
          e.preventDefault();
          e.stopPropagation();
          searchInput.value = '';
          applySearch();
        }
      });
      clearBtn.addEventListener('click', (e) => {
        e.preventDefault();
        searchInput.value = '';
        applySearch();
        searchInput.focus();
      });
      searchWrap.appendChild(searchInput);
      searchWrap.appendChild(clearBtn);
      card.appendChild(searchWrap);
      card._searchWrap = searchWrap;
      card._searchInput = searchInput;
      card._query = '';

      // Default sort: count descending
      card._sortCol = 'count';
      card._sortDir = 'desc';

      // Scrollable table
      const scroll = el('div', 'dm-cf-tw-scroll');
      const table = el('table', 'dm-cf-tw-table');
      const thead = el('thead');
      const headRow = el('tr');

      const valueTh = el('th', 'dm-cf-tw-th');
      const countTh = el('th', 'dm-cf-tw-th');
      // Width follows the widest number the card actually renders
      // (_sizeCountColumn); this is the starting point, replaced on first
      // render. table-layout is fixed, so whatever this column does not take
      // goes to the values, which is where long level names need it.
      countTh.style.width = '135px';

      // The sort cue, as in the table preview: sort bars beside the
      // header text, in the accent on the sorted column. The other column
      // shows, muted and on hover only, what a click on it would do.
      const firstDir = (col) => col === 'count' ? 'desc' : 'asc';
      const sortWords = {
        value: { asc: 'A to Z', desc: 'Z to A' },
        count: { asc: 'Smallest first', desc: 'Largest first' }
      };
      const head = (th, text, col) => {
        th.innerHTML = '';
        const on = card._sortCol === col;
        const dir = on ? card._sortDir : firstDir(col);
        const h = el('span', 'jscf-th-head');
        const cue = el('span',
          `jscf-sort-cue jscf-sort-cue--${dir}${on ? ' jscf-sort-cue--on' : ''}`);
        // Before the name on the right-aligned Count, so the name stays
        // over its numbers; after it on the values.
        if (col === 'count') h.appendChild(cue);
        h.appendChild(document.createTextNode(text));
        if (col !== 'count') h.appendChild(cue);
        th.appendChild(h);
        th.classList.toggle('dm-cf-tw-th--sorted', on);
        if (on) th.setAttribute('aria-sort', dir === 'asc' ? 'ascending' : 'descending');
        else th.removeAttribute('aria-sort');
      };
      const updateThLabels = () => {
        head(valueTh, dim, 'value');
        head(countTh, 'Count', 'count');
      };
      updateThLabels();
      countTh.classList.add('dm-cf-tw-th--num');
      for (const [th, col] of [[valueTh, 'value'], [countTh, 'count']]) {
        tip(th, () => card._sortCol === col ? sortWords[col][card._sortDir] : null);
      }

      const toggleSort = (col) => {
        if (card._sortCol === col) {
          card._sortDir = card._sortDir === 'desc' ? 'asc' : 'desc';
        } else {
          card._sortCol = col;
          card._sortDir = col === 'count' ? 'desc' : 'asc';
        }
        updateThLabels();
        this._renderCategoricalCounts(dim, this.groups[dim].all());
      };

      valueTh.addEventListener('click', () => toggleSort('value'));
      countTh.addEventListener('click', () => toggleSort('count'));

      headRow.appendChild(valueTh);
      headRow.appendChild(countTh);
      thead.appendChild(headRow);
      table.appendChild(thead);

      const tbody = el('tbody');
      table.appendChild(tbody);
      scroll.appendChild(table);
      card.appendChild(scroll);

      card._tbody = tbody;
      card._countTh = countTh;
      return card;
    }

    _renderCategoricalCounts(dim, counts) {
      const card = this.panels[dim];
      if (!card || !card._tbody) return;
      const tbody = card._tbody;

      const gv = (d) => this._getGroupValue(d);
      const hasMeasure = this.measure && this.measure !== '.count';

      const filtered = counts.filter(d => gv(d) > 0 || this._isSelected(dim, d.key));

      // Apply card's sort state
      const sortCol = card._sortCol || 'count';
      const sortDir = card._sortDir || 'desc';
      const sorted = filtered.sort((a, b) => {
        let cmp;
        if (sortCol === 'value') {
          // Decode BEFORE comparing: group keys are int codes for encoded
          // dims, and comparing codes (or stringified codes: "10" < "2")
          // would change the visible order. Missing and empty values go
          // last in both directions, as in every table.
          const av = this._decodeStr(dim, a.key);
          const bv = this._decodeStr(dim, b.key);
          const am = isMissingKey(av), bm = isMissingKey(bv);
          if (am || bm) return am && bm ? 0 : am ? 1 : -1;
          cmp = av.localeCompare(bv);
        } else {
          cmp = gv(a) - gv(b);
        }
        return sortDir === 'asc' ? cmp : -cmp;
      });

      const selected = this.filters[dim];
      const selectedSet = selected ? new Set(selected) : null;
      const hasFilter = !!selectedSet;
      const maxVal = sorted.reduce((m, d) => Math.max(m, Math.abs(gv(d))), 0);

      tbody.innerHTML = '';
      let widestLabel = 0;
      for (const item of sorted) {
        const count = gv(item);
        // Decode once: item.key is an int code for encoded dims. The label
        // is what selection state, dataset.value, the sentinel display test
        // and the click handler all operate on -- comparing item.key against
        // the '__NA__'/'__EMPTY__' STRINGS would silently never match.
        const valLabel = this._decodeStr(dim, item.key);
        const isSelected = selectedSet && selectedSet.has(valLabel);
        const isDimmed = hasFilter && !isSelected;

        const tr = el('tr', 'dm-cf-tw-row' + (isDimmed ? ' dimmed' : ''));
        tr.dataset.value = valLabel;

        // Value cell
        const tdVal = el('td');
        const displayKey = valLabel === '__NA__' ? 'NA'
          : valLabel === '__EMPTY__' ? '(empty)' : valLabel;
        if (valLabel === '__NA__' || valLabel === '__EMPTY__') {
          tdVal.appendChild(el('span', 'dm-cf-tw-missing', displayKey));
        } else {
          tdVal.textContent = displayKey;
        }
        tr.appendChild(tdVal);

        // Bar + count cell
        const tdBar = el('td');
        const barCell = el('div', 'dm-cf-tw-bar-cell');
        const track = el('div', 'dm-cf-tw-bar-track');
        const fill = el('div', 'dm-cf-tw-bar-fill');
        const absCount = Math.abs(count);
        fill.style.width = maxVal > 0 ? `${(absCount / maxVal) * 100}%` : '0%';
        track.appendChild(fill);
        barCell.appendChild(track);
        const label = hasMeasure ? fmtNum(count) : fmtCount(count);
        if (label.length > widestLabel) widestLabel = label.length;
        barCell.appendChild(el('span', 'dm-cf-tw-bar-label', label));
        tdBar.appendChild(barCell);
        tr.appendChild(tdBar);

        tr.addEventListener('click', () => {
          this._toggleCategorical(dim, valLabel);
        });

        tbody.appendChild(tr);
      }

      this._sizeCountColumn(card, widestLabel);

      const searchable = counts.length > SEARCH_MIN_VALUES;
      card._searchWrap.style.display = searchable ? '' : 'none';
      if (!searchable && card._query) {
        card._searchInput.value = '';
        card._query = '';
        card._searchWrap.classList.remove('dm-cf-tw-search--has');
      }
      this._applyCardSearch(card);
    }

    // Hide the rows the card's search does not match. Run after every render,
    // so a data update keeps the reader's query.
    _applyCardSearch(card) {
      const q = card._query || '';
      for (const r of card._tbody.querySelectorAll('.dm-cf-tw-row')) {
        const v = r.dataset.value.toLowerCase();
        r.style.display = (q === '' || v.includes(q)) ? '' : 'none';
      }
    }

    // The count column was a flat 160px, which is right for "1,234,567" and
    // wastes half the card on a study with two-digit counts -- the bar and its
    // number ended up at opposite ends of the row. Size it to the widest
    // number this card actually renders instead, and give what is left to the
    // values, where a long level name ("BLACK OR AFRICAN AMERICAN") is
    // otherwise wrapped to four lines. One width per card, not per row, so the
    // bars still line up.
    _sizeCountColumn(card, widestLabel) {
      const chars = Math.max(2, widestLabel || 0);
      // ch is the digit width in a tabular-numeral font, plus a little for the
      // padding the cell already carries.
      card.style.setProperty('--jscf-count-w', `calc(${chars}ch + 8px)`);
      if (card._countTh) {
        // What the bar needs to reach its full length: the 96px cap, the 5px
        // gap to the number, the cell's own 24px of padding, and the number
        // itself. Keep in step with .dm-cf-tw-bar-track's max-width, or the
        // bar never gets the width the column reserved for it.
        //
        // Capped at 45% of the card because the width is in px and the
        // column does not shrink with it: on a narrow card a fixed
        // reservation eats the level names, and a wrapped name costs a whole
        // row. The bar gives way instead -- it supports the choice, the name
        // IS the choice. The cap is applied here in px rather than as
        // `min(..., 45%)`, which parses but is ignored for a column of a
        // `table-layout: fixed` table (measured: the column fell back to an
        // even split and wrapped every long level).
        const needed = 133 + chars * 7.2;
        const cardW = card.clientWidth || 0;
        card._countTh.style.width = cardW && needed > cardW * 0.45
          ? `${Math.round(cardW * 0.45)}px`
          : `calc(133px + ${chars}ch)`;
      }
    }

    _isSelected(dim, key) {
      // `key` is a group key (int code for encoded dims); `this.filters`
      // holds labels.
      const sel = this.filters[dim];
      return sel && sel.includes(this._decodeStr(dim, key));
    }

    _toggleCategorical(dim, value) {
      let current = this.filters[dim] || null;
      if (!current) {
        current = [value];
      } else if (current.includes(value)) {
        current = current.filter(v => v !== value);
        if (current.length === 0) current = null;
      } else {
        current = [...current, value];
      }
      this._applyFilter(dim, current);
    }

    // -- Range card ---------------------------------------------------------

    _createRangeCard(dim, tbl, type) {
      const card = el('div', 'dm-cf-filter-card dm-cf-range-card');
      card.dataset.dim = dim;
      card._type = type;
      card._dim = dim;

      // Header
      const header = el('div', 'dm-cf-filter-card-header');
      const labelEl = el('span', 'dm-cf-filter-card-label', dim);
      const sublabel = this._getDimLabel(dim);
      if (sublabel) {
        labelEl.appendChild(el('span', 'dm-cf-filter-card-sublabel', sublabel));
      }
      setDimTitle(labelEl, dim, sublabel);
      header.appendChild(labelEl);

      header.appendChild(this._cardActions(dim, tbl));
      card.appendChild(header);

      // Initial bounds (excluding this dim's own filter)
      const bounds = this._getRangeBounds(dim, type);
      if (!bounds) return card;
      const { min: min0, max: max0 } = bounds;

      if (min0 === max0) {
        card.appendChild(el('div', 'dm-cf-range-info',
          `All values: ${type === 'date' ? fmtDate(min0) : fmtNum(min0)}`));
        card._min = min0;
        card._max = max0;
        return card;
      }

      // Row count info (populated by _applyRangeBounds / _updateRangeInfo)
      const infoEl = el('div', 'dm-cf-range-info');
      card.appendChild(infoEl);
      card._infoEl = infoEl;

      // KDE density overlay (SVG) — paths get their `d` attr set in
      // _applyRangeBounds (gray) and _updateRangeInfo (blue). Built for both
      // numeric and date range cards; the value extractors below read date
      // columns as epoch-days.
      {
        const svgNs = 'http://www.w3.org/2000/svg';
        const svg = document.createElementNS(svgNs, 'svg');
        svg.setAttribute('viewBox', '0 0 300 80');
        svg.setAttribute('preserveAspectRatio', 'none');
        svg.classList.add('dm-cf-density-svg');

        // Colours from the tokens (crossfilter-block.css), so the curves
        // follow the scheme.
        const pathAll = document.createElementNS(svgNs, 'path');
        pathAll.setAttribute('class', 'dm-cf-density-all');
        svg.appendChild(pathAll);

        const pathFiltered = document.createElementNS(svgNs, 'path');
        pathFiltered.setAttribute('class', 'dm-cf-density-cut');
        svg.appendChild(pathFiltered);

        card.appendChild(svg);
        card._pathAll = pathAll;
        card._pathFiltered = pathFiltered;
      }

      // Dual range slider
      const slider = el('div', 'dm-cf-dual-range');
      slider.appendChild(el('div', 'dm-cf-dual-range-track'));
      const fillBar = el('div', 'dm-cf-dual-range-fill');
      slider.appendChild(fillBar);

      const inputLo = document.createElement('input');
      inputLo.type = 'range';
      inputLo.style.cssText = 'z-index:3;pointer-events:none;';

      const inputHi = document.createElement('input');
      inputHi.type = 'range';
      inputHi.style.cssText = 'z-index:4;pointer-events:none;';

      // Tooltips
      const bubbleLo = el('span', 'dm-cf-bubble');
      const bubbleHi = el('span', 'dm-cf-bubble');

      slider.appendChild(inputLo);
      slider.appendChild(inputHi);
      slider.appendChild(bubbleLo);
      slider.appendChild(bubbleHi);
      card.appendChild(slider);

      // Min/max labels. They read the HANDLES, not the data bounds: at rest
      // the two are the same value, and once a filter is on, the numbers the
      // user picked are the ones worth showing. Clicking one types it (see
      // _makeRangeLabelEditable).
      // `.blockr-slot` is the ecosystem's mark for a word that is also a
      // control -- blue, dashed underline, solid on hover -- the same one
      // blockr.viz hangs a chart's sentence-style dropdowns off. A bound you
      // can type is that, so it wears that and not a mark of its own.
      const minMaxRow = el('div', 'dm-cf-range-minmax');
      const labelMin = el('span', 'blockr-slot dm-cf-range-edit');
      const labelMax = el('span', 'blockr-slot dm-cf-range-edit');
      labelMin.tabIndex = 0;
      labelMax.tabIndex = 0;
      labelMin.setAttribute('role', 'button');
      labelMax.setAttribute('role', 'button');
      labelMin.setAttribute('aria-label', 'Type the lower bound');
      labelMax.setAttribute('aria-label', 'Type the upper bound');
      minMaxRow.appendChild(labelMin);
      minMaxRow.appendChild(labelMax);
      card.appendChild(minMaxRow);

      card._slider = slider;
      card._fillBar = fillBar;
      card._inputLo = inputLo;
      card._inputHi = inputHi;
      card._bubbleLo = bubbleLo;
      card._bubbleHi = bubbleHi;
      card._labelMin = labelMin;
      card._labelMax = labelMax;

      const fmtVal = type === 'date' ? fmtDate : fmtNum;
      card._fmtVal = fmtVal;

      // card._lo / card._hi are the values the card filters on; the range
      // inputs only carry the thumbs. They are not the same thing: an input
      // snaps its value to `step` ((max-min)/200 on a numeric card), so a
      // typed 60 came back as 59.93. The inputs get the number for the thumb
      // position, these keep it exactly.
      card._lo = Number(inputLo.value);
      card._hi = Number(inputHi.value);

      card._setValues = (loV, hiV) => {
        card._lo = Math.min(loV, hiV);
        card._hi = Math.max(loV, hiV);
        // step='any' for the write, so the DOM value starts from the exact
        // number; restoring step re-rounds it, which is why the thumbs are
        // positioned from card._lo / card._hi and not from the inputs.
        const sLo = inputLo.step, sHi = inputHi.step;
        inputLo.step = 'any'; inputHi.step = 'any';
        inputLo.value = card._lo;
        inputHi.value = card._hi;
        inputLo.step = sLo; inputHi.step = sHi;
        card._updateSlider();
      };

      // Read card._min / card._max so this picks up bounds updates.
      card._updateSlider = () => {
        let lo = card._lo;
        let hi = card._hi;
        if (lo > hi) [lo, hi] = [hi, lo];

        const mn = card._min;
        const mx = card._max;
        const range = mx - mn;
        const loP = range > 0 ? ((lo - mn) / range) * 100 : 0;
        const hiP = range > 0 ? ((hi - mn) / range) * 100 : 100;
        fillBar.style.left = loP + '%';
        fillBar.style.width = (hiP - loP) + '%';

        bubbleLo.textContent = fmtVal(lo);
        bubbleLo.style.left = loP + '%';
        bubbleHi.textContent = fmtVal(hi);
        bubbleHi.style.left = hiP + '%';

        // Skip while a label is being typed into -- the span is out of the
        // DOM then, and writing to it would fight the input.
        if (!card._editing) {
          labelMin.textContent = fmtVal(lo);
          labelMax.textContent = fmtVal(hi);
        }
      };

      // Show the bubbles for a moment after any programmatic move, the same
      // way a drag does.
      card._flashSlider = () => {
        slider.classList.add('dm-cf-active');
        clearTimeout(card._activeTimer);
        card._activeTimer = setTimeout(
          () => slider.classList.remove('dm-cf-active'), 1500);
      };

      // `which` is the end the user dragged, and ONLY that end is re-read
      // from the DOM -- the other one may hold a typed value the input
      // rounded to its step. Called with no argument (from a typed commit)
      // it reads neither and applies card._lo / card._hi as they stand.
      const onInput = (which) => {
        if (which === 'lo') card._lo = Number(inputLo.value);
        if (which === 'hi') card._hi = Number(inputHi.value);
        let lo = card._lo;
        let hi = card._hi;
        if (lo > hi) [lo, hi] = [hi, lo];
        card._lo = lo; card._hi = hi;
        card._updateSlider();
        card._flashSlider();

        // Apply silently: the in-block bars / counts / status update live while
        // dragging, but the R round-trip (and downstream re-eval) is deferred
        // to the `change` handler below, which fires once on drag-end.
        const mn = card._min;
        const mx = card._max;
        const range = mx - mn;
        if (lo <= mn + range * 0.001 && hi >= mx - range * 0.001) {
          this._applyFilter(dim, null, { silent: true });
        } else if (type === 'date') {
          this._applyFilter(dim, { min: lo, max: hi, isDate: true }, { silent: true });
        } else {
          this._applyFilter(dim, { min: lo, max: hi }, { silent: true });
        }
      };

      // Drag-end (mouse-up / key commit). The drag was applied silently, so
      // the other range cards still show the gray curve and "of N rows" from
      // before it: refresh them, then push the resting value to R once.
      const onChange = () => {
        this._updateAllCounts(dim);
        this._scheduleSubmit();
      };

      inputLo.addEventListener('input', () => onInput('lo'));
      inputHi.addEventListener('input', () => onInput('hi'));
      inputLo.addEventListener('change', onChange);
      inputHi.addEventListener('change', onChange);

      const onTyped = () => { onInput(); onChange(); };
      this._makeRangeLabelEditable(card, labelMin, 'lo', onTyped);
      this._makeRangeLabelEditable(card, labelMax, 'hi', onTyped);

      // Apply initial bounds (sets attrs, values, KDE, labels, totalRows).
      this._applyRangeBounds(card, min0, max0);

      return card;
    }

    // Click a min/max label to type the value. The span is swapped for an
    // input in place, so the card neither grows nor moves: a permanent pair of
    // boxes would spend a frame on every card for something used once.
    //
    // A date card gets `type="date"`, which is the calendar picker -- the
    // browser's own, opened on the same click via showPicker() where that
    // exists. Numeric cards get a plain text box; the label is formatted
    // ("1.2K"), so the input is seeded with the RAW number instead.
    _makeRangeLabelEditable(card, span, which, onCommit) {
      span.addEventListener('keydown', (e) => {
        if (e.key === 'Enter' || e.key === ' ') {
          e.preventDefault();
          span.click();
        }
      });
      span.addEventListener('click', () => {
        if (card._editing) return;
        card._editing = true;

        const isDate = card._type === 'date';
        const inp = document.createElement('input');
        inp.className = 'dm-cf-range-input';
        const current = Number(which === 'lo' ? card._lo : card._hi);

        if (isDate) {
          inp.type = 'date';
          inp.min = fmtDate(card._min);
          inp.max = fmtDate(card._max);
          inp.value = fmtDate(current);
        } else {
          inp.type = 'text';
          inp.inputMode = 'decimal';
          inp.value = String(Math.round(current * 1e6) / 1e6);
          // Content width: a box half the card wide for four digits reads as
          // a form field, which this is not.
          inp.size = Math.max(4, inp.value.length + 1);
        }

        span.replaceWith(inp);
        inp.focus();
        if (!isDate) inp.select();
        if (isDate && typeof inp.showPicker === 'function') {
          // Not supported everywhere, and it throws if the input is not
          // user-activated; the field still works without it.
          try { inp.showPicker(); } catch (e) { /* no picker, type instead */ }
        }

        let done = false;
        const finish = (commit) => {
          if (done) return;
          done = true;
          card._editing = false;
          inp.replaceWith(span);

          if (commit) {
            const raw = isDate
              ? Math.round(toEpochDay(inp.value)) : parseFloat(inp.value);
            if (isFinite(raw)) {
              // Clamp into the data bounds, and keep the handles in order --
              // typing a minimum past the maximum means "up to here", not an
              // inverted range.
              let v = Math.min(card._max, Math.max(card._min, raw));
              const other = Number(which === 'lo' ? card._hi : card._lo);
              if (which === 'lo' && v > other) v = other;
              if (which === 'hi' && v < other) v = other;

              card._setValues(
                which === 'lo' ? v : Number(card._lo),
                which === 'hi' ? v : Number(card._hi)
              );
            }
          }

          card._updateSlider();
          if (commit) {
            card._flashSlider();
            onCommit();
          }
        };

        inp.addEventListener('keydown', (e) => {
          if (e.key === 'Enter') finish(true);
          else if (e.key === 'Escape') finish(false);
        });
        inp.addEventListener('blur', () => finish(true));
        // A date input's picker lives outside the field; committing on
        // `change` means a pick from the calendar lands without a blur.
        if (isDate) inp.addEventListener('change', () => finish(true));
      });
    }

    // -- Range bounds helpers -----------------------------------------------

    // Query a range dim's [min, max] over data filtered by everything except
    // its own filter, by temporarily clearing it, reading bounds, restoring.
    _getRangeBounds(dim, type) {
      const cfDim = this.dimensions[dim];
      if (!cfDim) return null;

      const savedFilter = this.filters[dim];
      const hasOwnFilter = savedFilter !== undefined && savedFilter !== null;
      if (hasOwnFilter) cfDim.filterAll();

      const bottom = cfDim.bottom(1)[0];
      const top = cfDim.top(1)[0];

      const toNum = type === 'date'
        ? v => toEpochDay(v) : v => Number(v);
      let min = bottom ? toNum(bottom[dim]) : NaN;
      let max = top ? toNum(top[dim]) : NaN;

      // A column with missing values can put a NaN at either end of the
      // sort, and the card then had no bounds at all: `AENDT` (473 of 1191
      // AE end dates missing) rendered as a bare header, no slider, no
      // density. Fall back to a scan of the rows this dim can see -- same
      // pass the gray density already makes, and only on a bounds change.
      if (!isFinite(min) || !isFinite(max)) {
        const childTable = this.dimChild[dim];
        const rows = childTable ? this.instances[childTable].allFiltered() : [];
        min = Infinity; max = -Infinity;
        for (const r of rows) {
          const v = toNum(r[dim]);
          if (!isFinite(v)) continue;
          if (v < min) min = v;
          if (v > max) max = v;
        }
      }

      if (hasOwnFilter) this._reapplyFilter(dim, savedFilter);

      if (!isFinite(min) || !isFinite(max)) return null;
      return { min, max };
    }

    _reapplyFilter(dim, value) {
      if (value === undefined || value === null) return;
      const cfDim = this.dimensions[dim];
      if (Array.isArray(value)) {
        cfDim.filterFunction(this._makeCatPredicate(dim, value));
      } else if (value && value.min !== undefined) {
        if (value.isDate) {
          // Date dim values are epoch-days (see the dimension accessor); NaN
          // (missing) fails the comparison and is excluded, matching R.
          const loD = value.min, hiD = value.max;
          cfDim.filterFunction(v => typeof v === 'number' && v >= loD && v <= hiD);
        } else {
          // Use filterFunction (inclusive both ends) instead of filterRange
          // (which is [lo, hi) — half-open) so JS results match R's
          // `>= lo & <= hi` semantics exactly.
          const lo = value.min, hi = value.max;
          cfDim.filterFunction(v => typeof v === 'number' && v >= lo && v <= hi);
        }
      }
    }

    // Apply [min, max] to a range card: input attrs, labels, gray KDE,
    // totalRows. Slider value attrs are clamped (or set to bounds when no
    // filter). Caller is responsible for triggering blue-KDE / count refresh.
    _applyRangeBounds(card, min, max) {
      const dim = card._dim;
      card._min = min;
      card._max = max;

      const step = card._type === 'date' ? 1 : (max - min) / 200;
      const lo = card._inputLo;
      const hi = card._inputHi;

      const ownFilter = this.filters[dim];
      const hasRangeFilter = ownFilter && ownFilter.min !== undefined;

      lo.min = min; lo.max = max; lo.step = step;
      hi.min = min; hi.max = max; hi.step = step;

      // Clamp filter values into the new bounds (HTML clamps too, but be
      // explicit). If the filter no longer intersects, snap to endpoints.
      const loV = hasRangeFilter
        ? Math.max(min, Math.min(max, ownFilter.min)) : min;
      const hiV = hasRangeFilter
        ? Math.max(min, Math.min(max, ownFilter.max)) : max;

      // Labels follow the handles; _setValues -> _updateSlider writes them.
      if (card._setValues) {
        card._setValues(loV, hiV);
      } else {
        lo.value = loV;
        hi.value = hiV;
      }

      this._rebuildRangeDensity(card);
    }

    // Rebuild the gray curve + totalRows from the data filtered by
    // all-but-this-dim. Mirrors _getRangeBounds: temporarily clear own
    // filter, read, restore.
    //
    // Runs whenever ANOTHER dim's filter changes, not only when the bounds
    // move: gray is "everything the other filters left", and a filter that
    // drops rows without moving the extremes left it showing the old shape
    // (and the old "of N rows").
    _rebuildRangeDensity(card) {
      const dim = card._dim;
      const min = card._min;
      const max = card._max;
      if (!(max > min)) return;

      const childTable = this.dimChild[dim];
      if (childTable) {
        const cfDim = this.dimensions[dim];
        const savedFilter = this.filters[dim];
        const hadOwnFilter = savedFilter !== undefined && savedFilter !== null;
        if (hadOwnFilter) cfDim.filterAll();

        const toNum0 = card._type === 'date'
          ? r => toEpochDay(r[dim]) : r => +r[dim];
        const allRows = this.instances[childTable].allFiltered();
        card._totalRows = allRows.length;

        if (card._pathAll) {
          const allValues = allRows.map(toNum0).filter(v => isFinite(v));
          const kdeAll = kde(allValues, min, max);
          const kdeMaxY = Math.max(...kdeAll.map(p => p.y), 1);
          card._kdeMaxY = kdeMaxY;
          card._pathAll.setAttribute('d',
            kdeToSvgPath(kdeAll, min, max, kdeMaxY, 300, 80));
          // The blue overlay is this grid, cut (clipGrid), so it repaints
          // for free on every filter change -- no second KDE.
          card._kdeGrid = kdeAll;

          // Integer column: drag in whole units. The default step,
          // (max-min)/200, gave AGE a thumb that stopped on 75.96 and a
          // label reading "76.0" for a column that only holds whole years.
          if (allValues.every(Number.isInteger)) {
            const stepInt = Math.max(1, Math.round((max - min) / 200));
            card._inputLo.step = stepInt;
            card._inputHi.step = stepInt;
          }
        }

        if (hadOwnFilter) this._reapplyFilter(dim, savedFilter);
      }
    }

    // Recompute and apply bounds when other filters change. If the slider
    // had a range filter that no longer intersects the new bounds, clear it
    // (mutate state directly to avoid re-entering _applyFilter).
    _updateRangeBounds(dim, silent = false) {
      const card = this.panels[dim];
      if (!card || !card._inputLo) return; // not a fully-built range card

      const bounds = this._getRangeBounds(dim, card._type);
      if (!bounds) return;
      const { min, max } = bounds;
      if (min === max) return;
      if (Math.abs(card._min - min) < 1e-9 &&
          Math.abs(card._max - max) < 1e-9) {
        // Bounds held, but the rows behind them may not have: refresh the
        // gray curve in place. Skipped mid-drag, see _updateAllCounts.
        if (!silent) this._rebuildRangeDensity(card);
        return;
      }

      this._applyRangeBounds(card, min, max);

      // If filter is now outside new bounds, clear/clamp it (direct mutation —
      // avoids re-entering _applyFilter from inside _updateAllCounts).
      const filter = this.filters[dim];
      if (filter && filter.min !== undefined) {
        const newLo = Math.max(min, filter.min);
        const newHi = Math.min(max, filter.max);
        if (newLo >= newHi) {
          this.dimensions[dim].filterAll();
          delete this.filters[dim];
          card._setValues(min, max);
        } else if (newLo !== filter.min || newHi !== filter.max) {
          this.dimensions[dim].filterFunction(
            v => typeof v === 'number' && v >= newLo && v <= newHi
          );
          this.filters[dim] = { ...filter, min: newLo, max: newHi };
          card._setValues(newLo, newHi);
        }
      }
    }

    // -- Filter application -------------------------------------------------

    // `silent: true` applies the filter in the browser (re-filters crossfilter,
    // repaints bars / counts / status) but does NOT push to R. Used while a
    // range/date slider is being dragged so the in-block preview stays live
    // without re-evaluating downstream blocks on every intermediate value; the
    // slider's `change` (drag-end) handler submits the resting value.
    _applyFilter(dim, value, { silent = false } = {}) {
      if (value === null) {
        this.dimensions[dim].filterAll();
        delete this.filters[dim];
      } else if (Array.isArray(value)) {
        // `value` is label strings (from clicks, R pushes, or the debug
        // API); the predicate maps them to row codes for encoded dims.
        this.dimensions[dim].filterFunction(this._makeCatPredicate(dim, value));
        this.filters[dim] = value;
      } else if (value.min !== undefined) {
        if (value.isDate) {
          // Date dim values are epoch-days (see the dimension accessor); NaN
          // (missing) fails the comparison and is excluded, matching R.
          const loD = value.min, hiD = value.max;
          this.dimensions[dim].filterFunction(
            v => typeof v === 'number' && v >= loD && v <= hiD
          );
        } else {
          // Inclusive on both ends (matches R's `>= lo & <= hi`); also
          // robust against non-numeric (null) values that dodge the sort.
          const lo = value.min, hi = value.max;
          this.dimensions[dim].filterFunction(
            v => typeof v === 'number' && v >= lo && v <= hi
          );
        }
        this.filters[dim] = value;
      }

      this._syncSiblingKeys();
      this._updateAllCounts(dim, { silent });
      this._updateStatus();
      if (!silent) this._scheduleSubmit();
    }

    _clearFilter(dim) {
      this._applyFilter(dim, null);
      this._resetRangeCardValues(dim);
    }

    // A range card that has just been reset must SHOW the full range: the
    // filter is gone, so leaving the thumbs (and the labels that read them)
    // where they were says the card is still cutting when it is not.
    _resetRangeCardValues(dim) {
      const card = this.panels[dim];
      if (!card || !card._setValues) return;
      card._setValues(card._min, card._max);
    }

    _resetAllFilters() {
      for (const dim of Object.keys(this.filters)) {
        this.dimensions[dim].filterAll();
      }
      const cleared = Object.keys(this.panels || {});
      this.filters = {};
      for (const dim of cleared) this._resetRangeCardValues(dim);
      this._syncSiblingKeys();
      this._updateAllCounts(null);
      this._updateStatus();
      this._scheduleSubmit();
    }

    // -- Sibling key synchronization ----------------------------------------

    _tableHasActiveFilter(tbl) {
      for (const [dim, src] of Object.entries(this.dimSource)) {
        if (src === tbl && this.filters[dim] != null) return true;
      }
      return false;
    }

    _syncSiblingKeys() {
      const tables = Object.keys(this.instances);

      // Phase 1: clear all keyDim filters so reads reflect pre-sync state.
      for (const t of tables) {
        if (this.keyDims[t]) this.keyDims[t].filterAll();
      }

      // Phase 2: snapshot each filtered table's USUBJID set BEFORE mutating
      // any keyDims. Without this, a subsequent target's read of source N's
      // allFiltered() would see N's freshly-applied keyDim from a previous
      // iteration, cascading the intersection too far (190 → 7 in the
      // safetyData ADaM test). Only consider sources with an active filter —
      // otherwise a child missing some parent keys (adae has 225/254
      // USUBJIDs) would prune the parent even with no filters set.
      const sourceKeys = {};
      for (const source of tables) {
        if (!this._tableHasActiveFilter(source)) continue;
        const fkCol = this.childFkCols[source];
        if (!fkCol) continue;
        const filtered = this.instances[source].allFiltered();
        sourceKeys[source] = new Set(filtered.map(r => r[fkCol]));
      }

      // The subjects the header counts as kept: those every filtered table
      // keeps. null while no table filters, which reads as all of them.
      let kept = null;
      for (const keys of Object.values(sourceKeys)) {
        kept = kept ? new Set([...kept].filter(k => keys.has(k))) : keys;
      }
      this._keptKeys = kept;

      // Phase 3: apply intersections.
      for (const target of tables) {
        let allowedKeys = null;
        for (const [source, keys] of Object.entries(sourceKeys)) {
          if (source === target) continue;
          allowedKeys = allowedKeys
            ? new Set([...allowedKeys].filter(k => keys.has(k)))
            : keys;
        }
        if (allowedKeys && this.keyDims[target]) {
          this.keyDims[target].filterFunction(k => allowedKeys.has(k));
        }
      }
    }

    // -- Count updates ------------------------------------------------------

    // changedDim: the dim whose filter just changed (skip its own bounds
    // update — its filter doesn't affect its own all-but-self bounds).
    // Pass null to refresh all bounds (e.g., after a global reset).
    // `silent` is a drag in progress (see _applyFilter). The other cards'
    // gray curves do move while a slider is dragged, but rebuilding them is a
    // KDE per card per frame; they catch up in the slider's `change` handler.
    _updateAllCounts(changedDim, { silent = false } = {}) {
      // Iterate dimensions, not groups: range/date dims have no group (see
      // setData), only categorical dims do.
      // Update range bounds first so range cards show new min/max before
      // their blue KDE / count text refresh in _updateRangeInfo.
      for (const dim of Object.keys(this.dimensions)) {
        const type = this._getDimType(dim);
        if ((type === 'range' || type === 'date') && dim !== changedDim) {
          this._updateRangeBounds(dim, silent);
        }
      }
      for (const dim of Object.keys(this.dimensions)) {
        const type = this._getDimType(dim);
        if (type === 'range' || type === 'date') {
          this._updateRangeInfo(dim);
          continue;
        }
        this._renderCategoricalCounts(dim, this.groups[dim].all());
      }
    }

    _updateRangeInfo(dim) {
      const card = this.panels[dim];
      if (!card || !card._infoEl) return;
      const childTable = this.dimChild[dim];
      if (!childTable) return;

      const filteredRows = this.instances[childTable].allFiltered();
      const total = card._totalRows || 0;
      card._infoEl.textContent = `${fmtCount(filteredRows.length)} of ${fmtCount(total)} rows`;

      // Update the blue overlay: the gray curve, cut at the handles. See the
      // density helpers for why this is not a KDE of the filtered rows.
      if (card._pathFiltered && card._dim && card._kdeMaxY && card._kdeGrid) {
        const own = this.filters[card._dim];
        const lo = own && own.min !== undefined ? own.min : card._min;
        const hi = own && own.max !== undefined ? own.max : card._max;
        card._pathFiltered.setAttribute('d',
          kdeToSvgPath(clipGrid(card._kdeGrid, lo, hi), card._min, card._max,
            card._kdeMaxY, 300, 80));
      }
    }

    // The clause Reset all would undo, for its tooltip, in column names as
    // everything on the board is: "SEX = F; AGE 54 to 89". Nothing while no
    // filter is on.
    _filterClause() {
      const parts = [];
      for (const [dim, v] of Object.entries(this.filters)) {
        if (Array.isArray(v)) {
          if (!v.length) continue;
          const shown = v.map(x => x === '__NA__' ? '(NA)'
            : x === '__EMPTY__' ? '(empty)' : x);
          parts.push(`${dim} = ${shown.join(', ')}`);
        } else if (v && v.min !== undefined) {
          const fmt = v.isDate ? fmtDate : fmtNum;
          parts.push(`${dim} ${fmt(v.min)} to ${fmt(v.max)}`);
        }
      }
      return parts.length ? parts.join('; ') : null;
    }

    // -- Status bar ---------------------------------------------------------

    _updateStatus() {
      const childTables = Object.keys(this.instances);
      const nFilters = Object.keys(this.filters).length;

      // The pill row carries the same fact one level up, as an accent edge.
      this._syncShelfState();

      // Counted in subjects when R sent the parent's size, in lookup rows
      // otherwise (no single parent table to count).
      let kept, total, unit;
      if (this.parentN != null) {
        total = this.parentN;
        kept = this._keptKeys ? this._keptKeys.size : total;
        unit = this.subjectUnit;
      } else {
        total = 0;
        kept = 0;
        for (const ct of childTables) {
          total += this.instances[ct].size();
          kept += this.instances[ct].allFiltered().length;
        }
        unit = 'rows';
      }

      // One element in one place: the grey count while nothing is filtered,
      // Reset all carrying "kept of total" while anything is.
      if (nFilters > 0) {
        const label = `${fmtCount(kept)} of ${fmtCount(total)} ${unit}`;
        this.resetLabelEl.textContent = label;
        this.resetBtn.setAttribute('aria-label', `${label}. Reset all filters`);
        this.resetBtn.style.display = '';
        this.statusEl.style.display = 'none';
        return;
      }
      this.resetBtn.style.display = 'none';
      this.statusEl.style.display = '';

      if (childTables.length === 0) {
        this.statusEl.textContent = '';
        return;
      }
      this.statusEl.textContent = this.parentN != null
        ? `${fmtCount(total)} ${unit}`
        : `${fmtCount(total)} rows` +
          (childTables.length > 1 ? ` in ${childTables.length} tables` : '');
    }

    // -- Shiny communication ------------------------------------------------

    getValue() {
      // CONTRACT EDGE: everything published to R is label STRINGS (incl.
      // the __NA__/__EMPTY__ sentinels) and numeric ranges -- never the
      // dictionary codes. R state (r_filters) drives the dm-filter mirror
      // and is persisted in saved boards; `this.filters` already holds
      // labels, so no decode is needed here.
      //
      // Until setData() has run at least once, `this.filters` is the
      // empty default `{}` — not the user's intent, not the R-shipped
      // initial state. Returning null here keeps Shiny from posting an
      // empty `input$crossfilter_input` that would clobber any
      // r_filters loaded at block construction (board restore /
      // MCP-driven apply_configure).
      if (!this._ready) return null;
      const cat_filters = {};
      const rng_filters = {};

      for (const [dim, value] of Object.entries(this.filters)) {
        const sourceTable = this.dimSource[dim];
        if (!sourceTable) continue;
        if (Array.isArray(value)) {
          if (!cat_filters[sourceTable]) cat_filters[sourceTable] = {};
          cat_filters[sourceTable][dim] = value;
        } else if (value && value.min !== undefined) {
          if (!rng_filters[sourceTable]) rng_filters[sourceTable] = {};
          rng_filters[sourceTable][dim] = [value.min, value.max];
        }
      }

      // Always return an object, never null — Shiny's observeEvent on the
      // R side ignores null by default, which would otherwise drop the
      // "all filters cleared" state and leave r_filters stale.
      return { cat_filters, rng_filters };
    }

    _scheduleSubmit() {
      // setExternalFilters() flips this true while it mirrors an
      // R-side state into the UI. Skipping the change event keeps the
      // round-trip from re-firing input$crossfilter_input with a value
      // R already owns.
      if (this._suppressSubmit) return;
      if (this._submitTimer) clearTimeout(this._submitTimer);
      this._submitTimer = setTimeout(() => {
        $(this.el).trigger('change');
      }, 100);
    }
  }

  // =========================================================================
  // Shiny InputBinding
  // =========================================================================

  const binding = new Shiny.InputBinding();
  Object.assign(binding, {
    find: (scope) => $(scope).find('.js-crossfilter-container'),
    getValue: (el) => el._block ? el._block.getValue() : null,
    subscribe: (el, callback) => { $(el).on('change.jscf', () => callback()); },
    unsubscribe: (el) => { $(el).off('.jscf'); },
    initialize: (el) => {
      el._block = new CrossfilterBlock(el);

      const announce = () => {
        if (!window.Shiny || !Shiny.setInputValue) return;
        readyCounter += 1;
        Shiny.setInputValue(el.id + '_ready', readyCounter, { priority: 'event' });
      };

      window._blockrCfBound = window._blockrCfBound || {};
      const rebind = !!window._blockrCfBound[el.id];
      window._blockrCfBound[el.id] = true;

      // Re-bind: an id this page has bound before means the element was
      // recreated and its JS state (el._block data) is gone, so ask R to
      // re-ship its CACHED payload (no lookup rebuild) right away. A full page
      // reload resets this registry AND starts a fresh Shiny session, whose
      // normal push path applies again.
      if (rebind) {
        announce();
        return;
      }

      // First bind. R's regular data push normally covers the initial ship,
      // and announcing here would double-ship the payload at boot -- so this
      // is a RESCUE that cancels itself once that push lands.
      //
      // It has to exist because of the deferred dock panel: this script ships
      // WITH the panel, so a block whose view is not active at startup has no
      // registered handler when R pushes at boot, and Shiny drops such
      // messages silently. No client-side queue can catch a message that
      // arrived before the queue's own script, so the block stayed blank for
      // the rest of the session -- even after the user switched to its view.
      // In that case nothing ever arrives here, the timer fires, and R
      // re-ships. Cost when the push is merely slow: none, because R only
      // answers the announce if it already has a payload cached.
      setTimeout(() => {
        if (el._block && !el._block._dataSeen && document.contains(el)) {
          announce();
        }
      }, 1000);
    }
  });

  Shiny.inputBindings.register(binding, 'blockr.jscrossfilter');

  // =========================================================================
  // Custom message handler
  // =========================================================================

  Shiny.addCustomMessageHandler('js-crossfilter-data', (msg) => {
    const el = document.getElementById(msg.id);
    if (el && el._block) {
      el._block.setData(msg);
    } else {
      // Element not bound yet: wait for it briefly. Past the give-up the
      // binding's own rescue announce takes over, and R answers that one with
      // the filter state refreshed from its live reactives.
      let attempts = 0;
      const t = setInterval(() => {
        attempts++;
        const el2 = document.getElementById(msg.id);
        if (el2 && el2._block) { el2._block.setData(msg); clearInterval(t); }
        if (attempts > 50) clearInterval(t);
      }, 100);
    }
  });

  // Filter-only update — emitted when R-side state (r_filters /
  // r_range_filters) changes WITHOUT a lookup rebuild (the common
  // AI-assistant case: writes to a state reactiveVal directly).
  Shiny.addCustomMessageHandler('js-crossfilter-filters', (msg) => {
    const el = document.getElementById(msg.id);
    if (el && el._block && el._block._ready) {
      el._block.setExternalFilters(msg.cat_filters, msg.rng_filters);
    }
  });

})();
