(() => {
  'use strict';

  // =========================================================================
  // SVG icons (blockr design system)
  // =========================================================================

  const ICON_GEAR = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16"><path d="M9.405 1.05c-.413-1.4-2.397-1.4-2.81 0l-.1.34a1.464 1.464 0 0 1-2.105.872l-.31-.17c-1.283-.698-2.686.705-1.987 1.987l.169.311c.446.82.023 1.841-.872 2.105l-.34.1c-1.4.413-1.4 2.397 0 2.81l.34.1a1.464 1.464 0 0 1 .872 2.105l-.17.31c-.698 1.283.705 2.686 1.987 1.987l.311-.169a1.464 1.464 0 0 1 2.105.872l.1.34c.413 1.4 2.397 1.4 2.81 0l.1-.34a1.464 1.464 0 0 1 2.105-.872l.31.17c1.283.698 2.686-.705 1.987-1.987l-.169-.311a1.464 1.464 0 0 1 .872-2.105l.34-.1c1.4-.413 1.4-2.397 0-2.81l-.34-.1a1.464 1.464 0 0 1-.872-2.105l.17-.31c.698-1.283-.705-2.686-1.987-1.987l-.311.169a1.464 1.464 0 0 1-2.105-.872zM8 10.93a2.929 2.929 0 1 1 0-5.86 2.929 2.929 0 0 1 0 5.858z"/></svg>';

  const ICON_RESET = '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor"><path fill-rule="evenodd" d="M8 3a5 5 0 1 0 4.546 2.914.5.5 0 1 1 .908-.418A6 6 0 1 1 8 2v1z"/><path d="M8 4.466V.534a.25.25 0 0 1 .41-.192l2.36 1.966c.12.1.12.284 0 .384L8.41 4.658A.25.25 0 0 1 8 4.466z"/></svg>';

  const ICON_REMOVE = '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor"><path d="M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z"/></svg>';

  const ICON_RESET_SM = '<svg width="12" height="12" viewBox="0 0 16 16" fill="currentColor"><path fill-rule="evenodd" d="M8 3a5 5 0 1 0 4.546 2.914.5.5 0 1 1 .908-.418A6 6 0 1 1 8 2v1z"/><path d="M8 4.466V.534a.25.25 0 0 1 .41-.192l2.36 1.966c.12.1.12.284 0 .384L8.41 4.658A.25.25 0 0 1 8 4.466z"/></svg>';

  const ICON_REMOVE_SM = '<svg width="12" height="12" viewBox="0 0 16 16" fill="currentColor"><path d="M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z"/></svg>';

  // Type icons for search results
  const TYPE_ICONS = { categorical: '\u2261', range: '#', date: '\u25f4' };

  // =========================================================================
  // Helpers
  // =========================================================================

  // Ensure value is always an array (R length-1 vectors serialize as scalars)
  function asArray(v) {
    if (v == null) return [];
    return Array.isArray(v) ? v : [v];
  }

  // Pivot columnar object {col: [...]} to array of row objects [{col: val}, ...]
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

  function el(tag, cls, html) {
    const e = document.createElement(tag);
    if (cls) e.className = cls;
    if (html !== undefined) e.innerHTML = html;
    return e;
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
      this.childFkCols = {};
      this.keyDims = {};
      this.filters = {};
      this.columnInfo = {};
      this.allColumns = {};   // full catalog for search
      this.activeDims = {};   // table -> [dim, ...]
      this.measure = '.count';
      this.aggFunc = 'sum';
      this.panels = {};
      this._submitTimer = null;
      this._buildDOM();
    }

    // -- DOM skeleton -------------------------------------------------------

    _buildDOM() {
      this.el.innerHTML = '';

      // Gear header (top-right)
      const gearHeader = el('div', 'jscf-gear-header');
      const anchor = el('div', 'jscf-popover-anchor');
      this.gearBtn = el('button', 'jscf-gear-btn', ICON_GEAR);
      this.gearBtn.type = 'button';
      this.gearBtn.title = 'Add / remove filters';
      this.gearBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._togglePopover();
      });
      anchor.appendChild(this.gearBtn);

      // Popover with search + clear
      this.popoverEl = el('div', 'jscf-popover');
      this.searchInput = el('input', 'jscf-popover-search');
      this.searchInput.type = 'text';
      this.searchInput.placeholder = 'Search columns\u2026';
      this.searchInput.autocomplete = 'off';
      this.searchInput.addEventListener('input', () => this._onSearchInput());
      this.popoverEl.appendChild(this.searchInput);
      this.searchResultsEl = el('div', 'jscf-popover-results');
      this.popoverEl.appendChild(this.searchResultsEl);

      // Measure / aggregation controls
      this._measureSection = el('div', 'jscf-measure-section');
      this._measureSection.style.display = 'none'; // shown when measures available
      const measureLabel = el('div', 'jscf-popover-label', 'Measure');
      this._measureSelect = el('select', 'jscf-popover-select');
      this._measureSelect.addEventListener('change', () => {
        this._setMeasure(this._measureSelect.value);
      });
      this._measureSection.appendChild(measureLabel);
      this._measureSection.appendChild(this._measureSelect);

      this._aggSection = el('div', 'jscf-measure-section');
      this._aggSection.style.display = 'none';
      const aggLabel = el('div', 'jscf-popover-label', 'Aggregation');
      this._aggSelect = el('select', 'jscf-popover-select');
      const aggOpts = [['sum', 'Sum'], ['mean', 'Mean']];
      for (const [val, text] of aggOpts) {
        const opt = el('option');
        opt.value = val;
        opt.textContent = text;
        this._aggSelect.appendChild(opt);
      }
      this._aggSelect.addEventListener('change', () => {
        this._setAggFunc(this._aggSelect.value);
      });
      this._aggSection.appendChild(aggLabel);
      this._aggSection.appendChild(this._aggSelect);

      this.popoverEl.appendChild(this._measureSection);
      this.popoverEl.appendChild(this._aggSection);

      // "Clear all" is now in the active dims header row
      anchor.appendChild(this.popoverEl);

      gearHeader.appendChild(anchor);
      this.el.appendChild(gearHeader);

      // Close popover on outside click
      document.addEventListener('click', (e) => {
        if (this._popoverOpen && this.popoverEl &&
            !this.popoverEl.contains(e.target) &&
            !this.gearBtn.contains(e.target)) {
          this._closePopover();
        }
      });

      // Status bar
      this.statusEl = el('div', 'dm-cf-status-bar');
      this.el.appendChild(this.statusEl);

      // Filter panels container
      this.panelsEl = el('div', 'jscf-panels');
      this.el.appendChild(this.panelsEl);
    }

    _togglePopover() {
      this._popoverOpen ? this._closePopover() : this._openPopover();
    }
    _openPopover() {
      this.popoverEl.style.display = 'block';
      this._popoverOpen = true;
      this.gearBtn.classList.add('jscf-gear-active');
      this.searchInput.value = '';
      this._onSearchInput();
      this.searchInput.focus();
    }
    _closePopover() {
      this.popoverEl.style.display = 'none';
      this._popoverOpen = false;
      this.gearBtn.classList.remove('jscf-gear-active');
    }

    // -- Search bar ----------------------------------------------------------

    _onSearchInput() {
      const query = this.searchInput.value.trim().toLowerCase();

      // Show active dims with remove buttons
      this._renderActiveDimsInPopover(query);

      // Build list of available (non-active) columns
      const results = [];
      for (const [tbl, info] of Object.entries(this.allColumns)) {
        const activeDims = asArray(this.activeDims[tbl]);
        const dims = asArray(info.dimensions);
        const rangeDims = asArray(info.range_dimensions);
        const dateDims = asArray(info.date_dimensions);
        const allDims = [...dims, ...rangeDims, ...dateDims];
        for (const dim of allDims) {
          if (activeDims.includes(dim)) continue;
          const label = (info.labels && info.labels[dim]) || '';
          const matchName = dim.toLowerCase().includes(query);
          const matchLabel = label.toLowerCase().includes(query);
          if (query === '' || matchName || matchLabel) {
            let type = 'categorical';
            if (rangeDims.includes(dim)) type = 'range';
            if (dateDims.includes(dim)) type = 'date';
            results.push({ tbl, dim, label, type });
          }
        }
      }

      this.searchResultsEl.innerHTML = '';
      if (results.length === 0) {
        this.searchResultsEl.appendChild(el('div', 'jscf-search-empty',
          query ? 'No matching columns' : 'All columns are active'));
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
            row.appendChild(el('span', 'jscf-search-item-icon',
              TYPE_ICONS[item.type] || '\u2026'));

            const nameEl = el('span', 'jscf-search-item-name', item.dim);
            if (item.label) {
              nameEl.appendChild(el('span', 'jscf-search-item-label', item.label));
            }
            row.appendChild(nameEl);

            const badgeCls = item.type === 'date' ? 'jscf-badge-date'
              : item.type === 'range' ? 'jscf-badge-numeric'
              : 'jscf-badge-categorical';
            const badgeText = item.type === 'date' ? 'Date'
              : item.type === 'range' ? 'Numeric' : 'Categorical';
            row.appendChild(
              el('span', `jscf-search-item-badge ${badgeCls}`, badgeText)
            );

            row.addEventListener('click', () => {
              this._addDimension(item.tbl, item.dim);
            });
            this.searchResultsEl.appendChild(row);
          }
        }
      }
    }

    _renderActiveDimsInPopover(query) {
      // Show/update the active dims section in the popover
      if (!this._activeDimsEl) {
        this._activeDimsEl = el('div', 'jscf-active-dims');
        // Insert before search results
        this.popoverEl.insertBefore(this._activeDimsEl, this.searchResultsEl);
      }
      this._activeDimsEl.innerHTML = '';

      const activeDims = [];
      for (const [tbl, dims] of Object.entries(this.activeDims)) {
        for (const dim of asArray(dims)) {
          activeDims.push({ tbl, dim });
        }
      }

      if (activeDims.length === 0) {
        this._activeDimsEl.style.display = 'none';
        return;
      }
      this._activeDimsEl.style.display = '';

      // Label + clear all on same line
      const headerRow = el('div', 'jscf-active-dims-header');
      headerRow.appendChild(el('span', 'jscf-active-dims-label', 'Active filters'));
      if (activeDims.length > 1) {
        const clearAllLink = el('button', 'jscf-active-dims-clear', 'Remove all');
        clearAllLink.addEventListener('click', (e) => {
          e.stopPropagation();
          this._clearAllDimensions();
        });
        headerRow.appendChild(clearAllLink);
      }
      this._activeDimsEl.appendChild(headerRow);

      const chipWrap = el('div', 'jscf-active-dims-chips');
      for (const { tbl, dim } of activeDims) {
        if (query && !dim.toLowerCase().includes(query)) continue;

        const chip = el('span', 'jscf-active-chip');
        chip.appendChild(document.createTextNode(dim));
        const removeX = el('button', 'jscf-active-chip-x', '\u00d7');
        removeX.title = `Remove ${dim}`;
        removeX.addEventListener('click', (e) => {
          e.stopPropagation();
          this._removeDimension(tbl, dim);
        });
        chip.appendChild(removeX);
        chipWrap.appendChild(chip);
      }
      this._activeDimsEl.appendChild(chipWrap);
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
      // Populate measure dropdown from allColumns
      const measures = [];
      for (const [tbl, info] of Object.entries(this.allColumns)) {
        for (const m of asArray(info.measures)) {
          measures.push({ tbl, col: m });
        }
      }

      this._measureSelect.innerHTML = '';
      const countOpt = el('option');
      countOpt.value = '.count';
      countOpt.textContent = 'Count';
      this._measureSelect.appendChild(countOpt);

      const multiTable = Object.keys(this.allColumns).length > 1;
      for (const { tbl, col } of measures) {
        const opt = el('option');
        opt.value = tbl + '.' + col;
        opt.textContent = multiTable ? tbl + '.' + col : col;
        this._measureSelect.appendChild(opt);
      }

      this._measureSelect.value = this.measure;
      this._measureSection.style.display = measures.length > 0 ? '' : 'none';
      this._aggSelect.value = this.aggFunc;
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

    // -- Receive data from R ------------------------------------------------

    setData(msg) {
      const t0 = performance.now();
      this._teardown();

      this.dimSource = msg.dim_source || {};
      this.parentKey = msg.parent_key;
      this.parentTable = msg.parent_table;
      this.childFkCols = msg.child_fk_cols || {};
      this.columnInfo = msg.column_info || {};
      this.allColumns = msg.all_columns || msg.column_info || {};
      this.activeDims = msg.active_dims || {};
      this.measure = msg.measure || '.count';
      this.aggFunc = msg.agg_func || 'sum';

      // Create crossfilter instances from columnar data
      // Shiny delivers pre-serialized json verbatim as parsed objects
      for (const [childTable, colsOrRows] of Object.entries(msg.lookups || {})) {
        let rows;
        if (typeof colsOrRows === 'string') {
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

      // Map dims to child tables
      const childTables = Object.keys(this.instances);
      for (const dim of Object.keys(this.dimSource)) {
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
        this.dimensions[dim] = cf.dimension(d => d[dim]);
        this.groups[dim] = this._createGroup(dim, sourceTable);
      }

      this._updateMeasureUI();
      this._renderActiveDimsInPopover('');
      this._buildPanels();
      this._updateAllCounts();

      const elapsed = Math.round(performance.now() - t0);
      this._lastSetDataMs = elapsed;
      console.log(`[js-crossfilter] setData: ${elapsed}ms (${childTables.length} tables, ${Object.keys(this.dimSource).length} dims)`);
      this._updateStatus();
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
      this.panels = {};
    }

    // -- Group creation (measure-aware) -------------------------------------

    _createGroup(dim, sourceTable) {
      const d = this.dimensions[dim];
      const hasMeasure = this.measure && this.measure !== '.count';
      const isParentDim = (sourceTable === this.parentTable);

      // Parse measure: "table.column" format
      let measureCol = null;
      if (hasMeasure) {
        const dot = this.measure.indexOf('.');
        measureCol = dot >= 0 ? this.measure.slice(dot + 1) : this.measure;
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

      // Group dims by source table
      const grouped = {};
      for (const dim of dims) {
        if (!this.dimensions[dim]) continue;
        const src = this.dimSource[dim];
        (grouped[src] = grouped[src] || []).push(dim);
      }

      const multiTable = Object.keys(grouped).length > 1;

      for (const [tbl, tblDims] of Object.entries(grouped)) {
        const section = el('div', 'dm-cf-table-section');

        if (multiTable) {
          section.appendChild(el('div', 'dm-cf-table-header', tbl));
        }

        const wrap = el('div', 'dm-cf-dims-wrap');

        for (const dim of tblDims) {
          const type = this._getDimType(dim);
          const card = (type === 'range' || type === 'date')
            ? this._createRangeCard(dim, tbl, type)
            : this._createCategoricalCard(dim, tbl);
          wrap.appendChild(card);
          this.panels[dim] = card;
        }

        section.appendChild(wrap);
        this.panelsEl.appendChild(section);
      }
    }

    // -- Categorical card ---------------------------------------------------

    _createCategoricalCard(dim, tbl) {
      const card = el('div', 'dm-cf-filter-card');
      card.dataset.dim = dim;

      // Header
      const header = el('div', 'dm-cf-filter-card-header');
      const labelEl = el('span', 'dm-cf-filter-card-label', dim);
      const sublabel = this._getDimLabel(dim);
      if (sublabel) {
        labelEl.appendChild(el('span', 'dm-cf-filter-card-sublabel', sublabel));
      }
      header.appendChild(labelEl);

      const actions = el('div', 'dm-cf-filter-card-actions');
      const resetBtn = el('button', 'dm-cf-reset-btn', ICON_RESET);
      resetBtn.title = 'Reset filter';
      resetBtn.addEventListener('click', () => this._clearFilter(dim));
      actions.appendChild(resetBtn);
      header.appendChild(actions);
      card.appendChild(header);

      // In-panel search
      const searchInput = el('input', 'dm-cf-tw-search');
      searchInput.type = 'text';
      searchInput.placeholder = 'Search...';
      searchInput.addEventListener('input', () => {
        const q = searchInput.value.toLowerCase();
        const rows = card.querySelectorAll('.dm-cf-tw-row');
        rows.forEach(r => {
          const val = r.dataset.value.toLowerCase();
          r.style.display = (q === '' || val.includes(q)) ? '' : 'none';
        });
      });
      card.appendChild(searchInput);

      // Scrollable table
      const scroll = el('div', 'dm-cf-tw-scroll');
      const table = el('table', 'dm-cf-tw-table');
      const thead = el('thead');
      const headRow = el('tr');
      headRow.appendChild(el('th', 'dm-cf-tw-th', dim));
      const countTh = el('th', 'dm-cf-tw-th', 'Count');
      countTh.style.width = '130px';
      headRow.appendChild(countTh);
      thead.appendChild(headRow);
      table.appendChild(thead);

      const tbody = el('tbody');
      table.appendChild(tbody);
      scroll.appendChild(table);
      card.appendChild(scroll);

      card._tbody = tbody;
      return card;
    }

    _renderCategoricalCounts(dim, counts) {
      const card = this.panels[dim];
      if (!card || !card._tbody) return;
      const tbody = card._tbody;

      const gv = (d) => this._getGroupValue(d);
      const hasMeasure = this.measure && this.measure !== '.count';

      const sorted = counts
        .filter(d => gv(d) > 0 || this._isSelected(dim, d.key))
        .sort((a, b) => gv(b) - gv(a));

      const selected = this.filters[dim];
      const selectedSet = selected ? new Set(selected) : null;
      const hasFilter = !!selectedSet;
      const maxVal = sorted.reduce((m, d) => Math.max(m, Math.abs(gv(d))), 0);

      tbody.innerHTML = '';
      for (const item of sorted) {
        const count = gv(item);
        const isSelected = selectedSet && selectedSet.has(String(item.key));
        const isDimmed = hasFilter && !isSelected;

        const tr = el('tr', 'dm-cf-tw-row' + (isDimmed ? ' dimmed' : ''));
        tr.dataset.value = String(item.key);

        // Value cell
        const tdVal = el('td');
        const displayKey = item.key === '__NA__' ? '(NA)'
          : item.key === '__EMPTY__' ? '(empty)' : String(item.key);
        if (item.key === '__NA__' || item.key === '__EMPTY__') {
          tdVal.innerHTML = `<em style="color:#9ca3af">${displayKey}</em>`;
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
        barCell.appendChild(el('span', 'dm-cf-tw-bar-label', label));
        tdBar.appendChild(barCell);
        tr.appendChild(tdBar);

        tr.addEventListener('click', () => {
          this._toggleCategorical(dim, String(item.key));
        });

        tbody.appendChild(tr);
      }
    }

    _isSelected(dim, key) {
      const sel = this.filters[dim];
      return sel && sel.includes(String(key));
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

      // Header
      const header = el('div', 'dm-cf-filter-card-header');
      const labelEl = el('span', 'dm-cf-filter-card-label', dim);
      const sublabel = this._getDimLabel(dim);
      if (sublabel) {
        labelEl.appendChild(el('span', 'dm-cf-filter-card-sublabel', sublabel));
      }
      header.appendChild(labelEl);

      const actions = el('div', 'dm-cf-filter-card-actions');
      const resetBtn = el('button', 'dm-cf-reset-btn', ICON_RESET);
      resetBtn.title = 'Reset filter';
      resetBtn.addEventListener('click', () => this._clearFilter(dim));
      actions.appendChild(resetBtn);
      header.appendChild(actions);
      card.appendChild(header);

      // Get min/max
      const cfDim = this.dimensions[dim];
      const bottom = cfDim.bottom(1)[0];
      const top = cfDim.top(1)[0];
      if (!bottom || !top) return card;

      let min = type === 'date' ? new Date(bottom[dim]).getTime() / 86400000
        : Number(bottom[dim]);
      let max = type === 'date' ? new Date(top[dim]).getTime() / 86400000
        : Number(top[dim]);

      if (isNaN(min) || isNaN(max) || min === max) {
        card.appendChild(el('div', 'dm-cf-range-info', `All values: ${type === 'date' ? fmtDate(min) : fmtNum(min)}`));
        return card;
      }

      // Row count info
      const infoEl = el('div', 'dm-cf-range-info');
      card.appendChild(infoEl);
      card._infoEl = infoEl;
      card._totalRows = cfDim.top(Infinity).length;

      // Dual range slider
      const slider = el('div', 'dm-cf-dual-range');
      slider.appendChild(el('div', 'dm-cf-dual-range-track'));
      const fillBar = el('div', 'dm-cf-dual-range-fill');
      slider.appendChild(fillBar);

      const step = type === 'date' ? 1 : (max - min) / 200;

      const inputLo = document.createElement('input');
      inputLo.type = 'range';
      inputLo.min = min; inputLo.max = max; inputLo.value = min; inputLo.step = step;
      inputLo.style.cssText = 'z-index:3;pointer-events:none;';

      const inputHi = document.createElement('input');
      inputHi.type = 'range';
      inputHi.min = min; inputHi.max = max; inputHi.value = max; inputHi.step = step;
      inputHi.style.cssText = 'z-index:4;pointer-events:none;';

      // Tooltips
      const bubbleLo = el('span', 'dm-cf-bubble');
      const bubbleHi = el('span', 'dm-cf-bubble');

      slider.appendChild(inputLo);
      slider.appendChild(inputHi);
      slider.appendChild(bubbleLo);
      slider.appendChild(bubbleHi);
      card.appendChild(slider);

      // Min/max labels
      const minMaxRow = el('div', 'dm-cf-range-minmax');
      const fmtVal = type === 'date' ? fmtDate : fmtNum;
      minMaxRow.appendChild(el('span', '', fmtVal(min)));
      minMaxRow.appendChild(el('span', '', fmtVal(max)));
      card.appendChild(minMaxRow);

      const updateSlider = () => {
        let lo = Number(inputLo.value);
        let hi = Number(inputHi.value);
        if (lo > hi) [lo, hi] = [hi, lo];

        // Fill bar
        const range = max - min;
        const loP = ((lo - min) / range) * 100;
        const hiP = ((hi - min) / range) * 100;
        fillBar.style.left = loP + '%';
        fillBar.style.width = (hiP - loP) + '%';

        // Bubbles
        bubbleLo.textContent = fmtVal(lo);
        bubbleLo.style.left = loP + '%';
        bubbleHi.textContent = fmtVal(hi);
        bubbleHi.style.left = hiP + '%';
      };
      updateSlider();

      const onInput = () => {
        let lo = Number(inputLo.value);
        let hi = Number(inputHi.value);
        if (lo > hi) [lo, hi] = [hi, lo];
        updateSlider();
        slider.classList.add('dm-cf-active');
        clearTimeout(card._activeTimer);
        card._activeTimer = setTimeout(() => slider.classList.remove('dm-cf-active'), 1500);

        const range = max - min;
        if (lo <= min + range * 0.001 && hi >= max - range * 0.001) {
          this._applyFilter(dim, null);
        } else {
          if (type === 'date') {
            this._applyFilter(dim, { min: lo, max: hi, isDate: true });
          } else {
            this._applyFilter(dim, { min: lo, max: hi });
          }
        }
      };

      inputLo.addEventListener('input', onInput);
      inputHi.addEventListener('input', onInput);

      card._min = min;
      card._max = max;
      card._type = type;

      return card;
    }

    // -- Filter application -------------------------------------------------

    _applyFilter(dim, value) {
      if (value === null) {
        this.dimensions[dim].filterAll();
        delete this.filters[dim];
      } else if (Array.isArray(value)) {
        const set = new Set(value);
        this.dimensions[dim].filterFunction(v => set.has(String(v)));
        this.filters[dim] = value;
      } else if (value.min !== undefined) {
        if (value.isDate) {
          const loD = value.min;
          const hiD = value.max;
          this.dimensions[dim].filterFunction(v => {
            const t = new Date(v).getTime() / 86400000;
            return t >= loD && t <= hiD;
          });
        } else {
          this.dimensions[dim].filterRange([value.min, value.max]);
        }
        this.filters[dim] = value;
      }

      this._syncSiblingKeys();
      this._updateAllCounts();
      this._updateStatus();
      this._scheduleSubmit();
    }

    _clearFilter(dim) {
      this._applyFilter(dim, null);
    }

    _resetAllFilters() {
      for (const dim of Object.keys(this.filters)) {
        this.dimensions[dim].filterAll();
      }
      this.filters = {};
      this._syncSiblingKeys();
      this._updateAllCounts();
      this._updateStatus();
      this._scheduleSubmit();
    }

    _clearAllDimensions() {
      // Tell R to clear all active dims
      const id = this.el.id;
      const nsBase = id.replace(/-crossfilter_input$/, '');
      Shiny.setInputValue(nsBase + '-clear_filters', Math.random(),
        { priority: 'event' });
    }

    // -- Sibling key synchronization ----------------------------------------

    _syncSiblingKeys() {
      const childTables = Object.keys(this.instances);
      if (childTables.length <= 1) return;

      for (const t of childTables) {
        if (this.keyDims[t]) this.keyDims[t].filterAll();
      }

      for (const targetChild of childTables) {
        let allowedKeys = null;
        for (const sourceChild of childTables) {
          if (sourceChild === targetChild) continue;
          const fkCol = this.childFkCols[sourceChild];
          if (!fkCol) continue;
          const filtered = this.instances[sourceChild].allFiltered();
          const keys = new Set(filtered.map(r => r[fkCol]));
          allowedKeys = allowedKeys
            ? new Set([...allowedKeys].filter(k => keys.has(k)))
            : keys;
        }
        if (allowedKeys && this.keyDims[targetChild]) {
          this.keyDims[targetChild].filterFunction(k => allowedKeys.has(k));
        }
      }
    }

    // -- Count updates ------------------------------------------------------

    _updateAllCounts() {
      for (const dim of Object.keys(this.groups)) {
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
      const total = card._totalRows || 0;
      const filtered = this.instances[childTable].allFiltered().length;
      card._infoEl.textContent = `${fmtCount(filtered)} of ${fmtCount(total)} rows`;
    }

    // -- Status bar ---------------------------------------------------------

    _updateStatus() {
      this.statusEl.innerHTML = '';
      const childTables = Object.keys(this.instances);
      if (childTables.length === 0) return;

      const filterEntries = Object.entries(this.filters);
      const multiTable = Object.keys(this.activeDims).length > 1;

      // Filter chips
      for (const [dim, value] of filterEntries) {
        const chip = el('span', 'dm-cf-filter-chip');
        const src = this.dimSource[dim];

        if (multiTable && src) {
          chip.appendChild(el('span', 'dm-cf-chip-table', src + '.'));
        }

        if (Array.isArray(value)) {
          const display = value.length <= 2
            ? value.map(v => v === '__NA__' ? 'NA' : v === '__EMPTY__' ? '(empty)' : v).join(', ')
            : value[0] + ' +' + (value.length - 1);
          chip.appendChild(document.createTextNode(dim + '=' + display));
        } else if (value && value.min !== undefined) {
          const fmt = value.isDate ? fmtDate : fmtNum;
          chip.appendChild(document.createTextNode(dim + ' [' + fmt(value.min) + '\u2013' + fmt(value.max) + ']'));
        }

        this.statusEl.appendChild(chip);
      }

      // Buttons
      if (filterEntries.length > 0) {
        const resetBtn = el('button', 'dm-cf-reset-all-btn', ICON_RESET_SM + ' Reset all');
        resetBtn.addEventListener('click', () => this._resetAllFilters());
        this.statusEl.appendChild(resetBtn);
      }

      // "Clear all" is in the gear popover, not the status bar

      // Separator + row count
      if (filterEntries.length > 0 || Object.keys(this.dimSource).length > 0) {
        this.statusEl.appendChild(el('span', 'dm-cf-separator'));
      }

      let totalRows = 0;
      let filteredRows = 0;
      for (const ct of childTables) {
        totalRows += this.instances[ct].size();
        filteredRows += this.instances[ct].allFiltered().length;
      }

      const countEl = el('span', 'dm-cf-row-count');
      if (filterEntries.length > 0) {
        countEl.textContent = `${fmtCount(filteredRows)} / ${fmtCount(totalRows)} rows`;
      } else {
        countEl.textContent = `${fmtCount(totalRows)} rows` +
          (childTables.length > 1 ? ` in ${childTables.length} tables` : '');
      }
      this.statusEl.appendChild(countEl);

      // Timing stays in console.log only — not shown in UI
    }

    // -- Shiny communication ------------------------------------------------

    getValue() {
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

      const hasFilters = Object.keys(cat_filters).length > 0 ||
                          Object.keys(rng_filters).length > 0;
      if (!hasFilters) return null;

      return { cat_filters, rng_filters };
    }

    _scheduleSubmit() {
      if (this._submitTimer) clearTimeout(this._submitTimer);
      this._submitTimer = setTimeout(() => {
        $(this.el).trigger('change');
      }, 300);
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
    initialize: (el) => { el._block = new CrossfilterBlock(el); }
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
      let attempts = 0;
      const t = setInterval(() => {
        attempts++;
        const el2 = document.getElementById(msg.id);
        if (el2 && el2._block) { el2._block.setData(msg); clearInterval(t); }
        if (attempts > 50) clearInterval(t);
      }, 100);
    }
  });

})();
