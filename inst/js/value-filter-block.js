/**
 * FilterBlock — minimal value filter for a data frame or a dm.
 *
 * Body (card body): one row per active entry, each with a Blockr.Select
 *   (single or multi depending on the entry's mode).
 * Gear popover:
 *   - df input: "Fields" multi-select picks columns; per-column mode pill.
 *   - dm input: "Table" single-select scopes the "Fields" multi-select to one
 *     table at a time. Active entries from other tables persist; the mode
 *     toggles row shows every active entry across all tables.
 *
 * State emitted (and received) matches the R-side column-object shape:
 *   { columns: [{ name, table?, mode, values }, ...] }
 *
 * Depends on: blockr-core.js, blockr-select.js (from blockr.dplyr).
 */
(() => {
  'use strict';

  const optValueOf = (o) => (typeof o === 'object' && o !== null ? o.value : o);

  // Qualified key uniquely identifies a column. For df input the column name
  // alone is unique. For dm input we need table+name because two tables can
  // share a column name (e.g. policy_id on both locations and claims).
  const qualKey = (name, table) =>
    (table != null && table !== '') ? (table + '.' + name) : name;

  class FilterBlock {
    constructor(el) {
      this.el = el;
      this.isDm = false;
      this.columns = [];     // [{value, label, table?, column?}] all columns
      this.colLabels = {};   // qualKey -> label
      this.colValues = {};   // qualKey -> options array (loaded lazily on open)
      this._loadedKeys = new Set();        // qualKeys whose values have arrived
      this._pendingValueRequests = new Set(); // qualKeys with a request in flight
      this.tables = [];      // [string] only for dm — ordered table names
      this.currentTable = ''; // dm: which table the Fields picker is scoped to
      this.entries = [];     // [{name, table?, mode, values}] active filters

      this._callback = null;
      this._submitted = false;
      this._debounceTimer = null;
      this._popoverOpen = false;

      this._fieldSelect = null;
      this._tableSelect = null;
      this._modePills = {};
      this._bodySelects = {};

      this._buildDOM();
      this._renderBody();
    }

    _autoSubmit() {
      clearTimeout(this._debounceTimer);
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'bi-filter-card';
      this.card.style.position = 'relative';
      this.el.appendChild(this.card);

      // Gear header (top-right) — same markup as blockr.dplyr blocks.
      const gearHeader = document.createElement('div');
      gearHeader.className = 'blockr-gear-header';
      this.gearBtn = document.createElement('button');
      this.gearBtn.type = 'button';
      this.gearBtn.className = 'blockr-gear-btn';
      this.gearBtn.innerHTML = Blockr.icons.gear;
      this.gearBtn.title = 'Advanced settings';
      this.gearBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._togglePopover();
      });
      gearHeader.appendChild(this.gearBtn);
      this.card.appendChild(gearHeader);

      this._buildPopover();

      this.bodyEl = document.createElement('div');
      this.bodyEl.className = 'bi-filter-body';
      this.card.appendChild(this.bodyEl);

      document.addEventListener('click', (e) => {
        if (this._popoverOpen && this.popoverEl &&
            !this.popoverEl.contains(e.target) &&
            !this.gearBtn.contains(e.target)) {
          this._closePopover();
        }
      });
    }

    _buildPopover() {
      this.popoverEl = document.createElement('div');
      this.popoverEl.className = 'blockr-popover';
      this.popoverEl.style.display = 'none';

      // Table row (dm only — hidden in df mode).
      this.tableRow = document.createElement('div');
      this.tableRow.className = 'blockr-popover-row bi-filter-table-row';
      this.tableRow.style.display = 'none';

      const tableLabel = document.createElement('label');
      tableLabel.className = 'blockr-popover-label';
      tableLabel.textContent = 'Table';
      this.tableRow.appendChild(tableLabel);

      this.tableSelectWrap = document.createElement('div');
      this.tableSelectWrap.className = 'blockr-popover-select-wrap';
      this.tableRow.appendChild(this.tableSelectWrap);

      this.popoverEl.appendChild(this.tableRow);

      // Fields multi-select.
      const fieldsRow = document.createElement('div');
      fieldsRow.className = 'blockr-popover-row';
      const fieldsLabel = document.createElement('label');
      fieldsLabel.className = 'blockr-popover-label';
      fieldsLabel.textContent = 'Fields';
      fieldsRow.appendChild(fieldsLabel);

      this.fieldsWrap = document.createElement('div');
      this.fieldsWrap.className = 'blockr-popover-select-wrap';
      fieldsRow.appendChild(this.fieldsWrap);

      this.popoverEl.appendChild(fieldsRow);

      // Per-entry mode toggles (one per active entry across all tables).
      this.modesContainer = document.createElement('div');
      this.modesContainer.className = 'bi-filter-modes';
      this.popoverEl.appendChild(this.modesContainer);

      this.card.appendChild(this.popoverEl);
    }

    _columnsForCurrentTable() {
      if (!this.isDm) return this.columns;
      const tbl = this.currentTable;
      if (!tbl) return [];
      return this.columns.filter((c) => c.table === tbl);
    }

    _selectedColumnNamesForCurrentTable() {
      if (!this.isDm) {
        return this.entries.map((e) => e.name);
      }
      const tbl = this.currentTable;
      return this.entries.filter((e) => e.table === tbl).map((e) => e.name);
    }

    _rebuildFieldsSelect() {
      // Re-create the Fields multi-select with options scoped to the current
      // table (dm mode) or all columns (df mode).
      this.fieldsWrap.innerHTML = '';
      const cols = this._columnsForCurrentTable();
      // For dm mode, the option's `value` is the bare column name (entries
      // in this picker are within the current table); label is the column
      // name plus its label if any.
      const opts = this.isDm
        ? cols.map((c) => ({ value: c.column, label: c.label || '' }))
        : cols;
      const selected = this._selectedColumnNamesForCurrentTable();
      this._fieldSelect = Blockr.Select.multi(this.fieldsWrap, {
        options: opts,
        selected: selected.slice(),
        placeholder: 'Select columns…',
        reorderable: false,
        onChange: (newNames) => {
          this._onFieldsChanged(newNames);
          this._autoSubmit();
        }
      });
    }

    _rebuildTableSelect() {
      if (!this.isDm) {
        this.tableRow.style.display = 'none';
        return;
      }
      this.tableRow.style.display = '';
      this.tableSelectWrap.innerHTML = '';
      const tblOpts = this.tables.map((t) => ({ value: t, label: t }));
      const cur = this.currentTable && this.tables.indexOf(this.currentTable) >= 0
        ? this.currentTable
        : (this.tables[0] || '');
      this.currentTable = cur;
      this._tableSelect = Blockr.Select.single(this.tableSelectWrap, {
        options: tblOpts,
        selected: cur,
        placeholder: 'Select table…',
        onChange: (t) => {
          this.currentTable = t || '';
          this._rebuildFieldsSelect();
        }
      });
    }

    _onFieldsChanged(newNamesInCurrentTable) {
      const newSet = new Set(newNamesInCurrentTable);
      const tbl = this.isDm ? this.currentTable : null;

      // Drop entries that belong to the current table but are no longer
      // selected. Keep entries from other tables intact.
      this.entries = this.entries.filter((e) => {
        const sameTable = this.isDm ? (e.table === tbl) : true;
        if (!sameTable) return true;
        return newSet.has(e.name);
      });

      // Add new entries for newly-selected names that weren't present yet.
      newNamesInCurrentTable.forEach((nm) => {
        const exists = this.entries.some((e) =>
          (this.isDm ? (e.table === tbl) : true) && e.name === nm);
        if (exists) return;
        const entry = { name: nm, mode: 'single', values: [] };
        if (this.isDm) entry.table = tbl;
        const first = this._firstValueOf(entry);
        if (first != null) entry.values = [String(first)];
        this.entries.push(entry);
      });

      this._rebuildModes();
      this._renderBody();
    }

    _rebuildModes() {
      this.modesContainer.innerHTML = '';
      this._modePills = {};

      this.entries.forEach((entry, idx) => {
        const row = document.createElement('div');
        row.className = 'blockr-popover-row bi-filter-mode-row';

        const label = document.createElement('label');
        label.className = 'blockr-popover-label';
        const display = this.isDm
          ? entry.table + '.' + entry.name
          : entry.name;
        label.textContent = this.colLabels[qualKey(entry.name, entry.table)] || display;
        row.appendChild(label);

        const pill = document.createElement('button');
        pill.type = 'button';
        pill.className = 'blockr-pill blockr-popover-toggle';
        this._stylePill(pill, entry.mode || 'single');
        pill.title = 'Toggle between single- and multi-select';
        pill.addEventListener('click', () => {
          const cur = this.entries[idx];
          const newMode = (cur.mode === 'multi') ? 'single' : 'multi';
          cur.mode = newMode;
          this._stylePill(pill, newMode);
          if (newMode === 'single' &&
              (!Array.isArray(cur.values) || cur.values.length !== 1)) {
            const first = this._firstValueOf(cur);
            cur.values = first != null ? [String(first)] : [];
          }
          this._renderBody();
          this._autoSubmit();
        });

        row.appendChild(pill);
        this._modePills[qualKey(entry.name, entry.table)] = pill;
        this.modesContainer.appendChild(row);
      });
    }

    _stylePill(pill, mode) {
      pill.textContent = mode === 'multi' ? 'Multi' : 'Single';
      pill.classList.toggle('blockr-popover-toggle-active', mode === 'multi');
    }

    _firstValueOf(entry) {
      const opts = this.colValues[qualKey(entry.name, entry.table)] || [];
      if (opts.length === 0) return null;
      return optValueOf(opts[0]);
    }

    // Lazy value loading: ask R for one column's values on first dropdown-open.
    // No-op once loaded or while a request is already in flight.
    _requestColumnValues(key) {
      if (this._loadedKeys.has(key)) return;
      if (this._pendingValueRequests.has(key)) return;
      this._pendingValueRequests.add(key);
      Shiny.setInputValue(
        this.el.id + '_request_values',
        key,
        { priority: 'event' }
      );
    }

    // R -> JS: a column's value options arrived. Store them and swap the
    // option list into the matching body select IN PLACE (a re-render would
    // destroy the just-opened dropdown), clearing its loading state.
    receiveColumnValues(key, opts) {
      this._pendingValueRequests.delete(key);
      this.colValues[key] = opts || [];
      this._loadedKeys.add(key);
      const sel = this._bodySelects[key];
      if (sel) {
        if (sel.updateOptions) sel.updateOptions(this.colValues[key]);
        if (sel.setLoading) sel.setLoading(false);
      }
    }

    _renderBody() {
      Object.values(this._bodySelects).forEach((s) => s && s.destroy && s.destroy());
      this._bodySelects = {};
      this.bodyEl.innerHTML = '';

      if (this.entries.length === 0) {
        const empty = document.createElement('div');
        empty.className = 'bi-filter-empty';
        empty.textContent = 'No filters. Click the gear to add fields.';
        this.bodyEl.appendChild(empty);
        return;
      }

      this.entries.forEach((entry, idx) => {
        const row = document.createElement('div');
        row.className = 'blockr-row bi-filter-row';

        const labelWrap = document.createElement('div');
        labelWrap.className = 'bi-filter-label-wrap';
        const label = document.createElement('label');
        label.className = 'blockr-label';
        const display = this.isDm
          ? entry.table + '.' + entry.name
          : entry.name;
        label.appendChild(document.createTextNode(display));
        const sub = this.colLabels[qualKey(entry.name, entry.table)];
        if (sub) {
          const subEl = document.createElement('span');
          subEl.className = 'bi-filter-label-sublabel';
          subEl.textContent = sub;
          label.appendChild(subEl);
        }
        labelWrap.appendChild(label);
        row.appendChild(labelWrap);

        const wrap = document.createElement('div');
        wrap.className = 'bi-filter-select-wrap';
        row.appendChild(wrap);

        const mode = entry.mode || 'single';
        const opts = this.colValues[qualKey(entry.name, entry.table)] || [];
        const sel = Array.isArray(entry.values) ? entry.values : [];

        const key = qualKey(entry.name, entry.table);
        // Values load lazily on first dropdown-open. Saved selections still
        // render (single-select falls back to the value text; multi-select
        // renders chips from `selected`) so chips show before the list lands.
        const loading = !this._loadedKeys.has(key);
        if (mode === 'single') {
          this._bodySelects[key] = Blockr.Select.single(wrap, {
            options: opts,
            selected: sel[0] != null ? sel[0] : null,
            placeholder: 'Select value…',
            loading: loading,
            onOpen: () => this._requestColumnValues(key),
            onChange: (v) => {
              const cur = this.entries[idx];
              if (v == null || v === '') {
                const first = this._firstValueOf(cur);
                cur.values = first != null ? [String(first)] : [];
              } else {
                cur.values = [String(v)];
              }
              this._autoSubmit();
            }
          });
        } else {
          this._bodySelects[key] = Blockr.Select.multi(wrap, {
            options: opts,
            selected: sel.slice(),
            placeholder: 'Select values…',
            reorderable: false,
            loading: loading,
            onOpen: () => this._requestColumnValues(key),
            onChange: (vals) => {
              this.entries[idx].values = vals.map(String);
              this._autoSubmit();
            }
          });
        }

        this.bodyEl.appendChild(row);
      });
    }

    _togglePopover() {
      this._popoverOpen ? this._closePopover() : this._openPopover();
    }

    _openPopover() {
      this.popoverEl.style.display = 'block';
      this._popoverOpen = true;
      this.gearBtn.classList.add('blockr-gear-active');
    }

    _closePopover() {
      this.popoverEl.style.display = 'none';
      this._popoverOpen = false;
      this.gearBtn.classList.remove('blockr-gear-active');
    }

    _compose() {
      // Emit the column-object state shape. Each entry carries name + table?
      // + mode + values.
      const cols = this.entries.map((e) => {
        const out = { name: e.name, mode: e.mode || 'single',
                      values: (Array.isArray(e.values) ? e.values : []).slice() };
        if (this.isDm && e.table != null && e.table !== '') out.table = e.table;
        return out;
      });
      return { columns: cols };
    }

    _submit() {
      this._submitted = true;
      if (this._callback) this._callback(true);
    }

    getValue() {
      if (!this._submitted) return null;
      return this._compose();
    }

    setState(state) {
      const colsIn = (state && state.columns) || [];
      // Accept the column-object list shape only — R-side auto-migration
      // ensures we never see the old parallel-list shape.
      this.entries = colsIn.map((e) => {
        const vals = (e && e.values != null)
          ? (Array.isArray(e.values) ? e.values.map(String) : [String(e.values)])
          : [];
        const entry = {
          name:   (e && e.name) || '',
          mode:   (e && e.mode) || 'single',
          values: vals
        };
        if (e && e.table != null && e.table !== '') entry.table = e.table;
        return entry;
      });
      this._rebuildTableSelect();
      this._rebuildFieldsSelect();
      this._rebuildModes();
      this._renderBody();
    }

    updateColumns(payload) {
      this.columns = (payload && payload.columns) || [];
      this.isDm = !!(payload && payload.is_dm);

      // A data change invalidates any cached values — they reload lazily on
      // the next dropdown-open. The startup payload carries column metadata
      // only (no value lists); that is what keeps startup cheap on a dm.
      this.colValues = {};
      this._loadedKeys.clear();
      this._pendingValueRequests.clear();

      this.colLabels = {};
      const tableSet = new Set();
      this.columns.forEach((c) => {
        const key = this.isDm
          ? qualKey(c.column, c.table)
          : (c.value || c);
        const lab = (typeof c === 'object' && c !== null && c.label) ? c.label : '';
        this.colLabels[key] = lab;
        if (this.isDm && c.table) tableSet.add(c.table);
      });
      this.tables = Array.from(tableSet);

      // Drop entries whose column or table is no longer present.
      const validKeys = new Set();
      this.columns.forEach((c) => {
        const key = this.isDm ? qualKey(c.column, c.table) : c.value;
        validKeys.add(key);
      });
      this.entries = this.entries.filter((e) =>
        validKeys.has(qualKey(e.name, e.table)));

      // Saved entry values are kept as-is: their column options are not yet
      // loaded, so there is nothing to prune against here. The R side's
      // `enforce_single_rule` fills empty single-selects and drops
      // schema-missing entries, pushing the corrected state back via
      // `bi-filter-update`.

      // For dm mode, pick a default current table if not already set.
      if (this.isDm) {
        if (!this.currentTable || this.tables.indexOf(this.currentTable) < 0) {
          this.currentTable = this.tables[0] || '';
        }
      } else {
        this.currentTable = '';
      }

      this._rebuildTableSelect();
      this._rebuildFieldsSelect();
      this._rebuildModes();
      this._renderBody();
    }
  }

  // --- Shiny input binding ---

  const binding = new Shiny.InputBinding();

  Object.assign(binding, {
    find: (scope) => $(scope).find('.bi-filter-container'),
    getId: (el) => el.id || null,
    getValue: (el) => (el._block ? el._block.getValue() : null),
    setValue: (el, value) => { if (el._block) el._block.setState(value); },
    subscribe: (el, callback) => {
      if (el._block) el._block._callback = () => callback(true);
    },
    unsubscribe: (el) => {
      if (el._block) el._block._callback = null;
    },
    initialize: (el) => {
      el._block = new FilterBlock(el);
      if (el._pendingColumns) {
        el._block.updateColumns(el._pendingColumns);
        delete el._pendingColumns;
      }
      if (el._pendingState) {
        el._block.setState(el._pendingState);
        delete el._pendingState;
      }
    },
    receiveMessage: (el, data) => {
      if (data && data.state && el._block) el._block.setState(data.state);
    }
  });

  Shiny.inputBindings.register(binding, 'blockr.bi.filter');

  const pumpColumns = (id, payload) => {
    const el = document.getElementById(id);
    if (el && el._block) {
      el._block.updateColumns(payload);
    } else if (el) {
      el._pendingColumns = payload;
    } else {
      let attempts = 0;
      const t = setInterval(() => {
        attempts++;
        const el2 = document.getElementById(id);
        if (el2 && el2._block) { el2._block.updateColumns(payload); clearInterval(t); }
        else if (el2) { el2._pendingColumns = payload; clearInterval(t); }
        if (attempts > 50) clearInterval(t);
      }, 100);
    }
  };

  Shiny.addCustomMessageHandler('bi-filter-columns', (msg) => {
    pumpColumns(msg.id, {
      columns: msg.columns,
      values: msg.values,
      is_dm: msg.is_dm
    });
  });

  Shiny.addCustomMessageHandler('bi-filter-column-values', (msg) => {
    const el = document.getElementById(msg.id);
    if (el && el._block) {
      el._block.receiveColumnValues(msg.key, msg.values);
    }
  });

  Shiny.addCustomMessageHandler('bi-filter-update', (msg) => {
    const el = document.getElementById(msg.id);
    if (el && el._block) {
      el._block.setState(msg.state);
    } else if (el) {
      el._pendingState = msg.state;
    }
  });
})();
