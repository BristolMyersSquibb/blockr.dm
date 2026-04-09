(() => {
  'use strict';

  // =========================================================================
  // CrossfilterBlock — client-side crossfilter using crossfilter2
  // =========================================================================

  class CrossfilterBlock {
    constructor(el) {
      this.el = el;
      this.instances = {};     // childTable -> crossfilter instance
      this.dimensions = {};    // dimName -> crossfilter dimension
      this.groups = {};        // dimName -> crossfilter group
      this.dimSource = {};     // dimName -> source table name
      this.dimChild = {};      // dimName -> which child table holds it
      this.parentKey = null;
      this.parentTable = null;
      this.childFkCols = {};   // childTable -> fk column name
      this.keyDims = {};       // childTable -> key dimension (sibling sync)
      this.filters = {};       // dimName -> current filter value (null = no filter)
      this.columnInfo = {};
      this.panels = {};        // dimName -> panel DOM element
      this._submitTimer = null;
      this._buildDOM();
    }

    // -- DOM skeleton -------------------------------------------------------

    _buildDOM() {
      this.el.innerHTML = '';
      this.statusEl = document.createElement('div');
      this.statusEl.className = 'jscf-status';
      this.el.appendChild(this.statusEl);

      this.gridEl = document.createElement('div');
      this.gridEl.className = 'jscf-grid';
      this.el.appendChild(this.gridEl);

      this._showEmpty();
    }

    _showEmpty() {
      this.statusEl.textContent = 'No dimensions active — add filters to start.';
      this.gridEl.innerHTML = '';
    }

    // -- Receive data from R ------------------------------------------------

    setData(msg) {
      this._teardown();

      this.dimSource = msg.dim_source || {};
      this.parentKey = msg.parent_key;
      this.parentTable = msg.parent_table;
      this.childFkCols = msg.child_fk_cols || {};
      this.columnInfo = msg.column_info || {};

      // Create crossfilter instances from lookup arrays
      for (const [childTable, rows] of Object.entries(msg.lookups || {})) {
        const cf = crossfilter(rows);
        this.instances[childTable] = cf;

        // Key dimension for sibling sync
        const fkCol = this.childFkCols[childTable];
        if (fkCol) {
          this.keyDims[childTable] = cf.dimension(d => d[fkCol]);
        }
      }

      // Map each dimension to the child table that contains it
      const childTables = Object.keys(this.instances);
      for (const [dim] of Object.entries(this.dimSource)) {
        for (const ct of childTables) {
          const cf = this.instances[ct];
          if (cf.size() > 0) {
            // Check first record for the column
            const sample = cf.all()[0];
            if (sample && dim in sample) {
              this.dimChild[dim] = ct;
              break;
            }
          }
        }
      }

      // Create crossfilter dimensions and groups
      for (const [dim, sourceTable] of Object.entries(this.dimSource)) {
        const childTable = this.dimChild[dim];
        if (!childTable) continue;
        const cf = this.instances[childTable];

        this.dimensions[dim] = cf.dimension(d => d[dim]);

        const isParentDim = (sourceTable === this.parentTable);
        if (isParentDim && this.parentKey) {
          // Distinct key count for parent-level dimensions
          const pk = this.parentKey;
          this.groups[dim] = this.dimensions[dim].group().reduce(
            (p, v) => {
              const k = v[pk];
              p.keys[k] = (p.keys[k] || 0) + 1;
              p.count = Object.keys(p.keys).length;
              return p;
            },
            (p, v) => {
              const k = v[pk];
              p.keys[k] -= 1;
              if (p.keys[k] <= 0) delete p.keys[k];
              p.count = Object.keys(p.keys).length;
              return p;
            },
            () => ({ keys: {}, count: 0 })
          );
        } else {
          this.groups[dim] = this.dimensions[dim].group().reduceCount();
        }
      }

      this._buildPanels();
      this._updateAllCounts();
      this._updateStatus();
    }

    _teardown() {
      // Dispose crossfilter dimensions
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

    // -- Panel building -----------------------------------------------------

    _buildPanels() {
      this.gridEl.innerHTML = '';

      const dims = Object.keys(this.dimSource);
      if (dims.length === 0) {
        this._showEmpty();
        return;
      }

      for (const dim of dims) {
        if (!this.dimensions[dim]) continue;
        const panel = this._createPanel(dim);
        this.gridEl.appendChild(panel);
        this.panels[dim] = panel;
      }
    }

    _getDimType(dim) {
      const source = this.dimSource[dim];
      if (!source || !this.columnInfo[source]) return 'categorical';
      const info = this.columnInfo[source];
      if (info.date_dimensions && info.date_dimensions.includes(dim)) return 'date';
      if (info.range_dimensions && info.range_dimensions.includes(dim)) return 'range';
      return 'categorical';
    }

    _getDimLabel(dim) {
      const source = this.dimSource[dim];
      if (!source || !this.columnInfo[source]) return dim;
      const labels = this.columnInfo[source].labels;
      if (labels && labels[dim] && labels[dim] !== '') {
        return labels[dim];
      }
      return dim;
    }

    _createPanel(dim) {
      const panel = document.createElement('div');
      panel.className = 'jscf-panel';
      panel.dataset.dim = dim;

      const header = document.createElement('div');
      header.className = 'jscf-panel-header';

      const title = document.createElement('span');
      title.className = 'jscf-panel-title';
      const label = this._getDimLabel(dim);
      title.textContent = label !== dim ? `${dim} — ${label}` : dim;

      const source = document.createElement('span');
      source.className = 'jscf-panel-source';
      source.textContent = this.dimSource[dim];

      const clearBtn = document.createElement('button');
      clearBtn.className = 'jscf-panel-clear';
      clearBtn.textContent = '\u00d7';
      clearBtn.title = 'Clear filter';
      clearBtn.style.display = 'none';
      clearBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._clearFilter(dim);
      });

      header.appendChild(title);
      header.appendChild(source);
      header.appendChild(clearBtn);
      panel.appendChild(header);

      const body = document.createElement('div');
      body.className = 'jscf-panel-body';
      panel.appendChild(body);

      const type = this._getDimType(dim);
      if (type === 'range' || type === 'date') {
        this._buildRangePanel(dim, body, type);
      } else {
        this._buildCategoricalPanel(dim, body);
      }

      return panel;
    }

    // -- Categorical panel --------------------------------------------------

    _buildCategoricalPanel(dim, body) {
      const list = document.createElement('ul');
      list.className = 'jscf-cat-list';
      body.appendChild(list);
      body._list = list;
    }

    _renderCategoricalCounts(dim, counts) {
      const panel = this.panels[dim];
      if (!panel) return;
      const list = panel.querySelector('.jscf-cat-list');
      if (!list) return;

      // Sort by count descending
      const getCount = (d) =>
        typeof d.value === 'object' ? d.value.count : d.value;

      const sorted = counts
        .filter(d => getCount(d) > 0 || this._isSelected(dim, d.key))
        .sort((a, b) => getCount(b) - getCount(a));

      const selected = this.filters[dim];
      const selectedSet = selected ? new Set(selected) : null;

      // Find max count for bar widths
      const maxCount = sorted.reduce((m, d) => Math.max(m, getCount(d)), 0);

      list.innerHTML = '';
      for (const item of sorted) {
        const count = typeof item.value === 'object'
          ? item.value.count : item.value;
        const li = document.createElement('li');
        li.className = 'jscf-cat-item';
        if (selectedSet && selectedSet.has(String(item.key))) {
          li.classList.add('selected');
        }
        if (count === 0) {
          li.classList.add('zero');
        }

        // Bar background
        const bar = document.createElement('div');
        bar.className = 'jscf-cat-bar';
        bar.style.width = maxCount > 0
          ? `${(count / maxCount) * 100}%` : '0%';
        li.appendChild(bar);

        const label = document.createElement('span');
        label.className = 'jscf-cat-label';
        const displayKey = item.key === '__NA__' ? '(NA)'
          : item.key === '__EMPTY__' ? '(empty)' : String(item.key);
        label.textContent = displayKey;
        li.appendChild(label);

        const countEl = document.createElement('span');
        countEl.className = 'jscf-cat-count';
        countEl.textContent = count;
        li.appendChild(countEl);

        li.addEventListener('click', () => {
          this._toggleCategorical(dim, String(item.key));
        });

        list.appendChild(li);
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

    // -- Range panel --------------------------------------------------------

    _buildRangePanel(dim, body, type) {
      // Get min/max from the dimension
      const cfDim = this.dimensions[dim];
      const bottom = cfDim.bottom(1)[0];
      const top = cfDim.top(1)[0];
      if (!bottom || !top) return;

      let min = bottom[dim];
      let max = top[dim];

      if (type === 'date') {
        // Dates come as strings from JSON
        min = new Date(min).getTime();
        max = new Date(max).getTime();
      } else {
        min = Number(min);
        max = Number(max);
      }

      if (isNaN(min) || isNaN(max) || min === max) {
        body.textContent = `All values: ${min}`;
        return;
      }

      const container = document.createElement('div');
      container.className = 'jscf-range-container';

      const labelEl = document.createElement('div');
      labelEl.className = 'jscf-range-label';
      container.appendChild(labelEl);

      const slider = document.createElement('div');
      slider.className = 'jscf-range-slider';

      const inputLo = document.createElement('input');
      inputLo.type = 'range';
      inputLo.className = 'jscf-range-lo';
      inputLo.min = min;
      inputLo.max = max;
      inputLo.value = min;
      inputLo.step = type === 'date' ? 86400000 : (max - min) / 200;

      const inputHi = document.createElement('input');
      inputHi.type = 'range';
      inputHi.className = 'jscf-range-hi';
      inputHi.min = min;
      inputHi.max = max;
      inputHi.value = max;
      inputHi.step = inputLo.step;

      slider.appendChild(inputLo);
      slider.appendChild(inputHi);
      container.appendChild(slider);
      body.appendChild(container);

      const formatVal = (v) => {
        if (type === 'date') {
          return new Date(Number(v)).toISOString().slice(0, 10);
        }
        return Number(v).toFixed(1);
      };

      const updateLabel = () => {
        labelEl.textContent = `${formatVal(inputLo.value)} — ${formatVal(inputHi.value)}`;
      };
      updateLabel();

      const onInput = () => {
        let lo = Number(inputLo.value);
        let hi = Number(inputHi.value);
        if (lo > hi) { [lo, hi] = [hi, lo]; }
        updateLabel();

        // Check if at full range (no filter)
        if (lo <= min && hi >= max) {
          this._applyFilter(dim, null);
        } else {
          if (type === 'date') {
            this._applyFilter(dim, {
              min: lo / 86400000,  // days since epoch for R
              max: hi / 86400000,
              isDate: true
            });
          } else {
            this._applyFilter(dim, { min: lo, max: hi });
          }
        }
      };

      inputLo.addEventListener('input', onInput);
      inputHi.addEventListener('input', onInput);
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
          // Date range: values in dimension are date strings
          const loMs = value.min * 86400000;
          const hiMs = value.max * 86400000;
          this.dimensions[dim].filterFunction(v => {
            const t = new Date(v).getTime();
            return t >= loMs && t <= hiMs;
          });
        } else {
          this.dimensions[dim].filterRange([value.min, value.max]);
        }
        this.filters[dim] = value;
      }

      // Update clear button visibility
      const panel = this.panels[dim];
      if (panel) {
        const clearBtn = panel.querySelector('.jscf-panel-clear');
        if (clearBtn) {
          clearBtn.style.display = value ? 'inline-block' : 'none';
        }
      }

      this._syncSiblingKeys();
      this._updateAllCounts();
      this._updateStatus();
      this._scheduleSubmit();
    }

    _clearFilter(dim) {
      this._applyFilter(dim, null);
    }

    // -- Sibling key synchronization ----------------------------------------

    _syncSiblingKeys() {
      const childTables = Object.keys(this.instances);
      if (childTables.length <= 1) return;

      for (const targetChild of childTables) {
        // Clear key dim first to avoid circular filtering
        if (this.keyDims[targetChild]) {
          this.keyDims[targetChild].filterAll();
        }
      }

      for (const targetChild of childTables) {
        let allowedKeys = null;

        for (const sourceChild of childTables) {
          if (sourceChild === targetChild) continue;
          const fkCol = this.childFkCols[sourceChild];
          if (!fkCol) continue;

          // Get keys surviving all filters on sourceChild
          const filtered = this.instances[sourceChild].allFiltered();
          const keys = new Set(filtered.map(r => r[fkCol]));

          if (allowedKeys === null) {
            allowedKeys = keys;
          } else {
            // Intersect
            allowedKeys = new Set([...allowedKeys].filter(k => keys.has(k)));
          }
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
          // Range panels don't need count updates (the slider is the UI)
          continue;
        }
        const counts = this.groups[dim].all();
        this._renderCategoricalCounts(dim, counts);
      }
    }

    _updateStatus() {
      const childTables = Object.keys(this.instances);
      if (childTables.length === 0) {
        this._showEmpty();
        return;
      }

      let totalRows = 0;
      let filteredRows = 0;
      for (const ct of childTables) {
        const cf = this.instances[ct];
        totalRows += cf.size();
        filteredRows += cf.allFiltered().length;
      }

      const nFilters = Object.keys(this.filters).length;
      if (nFilters === 0) {
        this.statusEl.textContent = `${totalRows} rows`;
      } else {
        this.statusEl.textContent =
          `${filteredRows} / ${totalRows} rows (${nFilters} filter${nFilters > 1 ? 's' : ''} active)`;
      }
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
        const el = this.el;
        $(el).trigger('change');
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
    subscribe: (el, callback) => {
      $(el).on('change.jscf', () => callback());
    },
    unsubscribe: (el) => {
      $(el).off('.jscf');
    },
    initialize: (el) => {
      el._block = new CrossfilterBlock(el);
    }
  });

  Shiny.inputBindings.register(binding, 'blockr.jscrossfilter');

  // =========================================================================
  // Custom message handler — receive lookup data from R
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
        if (el2 && el2._block) {
          el2._block.setData(msg);
          clearInterval(t);
        }
        if (attempts > 50) clearInterval(t);
      }, 100);
    }
  });

})();
