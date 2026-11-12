// Audit of a live crossfilter block, for the browser tests
// (test-crossfilter-range-cards.R). window.__audit() returns one line per
// thing a card shows, or the block holds, that does not follow from the
// filters in force: a range card's scale, row count, handles, thumbs, labels
// and both curves, a categorical card's counts, and the header.
//
// The reference is a recount of the block's own rows: a row passes when
// every filter in block.filters keeps it, and a card is measured with its
// own filter left out, as the block says it does.

(function () {
  const tick = setInterval(() => {
    const el = document.querySelector('.js-crossfilter-container');
    if (el && el._block) { window.__cfDebug = el._block; clearInterval(tick); }
  }, 100);

  // Copies of the block's private density helpers.
  function kde(values, min, max, nGrid = 64) {
    if (values.length === 0) {
      return Array.from({ length: nGrid }, (_, i) => ({
        x: min + i * ((max - min) / (nGrid - 1)), y: 0
      }));
    }
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
  function path(grid, min, max, maxY, W, H) {
    if (maxY <= 0 || !grid || !grid.length) return '';
    const sx = (x) => ((x - min) / (max - min)) * W;
    const sy = (y) => H - (y / maxY) * H * 0.9;
    let d = `M${sx(grid[0].x).toFixed(1)},${H}`;
    for (const p of grid) d += ` L${sx(p.x).toFixed(1)},${sy(p.y).toFixed(1)}`;
    d += ` L${sx(grid[grid.length - 1].x).toFixed(1)},${H} Z`;
    return d;
  }
  function clip(grid, lo, hi) {
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
  const fmtCount = (n) => n >= 1e6 ? (n / 1e6).toFixed(1) + 'M'
    : n >= 1e3 ? (n / 1e3).toFixed(1) + 'K' : String(n);
  const near = (a, b, tol) => Math.abs(a - b) <= tol;
  const r3 = (x) => Math.round(x * 1000) / 1000;

  window.__table = 'pt';
  // 'recount': one table, a row passes when every filter keeps it.
  // 'engine': several tables. The rows a card can see are then read from
  // the block's own crossfilter with the card's filter lifted, so the audit
  // checks what the cards draw against the engine, not the engine itself
  // (test-crossfilter-mirrors-dm-filter.R checks the engine against dm).
  window.__ref = 'recount';

  function reference() {
    const b = window.__cfDebug;
    const rows = b.instances[window.__table].all();
    const pass = (r, except) => {
      for (const [d, f] of Object.entries(b.filters)) {
        if (d === except) continue;
        if (Array.isArray(f)) {
          if (!f.includes(b._decodeStr(d, r[d]))) return false;
        } else {
          const v = r[d];
          if (!(typeof v === 'number' && v >= f.min && v <= f.max)) return false;
        }
      }
      return true;
    };
    return { b, rows, pass };
  }

  window.__audit = function () {
    const { b, rows, pass } = reference();
    const bad = [];
    const passAll = rows.filter(r => pass(r, null)).length;

    const engine = window.__ref === 'engine';
    const reachOf = (dim) => {
      if (!engine) return { others: rows.filter(r => pass(r, dim)), all: rows, passAll };
      const cf = b.instances[b.dimChild[dim]];
      const saved = b.filters[dim];
      if (saved != null) b.dimensions[dim].filterAll();
      const others = cf.allFiltered().slice();
      if (saved != null) b._reapplyFilter(dim, saved);
      return { others, all: cf.all(), passAll: cf.allFiltered().length };
    };

    for (const [dim, card] of Object.entries(b.panels)) {
      const reach = reachOf(dim);
      const others = reach.others;

      if (!card.classList.contains('dm-cf-range-card')) {
        // Categorical: every level's count on screen is the recount.
        const want = new Map();
        for (const r of others) {
          const k = b._decodeStr(dim, r[dim]);
          want.set(k, (want.get(k) || 0) + 1);
        }
        for (const row of card.querySelectorAll('.dm-cf-tw-row')) {
          const got = row.querySelector('.dm-cf-tw-bar-label').textContent;
          const exp = fmtCount(want.get(row.dataset.value) || 0);
          if (got !== exp) bad.push(`${dim}: level ${row.dataset.value} shows ${got}, recount ${exp}`);
        }
        continue;
      }

      const vals = others.map(r => r[dim]).filter(v => typeof v === 'number' && isFinite(v));
      const own = b.filters[dim];
      const tag = `${dim}`;

      if (!card._inputLo) {
        // A card built while its column held one value: it has no slider.
        const distinct = new Set(vals).size;
        if (distinct > 1) bad.push(`${tag}: card has no slider, but ${distinct} distinct values are in reach`);
        continue;
      }

      // The scale: the values in reach, or the column's whole range when
      // they are one value or none.
      const colVals = reach.all.map(r => r[dim]).filter(v => typeof v === 'number' && isFinite(v));
      let expMin = vals.length ? Math.min(...vals) : null;
      let expMax = vals.length ? Math.max(...vals) : null;
      let why = 'recount';
      if (expMin === null || expMin === expMax) {
        why = vals.length ? 'one value in reach, so the column range' : 'no rows in reach, so the column range';
        expMin = Math.min(...colVals);
        expMax = Math.max(...colVals);
      }
      const tolB = 1e-6;
      if (!(near(card._min, expMin, tolB) && near(card._max, expMax, tolB))) {
        bad.push(`${tag}: scale ${r3(card._min)}..${r3(card._max)}, ${why} ${r3(expMin)}..${r3(expMax)}`);
      }

      // "N of M rows"
      const info = card._infoEl ? card._infoEl.textContent : '';
      const expInfo = `${fmtCount(reach.passAll)} of ${fmtCount(others.length)} rows`;
      if (info !== expInfo) bad.push(`${tag}: info "${info}", recount "${expInfo}"`);

      // A filter may reach past the scale; its thumb, its label and the blue
      // curve then stop at the end.
      const within = (v) => Math.min(card._max, Math.max(card._min, v));

      // The handles
      if (own) {
        if (!(near(card._lo, own.min, tolB) && near(card._hi, own.max, tolB))) {
          bad.push(`${tag}: handles ${r3(card._lo)}..${r3(card._hi)}, filter ${r3(own.min)}..${r3(own.max)}`);
        }
        if (own.min > own.max) bad.push(`${tag}: filter is inverted ${r3(own.min)}..${r3(own.max)}`);
      } else if (!(near(card._lo, Math.min(...colVals), tolB) && near(card._hi, Math.max(...colVals), tolB))) {
        bad.push(`${tag}: no filter, but the handles hold ${r3(card._lo)}..${r3(card._hi)}, not the column's ends`);
      }

      // The thumbs sit where the values are, to one step.
      const step = Number(card._inputLo.step) || 0;
      const dLo = Number(card._inputLo.value), dHi = Number(card._inputHi.value);
      if (!near(dLo, within(card._lo), step + 1e-6)) bad.push(`${tag}: lower thumb at ${r3(dLo)}, value ${r3(card._lo)} (step ${r3(step)})`);
      if (!near(dHi, within(card._hi), step + 1e-6)) bad.push(`${tag}: upper thumb at ${r3(dHi)}, value ${r3(card._hi)} (step ${r3(step)})`);

      // The labels
      const labels = Array.from(card.querySelectorAll('.dm-cf-range-edit')).map(x => x.textContent);
      const expLabels = [card._fmtVal(within(card._lo)), card._fmtVal(within(card._hi))];
      if (labels.join('|') !== expLabels.join('|')) bad.push(`${tag}: labels ${labels.join('..')}, values ${expLabels.join('..')}`);

      // The curves: gray is every row the other filters leave, blue is gray
      // cut at the handles.
      if (card._max > card._min && card._pathAll) {
        const grid = kde(vals, card._min, card._max);
        const maxY = Math.max(...grid.map(p => p.y), 1);
        const expGray = vals.length ? path(grid, card._min, card._max, maxY, 300, 80) : '';
        if ((card._pathAll.getAttribute('d') || '') !== expGray) bad.push(`${tag}: gray curve is not the density of the ${vals.length} rows in reach`);
        const expBlue = vals.length ? path(clip(grid, within(card._lo), within(card._hi)), card._min, card._max, maxY, 300, 80) : '';
        if ((card._pathFiltered.getAttribute('d') || '') !== expBlue) bad.push(`${tag}: blue curve is not the gray one cut at ${r3(card._lo)}..${r3(card._hi)}`);
      }
    }

    // The header
    const any = Object.keys(b.filters).length > 0;
    const shown = b.resetBtn.style.display !== 'none';
    if (any !== shown) bad.push(`header: ${Object.keys(b.filters).length} filters, Reset all ${shown ? 'shown' : 'hidden'}`);
    if (shown && !engine) {
      const exp = `${fmtCount(passAll)} of ${fmtCount(rows.length)} rows`;
      if (b.resetLabelEl.textContent !== exp) bad.push(`header: "${b.resetLabelEl.textContent}", recount "${exp}"`);
    }
    return bad;
  };

  // What a user does.
  window.__drag = function (dim, which, value, opts) {
    const card = window.__cfDebug.panels[dim];
    const inp = which === 'lo' ? card._inputLo : card._inputHi;
    inp.value = value;
    inp.dispatchEvent(new Event('input', { bubbles: true }));
    if (!opts || opts.release !== false) inp.dispatchEvent(new Event('change', { bubbles: true }));
  };
  window.__type = function (dim, which, text) {
    const card = window.__cfDebug.panels[dim];
    card.querySelectorAll('.dm-cf-range-edit')[which === 'lo' ? 0 : 1].click();
    const inp = card.querySelector('.dm-cf-range-input');
    inp.value = text;
    inp.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter', bubbles: true }));
  };
  window.__tick = function (dim, value) {
    const card = window.__cfDebug.panels[dim];
    Array.from(card.querySelectorAll('.dm-cf-tw-row'))
      .find(r => r.dataset.value === value).click();
  };
  window.__filters = function () {
    const out = {};
    for (const [d, f] of Object.entries(window.__cfDebug.filters)) {
      out[d] = Array.isArray(f) ? f.slice() : { min: r3(f.min), max: r3(f.max) };
    }
    return out;
  };
  window.__card = function (dim) {
    const c = window.__cfDebug.panels[dim];
    return { min: r3(c._min), max: r3(c._max), lo: r3(c._lo), hi: r3(c._hi),
      step: Number(c._inputLo && c._inputLo.step), sliderMax: Number(c._inputHi && c._inputHi.max),
      info: c._infoEl ? c._infoEl.textContent : null };
  };
})();
