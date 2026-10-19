/* The crossfilter's state changes (inst/blocks/crossfilter/crossfilter.js,
 * REDUCERS): what each change the block reports does to the state. They were
 * the R observers of the block's server; tests/testthat covers what R still
 * computes (the payload, the expression). */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const { execFileSync } = require('node:child_process');

const root = path.join(__dirname, '..', '..');

// blockr.ui's blockr-block.js, from BLOCKR_UI_JS or the installed package
const uiDir = process.env.BLOCKR_UI_JS || execFileSync(
  'Rscript', ['-e', 'cat(system.file("assets", "js", package = "blockr.ui", mustWork = TRUE))'],
  { encoding: 'utf8' }
).trim();

const context = { console };
context.window = context;
vm.createContext(context);
vm.runInContext(fs.readFileSync(path.join(uiDir, 'blockr-block.js'), 'utf8'), context);
vm.runInContext(
  fs.readFileSync(path.join(root, 'inst', 'blocks', 'crossfilter', 'crossfilter.js'), 'utf8'),
  context
);
const R = context.Blockr.blocks.crossfilter.reducers;

// what a block reports to the reducers it needs
const block = (over = {}) => Object.assign({ pinned: null, subgroup: null, groupDefs: {} }, over);
const json = (x) => JSON.parse(JSON.stringify(x));

const STATE = {
  active_dims: { adsl: ['SEX', 'ARM'] },
  filters: { adsl: { SEX: ['F'] } },
  range_filters: {},
  measure: null,
  agg_func: null,
  featured: ['ARM', 'SEX'],
  pinned: 'ARM',
  groups: {},
  subgroup: null
};

test('adding a card adds the column to its table, once', () => {
  const next = R.add_filter(STATE, { table: 'adae', dim: 'AESEV' }, block());
  assert.deepStrictEqual(json(next.active_dims), { adsl: ['SEX', 'ARM'], adae: ['AESEV'] });
  assert.strictEqual(R.add_filter(STATE, { table: 'adsl', dim: 'SEX' }, block()), null);
});

test('removing a card takes its filter with it', () => {
  const next = R.remove_filter(STATE, { table: 'adsl', dim: 'SEX' }, block());
  assert.deepStrictEqual(json(next.active_dims), { adsl: ['ARM'] });
  assert.deepStrictEqual(json(next.filters), {});
});

test('moving the pin keeps the cards and filters', () => {
  const next = R.set_pinned(STATE, 'SEX', block());
  assert.strictEqual(next.pinned, 'SEX');
  assert.deepStrictEqual(json(next.active_dims), json(STATE.active_dims));
  assert.deepStrictEqual(json(next.filters), json(STATE.filters));
});

test('an empty pick unpins without clearing the cards', () => {
  const next = R.set_pinned(STATE, '', block());
  assert.strictEqual(next.pinned, null);
  assert.deepStrictEqual(json(next.active_dims), json(STATE.active_dims));
});

test('the featured vocabulary is edited from the client', () => {
  const next = R.set_featured(STATE, ['SEX', '', 'SEX', 'AGE'], block());
  assert.deepStrictEqual(json(next.featured), ['SEX', 'AGE']);
});

test('swap trades group and subgroup as the payload resolved them', () => {
  const next = R.swap_split(STATE, undefined, block({ pinned: 'ARM', subgroup: 'SEX' }));
  assert.strictEqual(next.pinned, 'SEX');
  assert.strictEqual(next.subgroup, 'ARM');
  assert.strictEqual(R.swap_split(STATE, undefined, block({ pinned: 'ARM' })), null);
});

test('a groups edit stores the block\'s definition, a default one not', () => {
  const def = { columns: [{ name: 'Active', members: ['Low', 'High'], custom: true }] };
  const next = R.set_groups(STATE, { column: 'ARM' }, block({ groupDefs: { ARM: def } }));
  assert.deepStrictEqual(json(next.groups), { ARM: def });
  const back = R.set_groups(next, { column: 'ARM' }, block({ groupDefs: {} }));
  assert.deepStrictEqual(json(back.groups), {});
});

test('the selections become filters and range filters', () => {
  const next = R.filters(STATE, {
    cat_filters: { adsl: { ARM: ['Placebo'] } },
    rng_filters: { adsl: { AGE: [40, 60] } }
  }, block());
  assert.deepStrictEqual(json(next.filters), { adsl: { ARM: ['Placebo'] } });
  assert.deepStrictEqual(json(next.range_filters), { adsl: { AGE: [40, 60] } });
  // nothing to report before the first data
  assert.strictEqual(R.filters(STATE, null, block()), null);
});
