# blockr.dm cleanup TODO

Status: not started. No urgency, captured so the reasoning is not lost.

## Context

`blockr.dm` is the relational / multi-table data layer of the ecosystem:
build a `dm`, pull / join / flatten, and filter across linked tables. Its
identity is that capability, not "wraps dm". The heavy transitive
dependency (`dm` -> `igraph`) is accepted and stays. The architectural
rule that follows from accepting it: **`blockr.dm` is the only package
that may `Imports: dm`. Every other blockr package operates on plain data
frames.** Keeping that boundary real is what keeps the rest of the
ecosystem installable without igraph.

## Principle: where the crossfilter / dm seam is

The crossfilter block builds lookup tables in R and ships them to
crossfilter2.js. Some of that work resembles what `dm` does. The test for
"reuse dm or not" is not "does it look similar" but "is this dm's actual
abstraction".

Split the work in two:

1. **Schema / topology reasoning** — which tables link to which, via which
   key columns, in which direction, the cascade order when a filter in
   table A must narrow table B, join-path selection when multiple paths
   exist. This *is* dm's core competency (and the reason it needs igraph).
   Cost scales with the schema (tiny, row-count independent).

2. **Data-volume materialization** — per-dimension indices, marginal
   counts, the columnar payload crossfilter2.js consumes in the browser.
   `dm` has no notion of this. It only resembles dm because both involve
   "tables and keys". Cost scales with rows.

### Decision rule

> Delegate the schema / topology reasoning to dm. Keep the data-volume
> materialization local to the crossfilter block.

A useful tell: schema work is row-count independent; payload work scales
with rows. Different cost classes is a strong signal they are different
concerns that merely share vocabulary.

### Caveat: depend on dm, not igraph

Leverage dm's *public API* for relationships (FK accessors, and dm's own
filter / path functions where behaviour should match `dm_filter`). Do not
reach into `igraph` directly from the crossfilter block. That would
re-spread the heavy dependency into a block that should stay light and
couple it to dm internals. dm owns igraph; the crossfilter block owns the
JS payload; the seam between them is dm's exported relationship API.

## Concrete actions (when this is picked up)

- [ ] Audit `blockr.dm/R/crossfilter-block.R` and
      `crossfilter-backends.R` for any hand-rolled FK-graph
      interpretation (deriving linkage / cascade order / join paths from
      key columns by hand). That is a second, divergent interpretation of
      relational semantics sitting next to dm's.
- [ ] When the crossfilter input is a `dm`: get linkage and cascade from
      dm's public API, then build the JS lookups from that. When the
      input is a plain data frame or user-declared relations: no dm is
      involved, nothing to delegate, just build the lookups.
- [ ] Confirm no `igraph` symbol is referenced outside what `dm` itself
      pulls in (crossfilter block must not `Imports: igraph`).
- [ ] Grep the workspace: `blockr.dm` should be the only `Imports: dm`.
      Anything else importing dm is a boundary leak to close.

## Related: move and rename `bi_filter` (DONE 2026-05-18)

Executed. `new_value_filter_block` now lives in `blockr.dm`
(`R/value-filter-block.R`, JS/CSS as `inst/{js,css}/value-filter-block.*`,
registered, roxygen exported). `blockr.bi` keeps non-forwarding
`.Defunct` stubs for `new_bi_filter_block` / `migrate_bi_filter_state`
pointing to the new names; `dm` dropped from `blockr.bi` Suggests; test
and `filter-demo` examples moved to `blockr.dm`; the ~6 referencing
demos/examples updated. Moved test suite passes. The crossfilter
delegation items above remain open.

Original decision record follows.

## Related: move and rename `bi_filter` (decided)

`blockr.bi/R/filter-block.R` (`new_bi_filter_block`) is a value-selection
filter (pick columns behind the gear, pick values, single/multi). It has
a real, implemented `dm` path: `dm::dm_get_tables`, `dm::dm_filter()`, FK
cascade, an `is_dm` branch shipped to JS. So `blockr.bi` genuinely
depends on `dm` today.

Key facts that shaped the decision:

- The `bi_` prefix was never meaningful. It only avoided a clash with
  `blockr.dplyr`'s `new_filter_block` (an *expression* filter). It does
  not collide with anything in `blockr.dm`.
- "Usable in a dashboard" does not mean "must live in the dashboard
  package". A dashboard is a board / dock layout that composes blocks
  across packages. A `blockr.dm` block dropped on a board is fully
  usable in a dashboard. The containment rule is about *imports*, not
  about which blocks may appear on a board.
- A value filter on a `dm` is genuinely useful in dashboards, so making
  it single-frame-only would destroy real value. It must keep its dm
  path.
- **Precedent: `crossfilter`.** It already accepts df *or* dm, lives in
  `blockr.dm`, and dashboards use it by depending on `blockr.dm`. The
  value filter is the same shape of thing; treating it differently would
  be the inconsistency.

### Principle: one block vs two is decided by shared complexity

The criterion for splitting a df-capable and dm-capable block into two
is NOT "df capability vs dm capability". It is **how much of the
implementation is shared**.

- `crossfilter`: the df and dm paths are ~90% the same machinery
  (lookup-table construction, JS payload, counting, UI). The dm part is
  a thin relational-topology cap on top. Splitting duplicates ~90% of
  the code to isolate 10%, and creates two drift surfaces. So it is
  correctly ONE block branching internally on input type. The reason
  there is no lean df-only twin is not "not needed yet", it is "a twin
  would be ~90% copy".
- value filter: same logic, smaller ratio (roughly 70/30). The bulk is
  the value-selection UI and state; the dm part is the extra
  `dm_filter` cascade on the apply step. The shared majority is exactly
  what a split would duplicate.

Rule: one block that branches on `is.data.frame` vs `inherits(dm)`. Two
blocks (or a shared-core split into two registered blocks) is justified
only when the paths are genuinely different machinery, not the same
machinery with a relational cap. This closes the earlier "lean df-only
twin" and "shared-core two-block" options for both blocks.

Decision:

- Move `new_bi_filter_block` into `blockr.dm`, renamed
  `new_value_filter_block`. Keep both the data-frame and dm paths intact.
  It becomes the value-selection member of the relational filter family
  next to `new_dm_filter_block` (expression-style) and
  `new_crossfilter_block`.
- Keep `new_bi_filter_block` as a deprecating alias (`.Deprecated`) that
  forwards to `new_value_filter_block`, so existing boards and
  serialized state keep working.
- Do NOT build a lean df-only twin in the dashboard package, and do NOT
  do a shared-core two-block split. Not for YAGNI reasons but because
  the df and dm paths share most of their implementation (see the
  shared-complexity principle above); a twin would mostly be copy with
  two drift surfaces.
- After the move, `blockr.bi` no longer needs `dm` (it was a Suggest;
  now dropped). That, plus the crossfilter audit above, makes
  `blockr.dm` the sole `dm`-importing package, satisfying the
  containment rule.

Note: the `blockr.bi` -> `blockr.dashboard` rename is NOT decided. It is
parked for team discussion (ecosystem-wide: DESCRIPTION, NAMESPACE, every
`library`/`load_all`, demos, docs site, repo/CRAN identity). The filter
move above is independent of it and does not presuppose the outcome.

Migration mechanics (same regardless of options, for when picked up):

- [ ] Define `new_value_filter_block` in `blockr.dm`; register it there;
      remove the block from `blockr.bi` and drop `dm` from
      `blockr.bi` DESCRIPTION.
- [ ] `new_bi_filter_block` deprecating alias in `blockr.bi` forwarding
      to `blockr.dm::new_value_filter_block` (or relocate the alias too,
      decide at implementation time to avoid a `blockr.bi -> blockr.dm`
      dependency; an alias that just errors with a pointer is acceptable
      if the back-dependency is unwanted).
- [ ] Update the ~14 referencing files (tests, `inst/examples`,
      `blockr.insurance`, sandbox `dev/` scripts) to the new name.
- [ ] Check serialized-board / legacy-deser compatibility for the old
      `new_bi_filter_block` name (cf. how `blockr.dplyr` handles legacy
      deser for its own `new_filter_block`).
- [ ] Update docs site and any registry/catalog listings.
