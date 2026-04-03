# blockr.dm Block Review

Status overview and plan for all 16 blocks in the package.
Test counts are `test_that()` calls per dedicated test file (serdes = serialization tests only).

## Maturity Tiers

### Tier 1: Core / Near-Final

Well-tested, heavily used, close to final shape.

| Block | File | LOC | Tests | Notes |
|-------|------|-----|-------|-------|
| **dm_crossfilter_block** | dm-crossfilter-block.R | 2394 | 27 | Flagship block. Lookup backend solid. Measure aggregation, search UI, density overlays. |
| **dm_example_block** | dm-example-block.R | 255 | 9 | Catalog of built-in datasets (BI star schema, ADaM, flights). Clean, stable. |
| **crossfilter_block** | crossfilter-block.R | 204 | 8 | Single-table crossfilter. Wraps df as one-table dm. See placement question below. |

### Tier 2: Important, Needs Love

Functional but UI/UX lags behind the crossfilter blocks. Filter blocks are the main gap.

| Block | File | LOC | Tests | What's Missing |
|-------|------|-----|-------|----------------|
| **dm_block** | dm-block.R | 543 | 5 | Core block for assembling dm objects. Key inference works. UI could be more polished. |
| **dm_filter_block** | dm-filter-block.R | 163 | 6 | Free-text R expression only. No type-aware UI, no multi-condition builder. |
| **dm_filter_value_block** | dm-filter-value-block.R | 208 | serdes | Three cascading dropdowns (table/column/value). Single value only. |
| **dm_add_keys_block** | dm-add-keys-block.R | 255 | 4 | Works but UI is basic selectInputs. |

### Tier 3: Solid Utilities

Small, focused, do one thing well. Low priority.

| Block | File | LOC | Tests | Notes |
|-------|------|-----|-------|-------|
| **dm_pull_block** | dm-pull-block.R | 100 | 5 | Extracts one table from dm. Tiny, correct. |
| **dm_flatten_block** | dm-flatten-block.R | 258 | 5 | Joins all tables via FKs. Start table + join type selection. |
| **dm_select_block** | dm-select-block.R | 135 | serdes | Subsets tables from dm. Simple multi-select. |
| **dm_nested_view_block** | dm-nested-view-block.R | 232 | serdes | Expandable reactable showing parent/child rows. Display-only. |

### Tier 4: I/O

Read/write blocks. Functional, niche.

| Block | File | LOC | Tests | Notes |
|-------|------|-----|-------|-------|
| **dm_read_block** | dm-read-block.R | 937 | serdes | Reads Excel/CSV/ZIP/RDS into dm. Large, covers many formats. |
| **dm_write_block** | dm-write-block.R | 677 | serdes | Writes dm to Excel/CSV/Parquet. Auto-write mode. |
| **cdisc_dm_block** | cdisc-dm-block.R | 322 | 6 | Detects CDISC parent (ADSL/DM), sets PK/FK on USUBJID. Domain-specific. |

### Tier 5: Experimental

Useful concept, narrow scope, needs rethinking.

| Block | File | LOC | Tests | Notes |
|-------|------|-----|-------|-------|
| **temporal_join_block** | temporal-join-block.R | 262 | -- | Standalone: two df inputs, date-window join. Clinical use case (labs within N days of AE). |
| **dm_temporal_join_block** | dm-temporal-join-block.R | 345 | 6 | Same logic, picks tables from a dm. More integrated. |

---

## Open Questions

### Placement: crossfilter_block

`crossfilter_block` operates on plain data frames, not dm objects. It wraps the df as a one-table dm internally to reuse the dm crossfilter engine.

- **Keep here:** depends on the dm crossfilter engine; moving means duplicating or creating a cross-dependency
- **Move out:** users filtering plain data frames shouldn't need blockr.dm; conceptually it's a df operation

Decision: keep here for now, revisit when/if the crossfilter engine is extracted.

### Filter block modernization

The two filter blocks (dm_filter_block, dm_filter_value_block) use server-rendered Shiny inputs. blockr.dplyr demonstrates a better pattern:

- R sends column metadata once (names, types, unique values, ranges)
- JS renders type-appropriate UI (categorical multi-select, numeric operator+input, expression mode)
- Multiple conditions with AND/OR toggle
- State synced as JSON -- no server round-trips for UI changes
- Bidirectional sync for external control (AI agents)

Reusable JS components from blockr.dplyr: `blockr-core.js`, `blockr-select.js`, `blockr-input.js`, `blockr-blocks.css`.

Migration path:
1. Port or share blockr.dplyr's JS components
2. Rebuild dm_filter_block using the JS architecture
3. Subsume dm_filter_value_block's point-and-click UX into the new block
4. Remove dm_filter_value_block

---

## Summary

```
Near-final:     dm_crossfilter_block, dm_example_block, crossfilter_block
Needs love:     dm_block, dm_filter_block, dm_filter_value_block, dm_add_keys_block
Solid utils:    dm_pull, dm_flatten, dm_select, dm_nested_view
I/O:            dm_read, dm_write, cdisc_dm
Experimental:   temporal_join, dm_temporal_join
Placement TBD:  crossfilter_block (df block in dm package)
```
