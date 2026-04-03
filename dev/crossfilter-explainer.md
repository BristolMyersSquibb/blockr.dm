# Why the Crossfilter Needs More Than One `dm_filter()` Call

## The crossfilter pattern

The canonical example is [dc.js](https://dc-js.github.io/dc.js/) — an interactive dashboard of 27 years of Nasdaq data with 6 linked charts (bubble chart, histograms, bar charts, time series). Brushing a range in any chart instantly filters all other charts, but the brushed chart itself keeps showing the full distribution. This is the **crossfilter** pattern, invented by Square/Mike Bostock and used everywhere from dc.js to Amazon's faceted search.

The key insight: each widget displays **counts that reflect all other filters but not its own** ("exclude own dimension"). Without this, selecting a value would hide all other values from that widget, making it impossible to compare or deselect.

## Example

Three active filters across two tables:

| Widget | Table | Current selection |
|--------|-------|-------------------|
| SEX | ADSL | F |
| AGE | ADSL | [65, 80] |
| PARAMCD | ADLB | NEUT |

When rendering the **SEX widget**, we need counts computed with AGE=[65,80] AND PARAMCD=NEUT applied, but **without** the SEX=F filter. This way the user still sees both "F (120)" and "M (95)" — they can compare and toggle.

When rendering the **AGE widget**, we apply SEX=F AND PARAMCD=NEUT but **exclude** AGE.

When rendering the **PARAMCD widget**, we apply SEX=F AND AGE=[65,80] but **exclude** PARAMCD.

## Cost structure

For N active filter widgets, each render cycle requires:

- **N calls** with different exclude-dim combinations (one per widget)
- **1 call** for global row counts (all filters applied)

Each call does the full filter propagation across related tables (via key intersection or `dm_filter()`).

## Why this is fundamentally more expensive than a single `dm_filter()`

The **output** of the crossfilter block is a single `dm::dm_filter()` expression with all filters applied — one call, cheap, and already implemented.

The **interactive widgets** are the expensive part. There is no way to compute N different exclude-one filter combinations from a single filter call. Each combination is a distinct query.

## Lookup backend

The lookup backend pre-joins each child table with the parent to create flat lookup tables. Filtering then becomes cheap column operations on pre-joined data (~5-10ms per call). This avoids O(N×T) semi-joins per filter interaction.

## When `dm_filter()` alone is sufficient

If you only need a **static filtered result** (no interactive widgets, no exclude-own-dim), a single `dm::dm_filter()` call is the right tool. This is exactly what the block's output expression already does. The crossfilter pattern is only needed for the interactive widget rendering.
