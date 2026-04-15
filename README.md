# blockr.dm

Interactive [blockr](https://blockr.site) blocks for relational data using the [dm](https://dm.cynkra.com/) package.

## Installation

```r
# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.dm")
```

## Quick Start

Two blocks are enough for an interactive crossfilter dashboard:

```r
library(blockr.core)
library(blockr.dm)

serve(
  new_crossfilter_block(),
  data = list(data = new_dm_example_block(dataset = "safetydata_adam"))
)
```

This loads a multi-table ADaM dataset and launches a crossfilter UI with linked filters across tables, search, density overlays, and automatic filter propagation via foreign keys.

## Available Blocks

### Data
- **new_dm_example_block()** -- Built-in example datasets (BI star schema, ADaM, NYC flights)
- **new_dm_read_block()** -- Read Excel, CSV, ZIP, RDS into a dm
- **new_dm_write_block()** -- Write dm to Excel, CSV, or Parquet

### Modeling
- **new_dm_block()** -- Combine data frames into a dm with automatic key inference
- **new_dm_add_keys_block()** -- Add primary/foreign key relationships
- **new_cdisc_dm_block()** -- Auto-detect CDISC parent table and set keys on USUBJID

### Filtering
- **new_crossfilter_block()** -- Client-side crossfilter (crossfilter2.js) for data frames and dm objects, with linked widgets, search, and automatic cascade via foreign keys
- **new_dm_filter_block()** -- Filter by R expression, cascades via foreign keys
- **new_dm_filter_value_block()** -- Point-and-click filter (table/column/value dropdowns)

### Extracting
- **new_dm_pull_block()** -- Extract one table as a data frame
- **new_dm_flatten_block()** -- Join all related tables into one data frame
- **new_dm_select_block()** -- Keep a subset of tables
- **new_dm_nested_view_block()** -- Expandable parent/child table view

### Joins
- **new_dm_temporal_join_block()** -- Date-window join between dm tables
- **new_temporal_join_block()** -- Date-window join between two data frames

## Learn More

- [blockr.site](https://blockr.site) -- Tutorials, examples, videos, and block reference
- [dm package](https://dm.cynkra.com/) -- The underlying relational data package
- [blockr.core](https://github.com/BristolMyersSquibb/blockr.core) -- The workflow engine
