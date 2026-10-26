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
  data = list(data = new_dm_example_block())
)
```

This loads a small multi-table example (a retail star schema) and opens a
crossfilter with linked counts across its tables. A selection in one table
narrows the others through their foreign keys.

## Available Blocks

### Data
- **new_dm_example_block()** -- Built-in example datasets (BI star schema, NYC flights, insurance)

### Modeling
- **new_dm_block()** -- Combine data frames into a dm with automatic key inference
- **new_dm_add_keys_block()** -- Add primary/foreign key relationships

### Filtering
- **new_crossfilter_block()** -- Client-side crossfilter (crossfilter2.js) for data frames and dm objects, with linked widgets, search, and cascade via foreign keys
- **new_dm_filter_block()** -- Type-aware filter UI (reused from blockr.dplyr) on one dm table; cascades via foreign keys
- **new_value_filter_block()** -- Pick values of chosen columns; on a dm, cascades via foreign keys
- **new_drill_filter_block()** -- The value filter a drill-down click writes into, used with blockr.viz's control bridge

### Extracting
- **new_dm_pull_block()** -- Extract one table as a data frame
- **new_dm_flatten_block()** -- Join related tables into one data frame
- **new_dm_select_block()** -- Keep a subset of tables

## Learn More

- [blockr.site](https://blockr.site) -- Tutorials, examples, videos, and block reference
- [dm package](https://dm.cynkra.com/) -- The underlying relational data package
- [blockr.core](https://github.com/BristolMyersSquibb/blockr.core) -- The workflow engine
