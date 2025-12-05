# blockr.dm

blockr.dm provides interactive blocks for working with relational data using the [dm](https://dm.cynkra.com/) package. Combine multiple data frames into data models, define relationships, filter across related tables, and visualize table connections.

## Overview

blockr.dm is part of the blockr ecosystem and enables visual management of multi-table data structures. Key features:

- **Relational data modeling**: Combine data frames and define primary/foreign key relationships
- **Cascading filters**: Filter one table and automatically filter related tables via foreign keys
- **Interactive diagrams**: Visualize table relationships with auto-generated diagrams
- **ADaM support**: Designed for pharmaceutical workflows where tables share subject identifiers

## Installation

Install the development version from GitHub using [pak](https://pak.r-lib.org/):

```r
# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.dm")
```

This will also install the required dependencies from GitHub (blockr.core, etc.) as specified in the `Remotes` field.

## Getting Started

```r
library(blockr)
library(blockr.dag)
library(blockr.dm)

# Example: ADaM-style workflow
adsl <- data.frame(
  USUBJID = paste0("SUBJ-", 1:5),
  AGE = c(45, 52, 38, 61, 29),
  SEX = c("M", "F", "M", "F", "M")
)

adlb <- data.frame(
  USUBJID = rep(paste0("SUBJ-", 1:5), each = 2),
  PARAMCD = rep(c("NEUT", "WBC"), 5),
  AVAL = c(4.5, 8.2, 6.1, 7.5, 3.2, 6.8, 5.5, 9.1, 4.8, 7.2)
)

adae <- data.frame(
  USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-2", "SUBJ-4"),
  AETERM = c("Headache", "Nausea", "Fatigue", "Dizziness")
)

# Launch visual interface
run_app(
  blocks = c(
    adsl_data = new_static_block(data = adsl),
    adlb_data = new_static_block(data = adlb),
    adae_data = new_static_block(data = adae),
    dm_obj = new_dm_block(),
    dm_keys1 = new_dm_add_keys_block(
      pk_table = "adsl_data",
      pk_column = "USUBJID",
      fk_table = "adlb_data",
      fk_column = "USUBJID"
    ),
    dm_keys2 = new_dm_add_keys_block(
      pk_table = "adsl_data",
      pk_column = "USUBJID",
      fk_table = "adae_data",
      fk_column = "USUBJID"
    ),
    # Filter by lab condition - cascades to related tables
    filtered_dm = new_dm_filter_block(
      table = "adlb_data",
      expr = "PARAMCD == 'NEUT' & AVAL > 5"
    ),
    # Flatten filtered dm: joins adae with adsl into one table
    flattened = new_dm_flatten_block(start_table = "adae_data")
  ),
  links = c(
    new_link("adsl_data", "dm_obj", "adsl_data"),
    new_link("adlb_data", "dm_obj", "adlb_data"),
    new_link("adae_data", "dm_obj", "adae_data"),
    new_link("dm_obj", "dm_keys1", "data"),
    new_link("dm_keys1", "dm_keys2", "data"),
    new_link("dm_keys2", "filtered_dm", "data"),
    new_link("filtered_dm", "flattened", "data")
  ),
  extensions = list(new_dag_extension())
)
# The flatten block joins adae with adsl via USUBJID for filtered subjects,
# resulting in a single table with columns from both:
# USUBJID, AETERM, AGE, SEX (only for subjects with neutrophils > 5)
```

## Available Blocks

blockr.dm provides 5 blocks for relational data management:

### Creating Data Models
- **new_dm_block()**: Combine multiple data frames into a dm (data model) object. Displays an interactive diagram showing all tables.

### Defining Relationships
- **new_dm_add_keys_block()**: Add primary and foreign key relationships to a dm object. The diagram updates to show connections between tables.

### Querying
- **new_dm_filter_block()**: Filter a dm by condition in any table. The filter automatically cascades to related tables via foreign key relationships - this is the key feature for queries like "get adverse events for subjects with lab value > X".

### Extracting Results
- **new_dm_pluck_block()**: Extract a single table from a dm object as a regular data frame.
- **new_dm_flatten_block()**: Flatten a dm into a single data frame by joining related tables based on foreign keys. For example, starting from an adverse events table, it joins in the subject demographics, resulting in one table with columns from both (USUBJID, AETERM, AGE, SEX, etc.).

## Use Case: Cross-Table Queries

The main use case is querying across related tables. For example, with ADaM clinical trial data:

> "For subjects with Neutrophil counts > 5, what are the corresponding adverse events?"

This query spans three tables (ADSL, ADLB, ADAE) linked by USUBJID. With blockr.dm:

1. Load the three tables with `new_static_block()`
2. Combine into a dm with `new_dm_block()`
3. Add USUBJID as the linking key with `new_dm_add_keys_block()`
4. Filter ADLB with `new_dm_filter_block(table = "adlb", expr = "PARAMCD == 'NEUT' & AVAL > 5")`
5. Extract filtered adverse events with `new_dm_pluck_block(table = "adae")` or flatten with `new_dm_flatten_block(start_table = "adae")` to get AEs joined with subject demographics

The filter cascades automatically - only adverse events for subjects meeting the lab criteria are returned.

## Learn More

- [dm package documentation](https://dm.cynkra.com/) - The underlying relational data package
- [blockr.core](https://github.com/BristolMyersSquibb/blockr.core) - The workflow engine
- [blockr.dag](https://github.com/BristolMyersSquibb/blockr.dag) - DAG-based visual interface
