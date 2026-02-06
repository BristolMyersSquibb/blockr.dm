# Dynamic Filtering: Comprehensive Research Report

## Context

This research investigates dynamic filtering approaches for interactive data exploration, with a focus on multi-table clinical trial (ADaM/CDISC) data. The goal is to understand the landscape - what teal does, what other tools/frameworks do, what patterns exist in the wider world - and assess how blockr's existing capabilities compare.

---

## Part I: teal's Dynamic Filtering System

### 1.1 Architecture Overview

teal (Roche/insightsengineering) is the dominant R/Shiny framework for clinical trial data exploration in pharmaverse. Every teal app has a **filter panel** on the right side, powered by the `teal.slice` package.

**Class hierarchy (all R6):**
```
FilteredData                          # Top-level: manages all datasets + filter panel
  -> FilteredDataset (one per dataset) # Per-dataset filter manager
       -> FilterStates                 # Collection of filters for one dataset
            -> FilterState             # Individual filter (one column, one condition)
                 |- ChoicesFilterState  # categorical: character, factor -> checkbox group
                 |- RangeFilterState    # numeric -> slider + numeric inputs
                 |- DateFilterState     # Date -> date range picker
                 |- DatetimeFilterState # POSIXct -> datetime range picker
                 |- LogicalFilterState  # logical -> TRUE/FALSE checkboxes
                 |- EmptyFilterState    # no data -> disabled
            -> FilterStateExpr         # Custom R expression filter (always fixed)
```

### 1.2 Filter Types and Auto-Detection

teal auto-generates the appropriate widget per column type:

| R Type | FilterState | UI Widget | Behavior |
|---|---|---|---|
| character, factor | ChoicesFilterState | Checkbox group | Select one or more levels |
| numeric, integer | RangeFilterState | Slider + numeric inputs + mini histogram | Inclusive range `[min, max]` |
| Date | DateFilterState | Date range picker | Date range |
| POSIXct/lt | DatetimeFilterState | Datetime range picker | Datetime range |
| logical | LogicalFilterState | Checkbox group (TRUE/FALSE) | Single selection |
| any (no data) | EmptyFilterState | Disabled | No filtering |
| custom expr | FilterStateExpr | Fixed display | R expression, immutable |

**Type coercion:** If a numeric column has fewer than N unique values (configurable), teal treats it as categorical (checkbox group instead of slider). This handles cases like Year, Quarter, Dose Level.

**NA/Inf handling:** Every filter type supports `keep_na` and `keep_inf` toggles.

### 1.3 Cross-Dataset Filter Propagation (join_keys)

This is teal's most important feature for clinical data.

**How it works:**
- `join_keys()` defines relationships between datasets with primary keys and foreign keys
- Parent-child direction is explicit (ADSL = parent, everything else = child)
- Pre-configured `default_cdisc_join_keys` covers 19 ADaM datasets

```r
jk <- join_keys(
  join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")),      # PK
  join_key("ADSL", "ADAE", c("STUDYID", "USUBJID")),       # FK: ADSL -> ADAE
  join_key("ADSL", "ADLB", c("STUDYID", "USUBJID")),       # FK: ADSL -> ADLB
)
```

**Propagation rules:**
1. Filters on parent (ADSL) **automatically cascade** to all children (ADAE, ADLB, etc.)
2. Child dataset filters can only be on columns NOT in the parent (prevents conflicts)
3. Implicit relationships: if ADAE and ADLB both link to ADSL, the system infers they're indirectly related
4. Subject count is tracked and displayed (unique USUBJID remaining)

**Propagation mechanism:** Constructs subsetting calls like `adae[adae$USUBJID %in% filtered_adsl$USUBJID, ]`

### 1.4 Filter Panel UX

**Three sections:**
1. **Top**: Record counts (filtered/unfiltered) + subject counts for relational data
2. **Middle**: Active filter controls (sliders, checkboxes, etc.)
3. **Bottom**: "Add new filter" dropdown for any column of any dataset

**Global vs module-specific:** By default all modules share one filter state. With `module_specific = TRUE`, each module tab can have independent filters, with a `mapping` argument controlling which filters are active where.

### 1.5 teal_slice / teal_slices (filter state objects)

```r
teal_slice(
  dataname = "ADSL",          # which dataset
  varname = "SEX",            # which column (or use expr instead)
  selected = "F",             # pre-selected values
  choices = c("M", "F"),      # allowed values
  keep_na = FALSE,            # include NAs?
  fixed = FALSE,              # if TRUE, user cannot change selection
  anchored = FALSE,           # if TRUE, user cannot remove/deactivate
  multiple = TRUE             # allow multi-select
)
```

**teal_slices()** aggregates multiple slices with panel-wide settings:
- `include_varnames` / `exclude_varnames` - whitelist/blacklist columns
- `count_type` - show filtered/total counts (expensive)
- `allow_add` - whether users can add new filters

### 1.6 Code Reproducibility

Every filter generates reproducible R code via nested `get_call()` chain: FilteredData -> FilteredDataset -> FilterStates -> FilterState. Produces `dplyr::filter()` expressions.

### 1.7 Known Limitations and Complaints

From GitHub issues and community feedback:

**UX/Interface:**
1. **Scrolling hell** (teal.slice #14): As filters accumulate, users must scroll endlessly. No collapsible groups per dataset.
2. **Filter panel always visible on start** (teal #1220): No option to start collapsed.
3. **Confusing numeric ranges** (teal.slice #220): `-2.7 - -1.85` uses same character for negative and range separator.
4. **Daunting for non-technical users** (teal.slice #47): Users unfamiliar with data find it overwhelming.

**Functional:**
5. **AND-only logic**: No OR operator between filters within a dataset.
6. **No crossfilter pattern**: No "exclude own dimension" for coordinated views.
7. **No hierarchical/cascading dependent filters**: Can't have study -> site -> patient where child options depend on parent selection.
8. **No named population presets**: No one-click "safety population" or "ITT population".
9. **Modules cannot ignore filter panel** (teal #905): All modules forced to use it.
10. **Opening "Available filters" clears state** (teal.slice #595): Bug.

**Performance:**
11. **count_type = "all" is expensive**: Computing filtered/unfiltered counts for every filter value adds overhead on large datasets.
12. **High-precision decimal crash**: Reported bug with many decimal places.
13. **General Shiny overhead**: Inherits all of Shiny's performance characteristics.

### 1.8 teal Module Ecosystem

**37 clinical modules** (teal.modules.clinical) covering:
- Adverse events (events summary, by grade, by SMQ, by term)
- Efficacy/survival (KM plot, Cox regression, forest plots)
- Lab/shift tables (abnormality, worst grade, shift by arm)
- Patient profiles (timeline, labs, vitals, AE plot)
- Statistical models (MMRM, ANCOVA, logistic regression, GEE)

**15 general modules** (teal.modules.general): data table, variable browser, scatter/distribution plots, PCA, regression, etc.

All modules consume filtered data from the filter panel. The filtering is global infrastructure, not per-module.

---

## Part II: Dynamic Filtering in the Wider World

### 2.1 Crossfilter.js - The Gold Standard for Single-Table Filtering

**Origin:** Created by Square (Mike Bostock), now maintained as open source. Powers dc.js charts.

**Core concept:** Explore large multivariate datasets in the browser with coordinated views and sub-30ms interaction on million+ records.

**Architecture:**
- **Crossfilter instance**: Holds entire dataset as an array
- **Dimensions**: Sorted indices over one column each, with **bitmask tracking** (each record has a bit per dimension indicating pass/fail)
- **Groups**: Aggregations that **exclude their own dimension's filter** - this is the key insight

**The "exclude own dimension" principle:**
When a user brushes a histogram for "age", the age histogram still shows the full distribution (greying out unselected bars), while all other charts update to show only the filtered subset. Each chart's group "sees through" its own filter. This creates coordinated views without explicit coordination logic.

**Incremental updates:** When one filter changes, crossfilter loops only through rows where selection status changed, updating group aggregates incrementally. This is how it achieves sub-30ms.

**Limitation:** **Single flat table only.** No concept of joins or relational data. Must pre-denormalize.

**Key takeaway for blockr:** blockr.bi's `table_filter_block` already implements this pattern via `crossfilter_data(exclude_dim)`. The challenge is extending it to multi-table scenarios.

### 2.2 Tableau - Deferred Joins and Relationships

Tableau (2020+) introduced a **relationship-based data model**:

- **Relationships** describe how tables relate but **don't merge them**. Tables remain separate.
- **Deferred joins**: Tableau automatically selects join type at query time based on which fields are used in the current visualization. No premature data loss from inner joins.
- **Multi-Fact Relationships** (2024.2+): Multiple fact tables can share dimension tables. Filters on shared dimensions affect all fact tables.
- **Filter Actions**: Clicking a chart element filters all other sheets sharing a data source.

**Key takeaway:** Don't materialize JOINs until you know what's needed. Filter predicates should flow through the relationship graph, and joins happen at the point of consumption.

### 2.3 Power BI - Star Schema with Directional Propagation

Power BI uses a **star schema** with explicit relationship metadata:

- **Relationships** defined with cardinality (1:1, 1:many, many:many) and **cross-filter direction** (single or bidirectional)
- **Default**: Filters flow from the "one" side (dimension table) to the "many" side (fact table)
- **Bidirectional** (optional): Filters can flow many-to-one (discouraged for performance)
- **CROSSFILTER DAX function**: Dynamically override direction for specific calculations
- **Edit Interactions**: Per-visual control of response (filter, highlight, or none)

**Key takeaway:** The directional filter flow (one->many) maps perfectly to ADaM data where ADSL (one) propagates to ADAE/ADLB (many). The "edit interactions" pattern (per-visual filter/highlight/none) is a useful UX concept.

### 2.4 DuckDB / Polars - Modern Analytical Engines

**DuckDB:**
- In-process analytical SQL engine, no server needed
- Columnar-vectorized execution (SIMD-optimized)
- Native Parquet support with predicate pushdown (skip irrelevant row groups)
- **R/Shiny benchmark** (Appsilon): 10-40x faster than dplyr for large datasets. 4.5M records filtered in ~0.5s.
- `duckplyr`: drop-in dplyr backend routing to DuckDB

**Polars:**
- Lazy evaluation: operations build a query plan, optimized before execution
- Predicate pushdown + projection pushdown + common subexpression elimination
- Available in R via `tidypolars`

**Key takeaway:** For datasets beyond ~50K rows, these engines provide dramatic speedups. Filter blocks could generate SQL predicates for DuckDB or lazy expressions for Polars instead of materializing intermediate data frames.

### 2.5 Apache Arrow / DataFusion - Columnar Filtering

- **Columnar layout**: Filter one column, produce boolean mask, apply to all columns
- **Parquet row-group pruning**: Use min/max statistics to skip entire groups
- **Late materialization**: Evaluate filter columns first, only decode matching rows
- Arrow R package provides dplyr backend with automatic pushdown

**Key takeaway:** If data is stored as Parquet, filter predicates can be evaluated during I/O, dramatically reducing the amount of data loaded into memory.

### 2.6 E-Commerce Faceted Search

Amazon, Airbnb, and every e-commerce site use faceted search:

- Multiple independent facets (price, brand, color, size) in a sidebar
- **Counts update dynamically**: Each facet value shows how many results it would produce
- **Within facet**: values OR together; **across facets**: AND together
- **The count-update pattern uses crossfilter's "exclude own dimension"**: the count for "blue" reflects data filtered by all OTHER facets but not color

**Key takeaway:** Dynamic count display per filter value is extremely useful for data exploration and is the most-requested UX pattern. teal supports it (`count_type = "all"`) but it's expensive.

### 2.7 Shneiderman's Information Seeking Mantra (Academic Foundation)

**"Overview first, zoom and filter, then details-on-demand"** (Ben Shneiderman, 1996)

- **Dynamic queries** (1994): Continuous manual adjustment of filter values with **<100ms visual feedback**
- **Coordinated Multiple Views** (CMV): Multiple linked visualizations where interaction in one propagates to others
- **Cross-table linking and brushing** (Visual Computer, 2018): Extends brushing/filtering to multiple related tables via mappings

**Key takeaway:** The 100ms target for filter response is well-established in HCI research. The "overview -> filter -> details" flow maps naturally to a block pipeline.

### 2.8 Reactive Vega - Streaming Dataflow Architecture

Satyanarayan et al. (2015) formalized reactive visualization as a **streaming dataflow graph**:

- Input data, scene graph, and interactions are all streaming data sources
- Changes propagate through a DAG in **topological order**
- Each operator re-evaluates only when its dependencies change
- The graph can dynamically rewrite itself at runtime

**Key takeaway:** blockr's DAG architecture (blockr.dag) is essentially this pattern. The infrastructure for reactive, topologically-ordered propagation already exists.

---

## Part III: What blockr Already Has

### 3.1 blockr.bi - table_filter_block (the crossfilter)

**File:** `blockr.bi/R/table-filter-block.R`

The `table_filter_block` is the primary crossfilter component. It implements:

- **Multiple dimension tables** side-by-side, each showing one column's values with aggregated measures and inline bar charts
- **Crossfilter principle**: `crossfilter_data(exclude_dim)` filters by ALL OTHER dimensions but NOT the current one - so each table shows the full distribution of its dimension
- **Click-to-toggle**: Click a row to add to filter, click again to remove. Multi-select.
- **Auto-detection**: Classifies columns as dimensions (categorical or numeric with <=10 unique values) vs measures (numeric, high cardinality)
- **Measure selection**: Dropdown to choose aggregation (Count or any numeric column sum)
- **Diverging bars**: Handles negative values with left-right diverging bar layout
- **Searchable + sortable** via reactable
- **Filter status**: Shows active filters, row counts, "Remove Filter" button
- **State persistence**: dimensions, measure, and filters are saved/restored

**Implementation detail - how crossfilter works:**
```r
crossfilter_data <- function(exclude_dim) {
  df <- data()                          # Start with full data
  filters <- r_filters()
  for (dim in names(filters)) {
    if (dim == exclude_dim) next        # Skip own dimension
    val <- filters[[dim]]
    if (!is.null(val) && length(val) > 0 && dim %in% names(df)) {
      df <- dplyr::filter(df, .data[[dim]] %in% val)
    }
  }
  df
}
```

**Output:** Generates `dplyr::filter()` expressions combining all active filters with `&`.

### 3.2 blockr.dm - dm_filter_block (relational cascading)

**File:** `blockr.dm/R/dm-filter-block.R`

- Filter a dm object by applying an expression to one table
- Uses `dm::dm_filter()` which **automatically cascades** via foreign key semi-joins
- Text expression input (e.g., `PARAMCD == 'NEUT' & AVAL > 5`)
- Auto-detects available tables from the dm
- Output: filtered dm with ALL related tables updated

**Supporting blocks:**
- `new_dm_block(infer_keys = TRUE)` - Creates dm from multiple data frames, auto-infers PK/FK from shared column names (USUBJID pattern)
- `new_dm_flatten_block()` - Flattens dm to single table via joins
- `new_dm_pull_block()` - Extracts one table from dm as data frame

### 3.3 blockr.extra - Function-based parametric filters

**File:** `blockr.extra/R/function-block-base.R`

- `create_input_for_arg()` auto-generates UI from function defaults:
  - `c("a", "b", "c")` -> selectInput
  - `list("A" = "a", "B" = "b")` -> multi-select
  - `42` -> numericInput
  - `TRUE` -> checkboxInput
  - `"text"` -> textInput
  - `NULL` -> textInput with "NULL" placeholder

### 3.4 blockr.dplyr - Column-level filter

- `new_filter_block()` - Standard column + operator + value filter
- Single filter per block (chain multiple for AND)

---

## Part IV: Gap Analysis

### What blockr does better than teal today:
1. **Crossfilter pattern** (table_filter_block) - teal has nothing comparable
2. **Visual, clickable filtering** - teal uses form widgets, not data-driven UIs
3. **DAG-based workflow architecture** - teal is module-based, not composable
4. **Expression-based filtering** (dm_filter) - supports complex logic beyond AND
5. **Function-based parametric filtering** - flexible, code-driven approach

### What teal does that blockr doesn't (yet):
1. **Auto-generated per-column filters** for every column type (slider, checkbox, date picker)
2. **Dynamic count display** per filter value (how many records each value produces)
3. **Filter state management API** (get/set/clear/serialize programmatically)
4. **Standardized CDISC join keys** (pre-configured for 19 ADaM datasets)
5. **Module-specific filtering** (different filters per analysis tab)

### What neither does:
1. **Crossfilter on multi-table data** (the bridge)
2. **Named population presets** (one-click ITT/Safety/Per-Protocol)
3. **DuckDB/Arrow performance backend** for large datasets
4. **Hierarchical/cascading dependent filters** (study -> site -> patient options update)
5. **OR logic in the UI** (both are AND-only in the visual interface)

---

## Part V: Use Cases from Clinical Trial Data Exploration

Based on teal's 37+ clinical modules and common pharma workflows:

### Use Case 1: Population Selection
"Show me the safety population, females over 65, in the Drug A arm"
- Requires: ADSL filters on SAFFL, SEX, AGE, TRT01A
- Cascades to: ALL downstream datasets

### Use Case 2: Adverse Event Investigation
"Which patients had Grade 3+ neutropenia within 7 days of a serious AE?"
- Requires: ADLB filter (PARAMCD == "NEUT", AVAL, Grade), ADAE filter (AESER, AESEV), temporal join
- Cross-table: ADLB subjects -> ADAE events (via USUBJID + time window)

### Use Case 3: Lab Signal Detection
"Show me the distribution of ALT values for patients who experienced hepatotoxicity"
- Requires: ADAE filter (AEDECOD contains "hepato"), propagate subjects to ADLB, visualize ADLB.AVAL for PARAMCD="ALT"
- Cross-table: Start from ADAE, identify subjects, view their ADLB records

### Use Case 4: Subgroup Analysis
"Compare efficacy (ADTTE time-to-event) between Japanese and non-Japanese patients with prior therapy"
- Requires: ADSL filter (RACE, prior therapy flag), cascades to ADTTE, KM plot

### Use Case 5: Data Quality Exploration
"Which sites have unusual patterns of missing lab data?"
- Requires: Crossfilter on ADLB with dimensions: SITEID, PARAMCD, VISIT, with Count measure and missing data flagging
- Single-table crossfilter (table_filter_block works today)

### Use Case 6: Regulatory Query Response
"The FDA asks: for patients with elevated creatinine, what concomitant medications were they on?"
- Requires: ADLB filter (PARAMCD == "CREAT", AVAL > threshold), propagate subjects to ADCM, list medications
- Cross-table cascading (dm_filter_block works today, but no visual crossfilter UI)

---

## Part VI: Summary of Key Architectural Patterns

| Pattern | Source | Description | blockr Status |
|---|---|---|---|
| Exclude own dimension | Crossfilter.js | Each view filters by all others except itself | Implemented (table_filter_block) |
| Parent-child propagation | Power BI / teal | Filters flow from dimension to fact tables | Implemented (dm_filter) |
| Deferred joins | Tableau | Don't materialize joins until consumption | Not implemented |
| Predicate pushdown | DuckDB/Arrow | Push filters to data source, reduce I/O | Not implemented |
| Lazy evaluation | Polars | Build query plan, optimize, execute once | Not implemented |
| Faceted count display | E-commerce | Show per-value counts reflecting other filters | Not implemented |
| Dynamic queries | Shneiderman | <100ms feedback on filter interaction | Partially (needs perf work) |
| Streaming dataflow DAG | Reactive Vega | Topological propagation with change tracking | Implemented (blockr.dag) |
| Star schema | BI tools | Dimension + fact tables with directional flow | Partially (dm has structure, no direction concept) |
