# synthetic.dm — Synthetic Relational Data Generation for dm Objects

## Problem

Clients often work with confidential data (clinical trials, patient records, internal
business data). When building workflows with blockr, the workflow logic is not
confidential — but it's trapped behind the data. If we could generate a synthetic version
of a `dm` object that preserves the schema, relationships, and realistic value
distributions, the workflow could be exported and developed in an open environment.

Today there is **no maintained R package** that does this:

- **DataFakeR** (openpharma) — removed from CRAN 2025-03, unmaintained since 2023.
  Used YAML schemas, no dm integration.
- **synthpop** — excellent for single tables, no multi-table support.
- **SDV** (Python) — gold standard for relational synthetic data, no R bindings.
- **fabricatr** — hierarchical data only, not arbitrary PK/FK.

## Goal

A standalone R package (`synthetic.dm` or similar) that takes a `dm` object and returns a
synthetic `dm` with:

1. Same table names, column names, column types
2. Same PK/FK relationships, all referentially valid
3. Realistic value distributions (numeric ranges, factor levels, date ranges)
4. No real records — safe to share openly

## Core Algorithm

### Step 1: Introspect the dm

```r
tables <- dm::dm_get_all_pks(dm_obj)
fks <- dm::dm_get_all_fks(dm_obj)
# For each table: column types, unique values, ranges, NA rates
profiles <- lapply(dm::dm_get_tables(dm_obj), profile_table)
```

### Step 2: Topological sort

Order tables so parents are generated before children. The `dm` package already exposes
the relationship graph — use `igraph` or a simple Kahn's algorithm to topo-sort.

```
ADSL (root, no FKs)
  -> ADAE (FK: USUBJID -> ADSL)
  -> ADLB (FK: USUBJID -> ADSL)
  -> ADVS (FK: USUBJID -> ADSL)
```

### Step 3: Generate root tables

For tables with no FK dependencies:

1. Determine target row count (`n` — same as original or user-specified)
2. Generate PK column with unique synthetic IDs
3. Generate remaining columns using one of:
   - **synthpop::syn()** — learns distributions from real data (best fidelity)
   - **Schema-driven** — extract type + range + levels, generate with base R / charlatan
     (no real data needed at generation time, only the profile)
4. The schema-driven approach is interesting because the **profile** could be exported
   without the data — a metadata-only artifact that enables generation elsewhere.

### Step 4: Generate child tables

For each child table (in topo order):

1. Get the FK mappings (which columns reference which parent table/columns)
2. **Cardinality sampling**: count how many child rows each parent key has in the real
   data. Fit that distribution (e.g., empirical histogram or parametric). For each
   synthetic parent key, sample a child-row count from that distribution.
3. Expand: create the child table skeleton with FK columns filled from parent PKs
4. Generate non-FK columns (synthpop or schema-driven), conditional on the FK grouping
5. Handle composite FKs (USUBJID + PARAMCD) — treat the tuple as the join key

### Step 5: Rebuild the dm

```r
synthetic_dm <- dm::dm(!!!synthetic_tables)
# Re-add all PKs and FKs from the original
for (pk in original_pks) dm::dm_add_pk(...)
for (fk in original_fks) dm::dm_add_fk(...)
```

## Key Design Decisions

### 1. Generation strategy

| Approach | Pros | Cons |
|---|---|---|
| **synthpop** (learn from real data) | Best statistical fidelity, handles correlations | Requires real data at generation time |
| **Schema-driven** (profile only) | Profile can be exported without data, generation happens anywhere | Less realistic correlations |
| **LLM-assisted** | Handles domain semantics ("PARAMCD should be lab test codes") | Slow, non-deterministic, overkill for most cases |

Recommendation: **support both synthpop and schema-driven**, let the user choose. The
schema-driven approach is the unique value prop — export a profile YAML/JSON, generate
data on a different machine without ever seeing the real data.

### 2. Cardinality preservation

For child tables, the number of rows per parent key matters. Options:

- **Exact** — same distribution as real data (histogram of rows-per-key)
- **Parametric** — fit a distribution (Poisson, negative binomial) to the row counts
- **Fixed** — user specifies a constant (e.g., "5 AEs per patient")

Default to exact (empirical resampling of row counts).

### 3. Column-level constraints

Some constraints go beyond type/range:

- Dates: AESTDT > RFSTDTC (AE start after reference start date)
- Derived columns: AVAL = AVAL - BASE (change from baseline)
- Categorical dependencies: AESEV ∈ {MILD, MODERATE, SEVERE} only when AESER == "N"

Phase 1: ignore these (synthpop's CART handles simple correlations).
Phase 2: allow user-specified constraint functions.

### 4. Composite keys

`dm` supports composite PKs/FKs (multiple columns). The generation logic must treat these
as tuples. For composite PKs, generate unique combinations. For composite FKs, sample
valid tuples from the parent.

## Package Structure

```
synthetic.dm/
  R/
    synthesize_dm.R       # Main entry point
    profile.R             # Extract table profiles from a dm
    topo_sort.R           # Topological ordering of tables
    generate_table.R      # Single-table generation (synthpop or schema-driven)
    cardinality.R         # Row-count distribution sampling
    utils.R               # ID generators, type helpers
  inst/
    profiles/             # Example exported profiles (CDISC ADaM, etc.)
  tests/
  vignettes/
```

## API Sketch

```r
# Full pipeline — requires real data
synthetic_dm <- synthesize_dm(real_dm, n_scale = 1.0, method = "synthpop")

# Two-step pipeline — profile can be exported
profile <- dm_profile(real_dm)
save_profile(profile, "my_study_profile.yaml")

# On a different machine, without real data:
profile <- load_profile("my_study_profile.yaml")
synthetic_dm <- generate_from_profile(profile, n_scale = 0.5)

# Validate the result
dm::dm_examine_constraints(synthetic_dm)  # All constraints satisfied
```

## Phases

### Phase 1 — MVP
- Single-column PKs and FKs only
- synthpop-based generation (requires real data)
- Exact cardinality resampling
- Works with CDISC ADaM-style dm objects (ADSL as root, star schema)

### Phase 2 — Schema-driven generation
- Export/import profiles (YAML or JSON)
- Generate without real data
- charlatan integration for realistic names, dates, IDs

### Phase 3 — Advanced
- Composite key support
- User-defined column constraints
- Configurable cardinality (parametric, fixed)
- Performance optimization for large dm objects

## Relation to blockr

Once `synthetic.dm` exists as a standalone package, wrapping it in a blockr block is
trivial:

```r
new_synthesize_dm_block <- function(n_scale = 1.0, method = "synthpop", ...) {
  new_transform_block(
    n_scale = new_numeric_field(n_scale, min = 0.1, max = 10),
    method = new_select_field(method, choices = c("synthpop", "schema")),
    ...,
    class = "synthesize_dm_block"
  )
}
```

## Open Questions

1. **Name**: `synthetic.dm`? `dm.synth`? `fakeDM`?
2. **Should this be proposed to cynkra** as a dm companion package, or fully independent?
3. **CDISC-specific helpers**: ship built-in profiles for standard ADaM domains (ADSL,
   ADAE, ADLB, etc.) so users can generate realistic clinical trial data without any
   real data at all?
4. **Licensing**: MIT to match dm? Or match blockr's LGPL?
