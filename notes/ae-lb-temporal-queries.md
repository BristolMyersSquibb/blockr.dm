# AE-LB Temporal Queries in ADaM

## The Question

> "If there is an adverse event, can we filter for those that had an event X days before a lab investigation?"

This involves linking two ADaM domains temporally:
- **ADAE** (Adverse Events) - with start/end dates
- **ADLB** (Lab Results) - with measurement dates

## Possible Interpretations

### A) Find labs affected by recent AEs

**Intent:** Identify lab measurements that might be abnormal due to a preceding adverse event.

**Example:**
- Patient has DIARRHOEA (Jan 9-11)
- Lab on Jan 16 shows elevated ALT
- Question: Did the diarrhoea (or dehydration) cause the liver enzyme elevation?

**Use case:** Quality control of lab data, identifying confounded measurements.

### B) Find AEs ongoing during lab collection

**Intent:** Flag labs that might be unreliable because the patient was experiencing an AE at the time.

**Example:**
- Patient has ongoing nausea
- Lab collected during nausea episode
- Question: Should we trust this lab value?

**Use case:** Data quality, protocol deviations.

### C) Temporal linking for causality assessment

**Intent:** Connect AEs and labs to support safety narratives and regulatory causality assessment.

**Example:**
- Serious AE occurs
- Need to document all labs before/after to assess drug causality
- Question: What's the temporal pattern of labs around this AE?

**Use case:** Regulatory submissions, safety narratives, signal detection.

### D) Signal detection

**Intent:** Find patterns across many patients - do AEs of type X correlate with lab abnormalities of type Y?

**Example:**
- Across all patients, do those with "rash" AEs also show elevated eosinophils?
- Question: Is there a safety signal?

**Use case:** Pharmacovigilance, safety monitoring.

---

## Deep Dive: Causality Assessment

### Regulatory Context

When a serious adverse event (SAE) occurs in a clinical trial, regulators (FDA, EMA) require a **causality assessment** - a determination of whether the study drug caused the adverse event.

This is not a simple yes/no. The standard categories are:

| Category | Meaning |
|----------|---------|
| Definitely related | Clear causal relationship |
| Probably related | Likely caused by drug |
| Possibly related | Could be drug or other factors |
| Unlikely related | Probably not the drug |
| Not related | Clearly not the drug |

### The Patient Narrative

For each serious AE, medical writers create a **patient safety narrative** that tells the story:

```
Patient 01-701-1015, 65-year-old male

Medical History: Hypertension, Type 2 diabetes

Timeline:
- Dec 26: Screening labs normal (ALT=27 U/L)
- Jan 3:  Randomized, started study drug (Drug X, 100mg)
- Jan 9:  Developed DIARRHOEA (Grade 2, moderate)
- Jan 11: DIARRHOEA resolved without intervention
- Jan 16: Scheduled lab visit shows elevated ALT (41 U/L)
- Jan 30: Follow-up lab shows ALT normalized (18 U/L)

Assessment:
Transient elevation of liver enzyme (ALT) observed 5 days
after resolution of gastrointestinal adverse event. The
elevation was 1.5x baseline but within normal limits.
Resolved without intervention.

Possible explanations:
1. Drug-related hepatotoxicity (known class effect)
2. Secondary to diarrhoea-induced dehydration
3. Unrelated fluctuation

Causality: Possibly related to study drug.
```

### Why Temporal Linking Matters

The key factors in causality assessment include:

| Factor | Question | How temporal data helps |
|--------|----------|------------------------|
| **Temporal relationship** | Did the abnormality occur after drug/AE? | Need timeline of AE vs lab dates |
| **Dechallenge** | Did labs normalize after drug stopped? | Compare labs before/after drug discontinuation |
| **Rechallenge** | Did it recur when drug restarted? | Track labs across treatment periods |
| **Dose-response** | Worse at higher doses? | Link lab values to dosing records |
| **Biological plausibility** | Known mechanism? | Medical/scientific judgment |

### The Data Query

To support this narrative, analysts need:

```r
# For a given subject with a serious AE:
# 1. All AEs with their dates
# 2. All labs with their dates and values
# 3. Temporal proximity calculated

subject_timeline <-
  labs |>
  left_join(adverse_events, by = "subject") |>
  mutate(
    days_from_ae_start = lab_date - ae_start_date,
    days_from_ae_end = lab_date - ae_end_date
  ) |>
  arrange(lab_date)
```

The "5 days before lab" query is asking:

> "For each lab measurement, were there any AEs ongoing in the 5-day window before the lab was taken?"

This helps identify labs that might be:
- Affected by the AE itself
- Related to the same underlying cause as the AE
- Part of a temporal pattern suggesting drug causality

---

## Concrete Example

From `pharmaverseadam` data:

```
Subject: 01-701-1015

Adverse Events:
┌─────────────────────────────┬────────────┬────────────┐
│ AETERM                      │ Start      │ End        │
├─────────────────────────────┼────────────┼────────────┤
│ APPLICATION SITE ERYTHEMA   │ 2014-01-03 │ (ongoing)  │
│ APPLICATION SITE PRURITUS   │ 2014-01-03 │ (ongoing)  │
│ DIARRHOEA                   │ 2014-01-09 │ 2014-01-11 │
└─────────────────────────────┴────────────┴────────────┘

Lab Tests (ALT):
┌────────────┬───────┬──────────────────────────────────┐
│ Date       │ Value │ AE Context                       │
├────────────┼───────┼──────────────────────────────────┤
│ 2013-12-26 │ 27    │ Baseline (no AEs yet)            │
│ 2014-01-16 │ 41    │ 5 days after DIARRHOEA ended     │
│ 2014-01-30 │ 18    │ AEs ongoing but remote           │
└────────────┴───────┴──────────────────────────────────┘
```

The Jan 16 lab (ALT=41) would be flagged by the query because:
- DIARRHOEA ended Jan 11
- Lab taken Jan 16
- Gap = 5 days → within the window

---

## R Implementation

```r
library(dplyr)
library(pharmaverseadam)

adae <- pharmaverseadam::adae
adlb <- pharmaverseadam::adlb

# Find AE-Lab pairs where AE was ongoing in 5-day window before lab
ae_lab_pairs <- adlb |>
  select(USUBJID, PARAMCD, ADT, AVAL) |>
  inner_join(
    adae |> select(USUBJID, AETERM, AESEV, ASTDT, AENDT),
    by = "USUBJID",
    relationship = "many-to-many"
  ) |>
  mutate(
    window_start = ADT - 5,
    window_end = ADT - 1,
    # Treat ongoing AEs (NA end date) as still active
    AENDT_adj = coalesce(AENDT, as.Date("2099-12-31"))
  ) |>
  filter(
    # AE overlaps with the 5-day window before lab
    ASTDT <= window_end,
    AENDT_adj >= window_start
  )
```

---

## Implications for blockr.dm

This query pattern is complex because it requires:

1. **Cross-domain join** - Linking ADAE and ADLB by subject
2. **Date arithmetic** - Calculating windows
3. **Interval overlap logic** - Two-sided date comparison
4. **Many-to-many handling** - Cardinality explosion

### Solution: `new_dm_temporal_join_block()`

We implemented a specialized block for this use case:

```r
new_dm_temporal_join_block(
  left_table = "adae",
  left_date = "ASTDT",
  right_table = "adlb",
  right_date = "ADT",
  window_days = 7,
  direction = "after"
)
```

**Input/Output:**

- **Input:** dm object (with multiple tables)
- **Output:** data frame (the joined result)

**Parameters:**

| Parameter | Description |
|-----------|-------------|
| `left_table` | The anchor table (e.g., ADAE) |
| `left_date` | Date column in left table (e.g., ASTDT) |
| `right_table` | The table to match (e.g., ADLB) |
| `right_date` | Date column in right table (e.g., ADT) |
| `window_days` | Time window in days |
| `direction` | "after", "before", or "around" |

**Direction options:**

- `"after"`: Right date is 0 to X days after left date
- `"before"`: Right date is X to 0 days before left date
- `"around"`: Right date is within X days in either direction

**What it does:**

1. Joins tables by common key (auto-detected, usually USUBJID)
2. Calculates `days_diff = right_date - left_date`
3. Filters based on window and direction
4. Returns result as a data frame

See `inst/examples/adam_temporal_workflow.R` for a complete example.

---

## Open Questions

1. What is the exact temporal condition needed?
   - AE *ongoing* during window?
   - AE *ended* within window?
   - AE *started* within window?

2. What output format is most useful?
   - List of AE-Lab pairs?
   - Subject timeline view?
   - Flagged lab dataset?
   - Summary statistics?

3. Is this for one subject (narrative) or across all subjects (signal detection)?
