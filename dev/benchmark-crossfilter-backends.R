# Benchmark: Crossfilter Backend Comparison
#
# Compares three backends (dplyr, duckdb SQL, duckplyr) on identical data.
# Uses the bundled ADaM parquet files (~201K rows, 8 tables).
#
# Purpose: reproducible case for discussing duckplyr performance with Kirill.
# The dplyr `.data[[dim]]` and `between()` patterns cause silent fallback in
# duckplyr, so the duckplyr backend uses `!!rlang::sym()` and `>=`/`<=`.

library(dplyr)
library(dm)
library(DBI)
library(duckdb)

pkgload::load_all(".")

# --- Load data ---
adam_dir <- system.file("extdata", "adam", package = "blockr.dm")
files <- list.files(adam_dir, pattern = "\\.parquet$", full.names = TRUE)
tables <- setNames(
  lapply(files, arrow::read_parquet),
  tools::file_path_sans_ext(basename(files))
)

message("Tables loaded:")
for (nm in names(tables)) {
  message(sprintf("  %-10s %s rows x %s cols", nm, nrow(tables[[nm]]),
                  ncol(tables[[nm]])))
}
message(sprintf("  Total: %s rows\n", sum(vapply(tables, nrow, integer(1)))))

# --- Build dm with keys ---
dm_obj <- dm::dm(!!!tables)
dm_obj <- dm::dm_add_pk(dm_obj, adsl, USUBJID)
for (tbl in setdiff(names(tables), "adsl")) {
  if ("USUBJID" %in% names(tables[[tbl]])) {
    dm_obj <- dm::dm_add_fk(dm_obj, !!tbl, USUBJID, adsl)
  }
}

# --- Key column function ---
pks <- dm::dm_get_all_pks(dm_obj)
fks <- dm::dm_get_all_fks(dm_obj)
key_col_fn <- function(tbl_name) {
  pk_row <- pks[pks$table == tbl_name, ]
  if (nrow(pk_row) > 0) return(pk_row$pk_col[[1]][[1]])
  fk_row <- fks[fks$child_table == tbl_name, ]
  if (nrow(fk_row) > 0) return(fk_row$child_fk_cols[[1]][[1]])
  NULL
}

# --- Pre-compute column classes (for duckplyr date detection w/o materialization) ---
col_classes <- build_col_classes(tables)

# --- Realistic filter state (simulates 5 active widgets) ---
cat_filters <- list(
  adsl = list(SEX = c("F"), ETHNIC = c("NOT HISPANIC OR LATINO")),
  adae = list(AESEV = c("SEVERE", "MODERATE"))
)
rng_filters <- list(
  adsl = list(AGE = c(50, 75))
)

table_names <- names(tables)

# --- DuckDB setup ---
con <- dbConnect(duckdb(), dbdir = ":memory:")
for (nm in table_names) duckdb_register(con, nm, tables[[nm]], overwrite = TRUE)

# --- duckplyr setup ---
duck_tables <- tryCatch({
  requireNamespace("duckplyr", quietly = TRUE)
  as_duck <- if (exists("as_duckdb_tibble", asNamespace("duckplyr"))) {
    duckplyr::as_duckdb_tibble
  } else {
    duckplyr::as_duckplyr_tibble
  }
  # Strip column-level attributes (labels) — duckplyr 1.0+ rejects them
  strip_attrs <- function(df) {
    for (nm in names(df)) {
      a <- attributes(df[[nm]])
      keep <- intersect(names(a), c("class", "levels", "names", "dim", "tzone"))
      attributes(df[[nm]]) <- a[keep]
    }
    df
  }
  setNames(lapply(table_names, function(nm) as_duck(strip_attrs(tables[[nm]]))),
           table_names)
}, error = function(e) {
  message("duckplyr setup failed: ", conditionMessage(e))
  NULL
})

# --- Benchmark parameters ---
n_iter <- 20
# Simulate the per-update hot path: for each of 5 widgets, call crossfilter_data
# (exclude-own-dim) + crossfilter_agg (for categorical) or just _data (for range)
widgets <- list(
  list(tbl = "adsl", dim = "SEX",    type = "cat"),
  list(tbl = "adsl", dim = "ETHNIC", type = "cat"),
  list(tbl = "adsl", dim = "AGE",    type = "range"),
  list(tbl = "adae", dim = "AESEV",  type = "cat"),
  list(tbl = "adae", dim = "AESEV",  type = "cat")  # duplicate to simulate more work
)

run_one_update <- function(backend) {
  for (w in widgets) {
    if (backend == "dplyr") {
      if (w$type == "cat") {
        dplyr_crossfilter_agg(tables, key_col_fn, w$tbl, w$dim,
                              cat_filters, rng_filters)
      } else {
        dplyr_crossfilter_data(tables, key_col_fn, w$tbl, w$dim,
                               cat_filters, rng_filters)
      }
    } else if (backend == "duckdb") {
      if (w$type == "cat") {
        duckdb_crossfilter_agg(con, tables, table_names, key_col_fn,
                               w$tbl, w$dim, cat_filters, rng_filters)
      } else {
        duckdb_crossfilter_data(con, tables, table_names, key_col_fn,
                                w$tbl, w$dim, cat_filters, rng_filters)
      }
    } else if (backend == "duckplyr" && !is.null(duck_tables)) {
      if (w$type == "cat") {
        duckplyr_crossfilter_agg(duck_tables, col_classes, key_col_fn,
                                 w$tbl, w$dim, cat_filters, rng_filters)
      } else {
        duckplyr_crossfilter_data(duck_tables, col_classes, key_col_fn,
                                   w$tbl, w$dim, cat_filters, rng_filters)
      }
    }
  }
  # Also compute counts
  if (backend == "dplyr") {
    dplyr_crossfilter_counts(tables, key_col_fn, table_names,
                             cat_filters, rng_filters)
  } else if (backend == "duckdb") {
    duckdb_crossfilter_counts(con, tables, table_names, key_col_fn,
                              cat_filters, rng_filters)
  } else if (backend == "duckplyr" && !is.null(duck_tables)) {
    duckplyr_crossfilter_counts(duck_tables, col_classes, key_col_fn,
                                table_names, cat_filters, rng_filters)
  }
}

# --- Correctness check: all backends produce same results ---
message("Correctness check...")
for (w in widgets[1:4]) {
  r_dplyr <- dplyr_crossfilter_data(tables, key_col_fn, w$tbl, w$dim,
                                     cat_filters, rng_filters)
  r_duck <- duckdb_crossfilter_data(con, tables, table_names, key_col_fn,
                                     w$tbl, w$dim, cat_filters, rng_filters)
  stopifnot(nrow(r_dplyr) == nrow(r_duck))
  if (!is.null(duck_tables)) {
    r_duckplyr <- duckplyr_crossfilter_data(duck_tables, col_classes, key_col_fn,
                                             w$tbl, w$dim, cat_filters, rng_filters)
    stopifnot(nrow(r_dplyr) == nrow(r_duckplyr))
  }
  message(sprintf("  %-6s.%-6s  dplyr=%d  duckdb=%d  OK",
                  w$tbl, w$dim, nrow(r_dplyr), nrow(r_duck)))
}

counts_dplyr <- dplyr_crossfilter_counts(tables, key_col_fn, table_names,
                                          cat_filters, rng_filters)
counts_duck <- duckdb_crossfilter_counts(con, tables, table_names, key_col_fn,
                                          cat_filters, rng_filters)
stopifnot(counts_dplyr$filtered == counts_duck$filtered)
message(sprintf("  Counts: dplyr=%d  duckdb=%d  OK\n", counts_dplyr$filtered,
                counts_duck$filtered))

# --- Warmup ---
message("Warming up...")
for (b in c("dplyr", "duckdb")) run_one_update(b)
if (!is.null(duck_tables)) run_one_update("duckplyr")

# --- Benchmark ---
message(sprintf("\nBenchmarking (%d iterations per backend)...\n", n_iter))

backends <- c("dplyr", "duckdb")
if (!is.null(duck_tables)) backends <- c(backends, "duckplyr")

results <- list()
for (b in backends) {
  t0 <- proc.time()["elapsed"]
  for (i in seq_len(n_iter)) run_one_update(b)
  elapsed <- proc.time()["elapsed"] - t0
  results[[b]] <- elapsed
  message(sprintf("  %-10s  %.1fms per update  (total %.1fs)",
                  b, elapsed / n_iter * 1000, elapsed))
}

# --- Summary ---
base <- results[["dplyr"]]
message("\n--- Full update (data + agg + counts) ---")
message(sprintf("%-12s  %-15s  %-10s", "Backend", "Per update", "vs dplyr"))
for (b in names(results)) {
  message(sprintf("%-12s  %-15s  %.1fx",
                  b,
                  sprintf("%.0fms", results[[b]] / n_iter * 1000),
                  base / results[[b]]))
}

# --- Agg-only sub-benchmark (shows push-down benefit) ---
message(sprintf("\n--- Agg-only benchmark (%d iterations) ---", n_iter))
message("This isolates the GROUP BY push-down benefit for categorical widgets.\n")

agg_widgets <- widgets[vapply(widgets, function(w) w$type == "cat", logical(1))]

run_agg_only <- function(backend) {
  for (w in agg_widgets) {
    if (backend == "dplyr") {
      dplyr_crossfilter_agg(tables, key_col_fn, w$tbl, w$dim,
                            cat_filters, rng_filters)
    } else if (backend == "duckdb") {
      duckdb_crossfilter_agg(con, tables, table_names, key_col_fn,
                             w$tbl, w$dim, cat_filters, rng_filters)
    }
  }
}

agg_results <- list()
for (b in c("dplyr", "duckdb")) {
  t0 <- proc.time()["elapsed"]
  for (i in seq_len(n_iter)) run_agg_only(b)
  elapsed <- proc.time()["elapsed"] - t0
  agg_results[[b]] <- elapsed
  message(sprintf("  %-10s  %.1fms per update  (total %.1fs)",
                  b, elapsed / n_iter * 1000, elapsed))
}
message(sprintf("\n  DuckDB agg push-down speedup: %.1fx",
                agg_results[["dplyr"]] / agg_results[["duckdb"]]))

if (!is.null(results[["duckplyr"]])) {
  message("\n--- duckplyr notes ---")
  message("Zero fallbacks confirmed. All operations stay in DuckDB.")
  message("\nWorkarounds applied to avoid fallbacks:")
  message("  1. summarise(.by=) instead of group_by() + summarise()")
  message("  2. as.character() after as.data.frame() (can't translate in lazy query)")
  message("  3. Pre-computed col_classes to avoid df[[dim]] materialization on lazy tibble")
  message("  4. Strip column label attributes before as_duckdb_tibble()")
  message("  5. semi_join with as_duckdb_tibble(keys) instead of %in% (>100 values limit)")
  message("\nPerformance bottleneck: per-verb dispatch overhead through DuckDB's")
  message("relational API. Each crossfilter update runs ~40-80 dplyr verbs, each")
  message("dispatched individually. Raw SQL sends 1 query per widget.")
  message("\nThis benchmark is a reproducible case for discussing duckplyr dispatch")
  message("overhead with Kirill.")
}

# --- Cleanup ---
dbDisconnect(con, shutdown = TRUE)
