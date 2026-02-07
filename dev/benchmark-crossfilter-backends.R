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

cat("Tables loaded:\n")
for (nm in names(tables)) {
  cat(sprintf("  %-10s %s rows x %s cols\n", nm, nrow(tables[[nm]]),
              ncol(tables[[nm]])))
}
cat(sprintf("  Total: %s rows\n\n", sum(vapply(tables, nrow, integer(1)))))

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
  # as_duckdb_tibble is the non-deprecated API (duckplyr >= 1.0.0)
  as_duck <- if (exists("as_duckdb_tibble", asNamespace("duckplyr"))) {
    duckplyr::as_duckdb_tibble
  } else {
    duckplyr::as_duckplyr_tibble
  }
  setNames(lapply(table_names, function(nm) as_duck(tables[[nm]])),
           table_names)
}, error = function(e) NULL)

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
cat("Correctness check...\n")
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
  cat(sprintf("  %-6s.%-6s  dplyr=%d  duckdb=%d  OK\n",
              w$tbl, w$dim, nrow(r_dplyr), nrow(r_duck)))
}

counts_dplyr <- dplyr_crossfilter_counts(tables, key_col_fn, table_names,
                                          cat_filters, rng_filters)
counts_duck <- duckdb_crossfilter_counts(con, tables, table_names, key_col_fn,
                                          cat_filters, rng_filters)
stopifnot(counts_dplyr$filtered == counts_duck$filtered)
cat(sprintf("  Counts: dplyr=%d  duckdb=%d  OK\n\n", counts_dplyr$filtered,
            counts_duck$filtered))

# --- Warmup ---
cat("Warming up...\n")
for (b in c("dplyr", "duckdb")) run_one_update(b)
if (!is.null(duck_tables)) run_one_update("duckplyr")

# --- Benchmark ---
cat(sprintf("\nBenchmarking (%d iterations per backend)...\n\n", n_iter))

backends <- c("dplyr", "duckdb")
if (!is.null(duck_tables)) backends <- c(backends, "duckplyr")

results <- list()
for (b in backends) {
  t0 <- proc.time()["elapsed"]
  for (i in seq_len(n_iter)) run_one_update(b)
  elapsed <- proc.time()["elapsed"] - t0
  results[[b]] <- elapsed
  cat(sprintf("  %-10s  %.1fms per update  (total %.1fs)\n",
              b, elapsed / n_iter * 1000, elapsed))
}

# --- Summary ---
base <- results[["dplyr"]]
cat("\n--- Summary ---\n")
cat(sprintf("%-12s  %-15s  %-10s\n", "Backend", "Per update", "vs dplyr"))
for (b in names(results)) {
  cat(sprintf("%-12s  %-15s  %.1fx\n",
              b,
              sprintf("%.0fms", results[[b]] / n_iter * 1000),
              base / results[[b]]))
}

if (!is.null(results[["duckplyr"]])) {
  cat("\n--- duckplyr notes ---\n")
  cat("Uses !!rlang::sym() (not .data[[]]), avoids between(),\n")
  cat("uses group_by+summarise (not .by), keeps queries lazy until collect.\n")
  cat("If still slower: per-verb dispatch overhead through DuckDB's relational\n")
  cat("API accumulates across 40-80 operations per update, while raw SQL\n")
  cat("sends one query string per widget.\n")
}

# --- Cleanup ---
dbDisconnect(con, shutdown = TRUE)
