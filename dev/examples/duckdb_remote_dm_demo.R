# Remote (DuckDB) relational data as a dm, explored with the dm preview
# ---------------------------------------------------------------------------
# The blockr.dplyr counterpart (blockr.dplyr/dev/duckdb-lazy-dock-dag-demo.R)
# opens a larger-than-RAM parquet as a lazy DuckDB tbl and runs it through a
# chain of dplyr blocks that all stay lazy. This is the dm version of the same
# idea: several related parquet files are opened as lazy DuckDB tables, bound
# into a single `dm` (data model) with primary / foreign keys, and explored
# with the dm Key-lines preview -- the diagram and its click-to-preview page
# the remote tables in the database (LIMIT / window query) instead of ever
# collecting them.
#
#   [fn(...): open DuckDB, read_parquet views -> dm() + keys]   <- arity-0 SOURCE
#       -> dm_select  (passthrough -> the Key-lines DIAGRAM preview)
#       -> dm_flatten (star join across the FKs, stays lazy)
#
# A star schema: one large fact table (`orders`, 5,000,000 rows, never held in
# an R data.frame) and three small dimensions (customers, products,
# categories). Click a table row in the diagram to preview its rows -- the
# 5M-row `orders` table pages in DuckDB just as fast as the 6-row
# `categories` table, because the preview never counts or collects it.
#
# The reader is a VARIADIC function block (`function(...)`): it takes no input,
# so it is a true arity-0 source -- no upstream block needed.

options(
  blockr.tabular_display = blockr.ui::html_table_display,   # lazy-aware HTML preview (dm + flatten)
  shiny.port = 3822,
  # shiny.host = "0.0.0.0",
  shiny.launch.browser = FALSE
)

library(shiny)
pkgload::load_all("blockr.core", quiet = TRUE)
pkgload::load_all("blockr.ui", quiet = TRUE)
pkgload::load_all("blockr.dplyr", quiet = TRUE)
pkgload::load_all("blockr.dm", quiet = TRUE)
pkgload::load_all("blockr.extra", quiet = TRUE)   # new_function_var_block source
pkgload::load_all("blockr.dock", quiet = TRUE)   # new_function_var_block source
pkgload::load_all("blockr.dag", quiet = TRUE)   # new_function_var_block source

stopifnot(
  requireNamespace("duckdb", quietly = TRUE),
  requireNamespace("DBI", quietly = TRUE),
  requireNamespace("dbplyr", quietly = TRUE)
)

# --- one-time: a star schema on disk as parquet ------------------------------
# `orders` is generated and streamed by DuckDB itself (never materialized in
# R); the three dimensions are tiny and built in R, then written via DuckDB.
#
# The parquet lives in a STABLE, OS-appropriate cache dir -- NOT tempdir() --
# on purpose: the reader block bakes these absolute paths into its function
# code, and that code is the block's persisted state. A tempdir() path is a
# fresh `…/RtmpXXXX/` every session, so it would 404 after an app restart or a
# board Save/Restore. R_user_dir() is stable across sessions and portable
# across machines, so the demo generates the data once and just reuses it.
dir <- file.path(tools::R_user_dir("blockr.dm", "cache"), "remote-dm-demo")
dir.create(dir, recursive = TRUE, showWarnings = FALSE)
f_cat <- file.path(dir, "categories.parquet")
f_prd <- file.path(dir, "products.parquet")
f_cus <- file.path(dir, "customers.parquet")
f_ord <- file.path(dir, "orders.parquet")

n_orders <- 5000000L

if (!all(file.exists(c(f_cat, f_prd, f_cus, f_ord)))) {
  message("generating star-schema parquet via DuckDB (one-time)...")
  con0 <- DBI::dbConnect(duckdb::duckdb())

  categories <- data.frame(
    category_id = 1:6,
    category_name = c(
      "Electronics", "Clothing", "Food", "Books", "Sports", "Home"
    ),
    stringsAsFactors = FALSE
  )
  set.seed(42)
  products <- data.frame(
    product_id = 1:50,
    product_name = paste0("Product_", 1:50),
    category_id = sample(1:6, 50, replace = TRUE),
    unit_price = round(runif(50, 5, 999), 2),
    stringsAsFactors = FALSE
  )
  customers <- data.frame(
    customer_id = 1:1000,
    name = paste0("Customer_", 1:1000),
    city = sample(
      c("New York", "Chicago", "Boston", "Denver", "Seattle", "Miami"),
      1000, replace = TRUE
    ),
    segment = sample(c("Consumer", "Business", "Enterprise"), 1000,
                     replace = TRUE),
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con0, "categories", categories)
  DBI::dbWriteTable(con0, "products", products)
  DBI::dbWriteTable(con0, "customers", customers)
  DBI::dbExecute(con0, sprintf(
    "COPY categories TO '%s' (FORMAT parquet)", f_cat))
  DBI::dbExecute(con0, sprintf(
    "COPY products TO '%s' (FORMAT parquet)", f_prd))
  DBI::dbExecute(con0, sprintf(
    "COPY customers TO '%s' (FORMAT parquet)", f_cus))

  # the fact table: 5,000,000 rows, generated inside DuckDB and streamed to
  # parquet -- it is never an R object.
  DBI::dbExecute(con0, sprintf("
    COPY (SELECT i + 1 AS order_id,
                 (1 + (random() * 999)::INT) AS customer_id,
                 (1 + (random() * 49)::INT) AS product_id,
                 (1 + (random() * 4)::INT) AS quantity,
                 DATE '2024-01-01' + CAST((random() * 364) AS INT) AS order_date
          FROM range(%d) t(i)) TO '%s' (FORMAT parquet)", n_orders, f_ord))

  DBI::dbDisconnect(con0, shutdown = TRUE)
}
message(sprintf("orders parquet: %s (%.1f MB, %s rows)",
                f_ord, file.size(f_ord) / 1e6,
                format(n_orders, big.mark = ",")))

# --- arity-0 SOURCE: open DuckDB, expose parquet as views, return a LAZY dm --
# `function(...)` takes no input, so the block is a root. Self-qualified
# (DBI::/duckdb::/dplyr::/dm::) because a block expr runs sandboxed. All four
# tables share ONE connection, so dm key checks and the flatten join push to
# SQL on the same backend. Keys are metadata-only (check = FALSE by default),
# so building the dm runs no scan over the 5M-row fact table.
read_remote_dm_fn <- sprintf(
  "function(...) {
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, \"CREATE OR REPLACE VIEW categories AS SELECT * FROM read_parquet('%s')\")
  DBI::dbExecute(con, \"CREATE OR REPLACE VIEW products   AS SELECT * FROM read_parquet('%s')\")
  DBI::dbExecute(con, \"CREATE OR REPLACE VIEW customers  AS SELECT * FROM read_parquet('%s')\")
  DBI::dbExecute(con, \"CREATE OR REPLACE VIEW orders     AS SELECT * FROM read_parquet('%s')\")

  d <- dm::dm(
    categories = dplyr::tbl(con, \"categories\"),
    products   = dplyr::tbl(con, \"products\"),
    customers  = dplyr::tbl(con, \"customers\"),
    orders     = dplyr::tbl(con, \"orders\")
  )
  d <- dm::dm_add_pk(d, categories, category_id)
  d <- dm::dm_add_pk(d, products, product_id)
  d <- dm::dm_add_pk(d, customers, customer_id)
  d <- dm::dm_add_pk(d, orders, order_id)
  d <- dm::dm_add_fk(d, products, category_id, categories)
  d <- dm::dm_add_fk(d, orders, customer_id, customers)
  d <- dm::dm_add_fk(d, orders, product_id, products)
  d
}",
  f_cat, f_prd, f_cus, f_ord
)

# --- materialize: pull the (now small) filtered join into a data.frame -------
# This is the blockr.dplyr DuckDB demo's lesson made literal: DON'T collect the
# 5,000,000-row join. The `filtered` block upstream cuts it down to ~1,000 rows
# IN DuckDB first (the filter pushes a WHERE clause to SQL), so here we can
# collect() the whole -- now tiny -- result safely, no head() bound needed. The
# collected table renders in the same HTML preview with a KNOWN total
# ("1-5 of 999") -- the visible lazy -> filtered -> materialized hand-off.
collect_fn <- "function(data) {
  dplyr::collect(data)
}"

board <- new_dock_board(
  blocks = c(
    # arity-0 lazy source -> a remote dm with keys
    remote = new_function_var_block(fn = read_remote_dm_fn),

    # the dm preview: Key-lines diagram + click-to-preview, lazy on the remote
    # tables (dm_select is a passthrough that keeps every table)
    model = new_dm_select_block(),

    # star join across the FKs -- stays lazy, previews via the same engine
    joined = new_dm_flatten_block(start_table = "orders", recursive = TRUE),

    # cut 5,000,000 rows down to ~1,000 IN DuckDB (the filter pushes a WHERE to
    # SQL) so the collect downstream is safe. Filtering happens on the LAZY
    # table; only the small filtered result is ever pulled into R.
    filtered = new_filter_block(
      conditions = list(
        list(type = "numeric", column = "order_id", op = "<", value = 1000)
      ),
      operator = "&"
    ),

    # materialize the (now small) filtered join into an in-memory data.frame
    collected = new_function_block(fn = collect_fn)
  ),
  links = links(
    from  = c("remote", "model", "joined", "filtered"),
    to    = c("model", "joined", "filtered", "collected"),
    input = c("data", "data", "data", "data")
  ),
  extensions = new_dock_extensions(list(
    new_dag_extension()
  ))
)

serve(board, "duckdb-remote-dm")
