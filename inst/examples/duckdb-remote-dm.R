# Remote (DuckDB) relational data as a dm, explored with the dm preview.
# Two things this demo highlights:
#
#   1. the dm PREVIEW  -- the Key-lines diagram + click-to-page-a-table view
#      that every dm block renders (block_output.dm_block). Click a table in
#      the diagram to page its rows.
#   2. the LAZY dm     -- several related parquet files opened as lazy DuckDB
#      tables, bound into a single `dm` (data model) with primary / foreign
#      keys and explored WITHOUT ever collecting the 5,000,000-row fact table.
#
# A star schema: one large fact table (`orders`, 5,000,000 rows, never held in
# an R data.frame) and three small dimensions (customers, products,
# categories). The 5M-row `orders` table pages in DuckDB just as fast as the
# 6-row `categories` table, because the preview never counts or collects it.
#
#   [fn(...): open DuckDB, read_parquet views -> dm() + keys]   <- arity-0 SOURCE
#       -> model    (dm_select passthrough -> the Key-lines DIAGRAM preview)
#       -> joined   (dm_flatten star join across the FKs, stays lazy)
#       -> filtered (cut 5,000,000 -> ~1000 rows IN DuckDB)
#       -> collected(materialize the now-small filtered join into a data.frame)
#
#   example  (dm_example_block: a small in-memory dm rendered with the SAME
#            Key-lines preview; switch datasets in the dropdown to see others)
#
# Run with:
#   source(system.file("examples/duckdb-remote-dm.R", package = "blockr.dm"))
#
# ---- Package loading (dual: installed vs local source) ---------------------
# `dev_local = FALSE` (the default, and what ships) attaches the INSTALLED
# packages with library(). Set it to TRUE -- or source this file from the
# dev/duckdb-remote-dm.R wrapper -- to load every blockr package from its LOCAL
# source checkout with pkgload::load_all(). One board, two loaders, no drift.
if (!exists("dev_local")) dev_local <- FALSE

options(blockr.tabular_display = blockr.ui::html_table_display)  # lazy-aware HTML preview (dm + flatten)

blockr_pkgs <- c(
  "blockr.core",
  "blockr.session", # project save / load / versions
  "blockr.ui",
  "blockr.dplyr",
  "blockr.dm",       # dm blocks (dm_select, dm_flatten, dm_example) + Key-lines preview
  "blockr.dock",
  "blockr.dag",
  "blockr.extra"     # new_function_var_block (variadic source) + new_function_block
)

for (pkg in blockr_pkgs) {
  if (dev_local) pkgload::load_all(pkg, quiet = TRUE)
  else library(pkg, character.only = TRUE)
}

# ---- Curate the block browser ----------------------------------------------
# Keep ONLY `dataset` and `glue` from blockr.core; drop its low-level / noise
# blocks (subset, merge, rbind, head, scatter, csv, filebrowser, upload) via
# unregister_blocks(), selecting by the registry `package` attribute so only
# core blocks are affected.
core_keep <- c("dataset_block", "glue_block")
core_drop <- setdiff(
  names(Filter(
    function(entry) identical(attr(entry, "package"), "blockr.core"),
    available_blocks()
  )),
  core_keep
)
unregister_blocks(core_drop)

# DuckDB pushdown backend (CRAN).
requireNamespace("duckdb", quietly = TRUE)   # embedded OLAP engine, reads parquet
requireNamespace("DBI", quietly = TRUE)      # database connection layer
requireNamespace("dbplyr", quietly = TRUE)   # translates dplyr verbs to SQL
stopifnot(
  requireNamespace("duckdb", quietly = TRUE),
  requireNamespace("DBI", quietly = TRUE),
  requireNamespace("dbplyr", quietly = TRUE)
)

# --- one-time: a star schema on disk as parquet ------------------------------
# `orders` is generated and streamed by DuckDB itself (never materialized in
# R); the three dimensions are tiny and built in R, then written via DuckDB.
#
# A fixed, writable path (NOT tempdir()) keeps the absolute paths the reader
# block bakes into its function code valid for the life of the session /
# container. Generated once, reused for every run. Override the fact-table size
# before sourcing for a lighter run: `n_orders <- 5e5L; source(...)`.
if (!exists("n_orders")) n_orders <- 5000000L
n_orders <- as.integer(n_orders)

dir <- "/tmp/blockr-dm-remote-demo"
dir.create(dir, recursive = TRUE, showWarnings = FALSE)
f_cat <- file.path(dir, "categories.parquet")
f_prd <- file.path(dir, "products.parquet")
f_cus <- file.path(dir, "customers.parquet")
f_ord <- file.path(dir, "orders.parquet")

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
# DON'T collect the 5,000,000-row join. The `filtered` block upstream cuts it
# down to ~1,000 rows IN DuckDB first (the filter pushes a WHERE clause to
# SQL), so here we can collect() the whole -- now tiny -- result safely.
collect_fn <- "function(data) {
  dplyr::collect(data)
}"

board <- new_dock_board(
  blocks = c(
    # --- dm PREVIEW (headline): the Key-lines diagram of the LAZY remote dm.
    # dm_select is a passthrough that keeps every table; click a table to page
    # its rows in DuckDB.
    model = new_dm_select_block(),

    # --- another dm PREVIEW: a small in-memory example dm rendered with the
    # SAME Key-lines preview. Arity-0 source; switch datasets in the dropdown.
    example = new_dm_example_block(),

    # arity-0 lazy source -> a remote dm with keys (the reader's function code)
    remote = new_function_var_block(fn = read_remote_dm_fn),

    # star join across the FKs -- stays lazy, previews via the same engine
    joined = new_dm_flatten_block(start_table = "orders", recursive = TRUE),

    # cut 5,000,000 rows down to ~1,000 IN DuckDB (the filter pushes a WHERE to
    # SQL) so the collect downstream is safe.
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
  )),
  # Land on the dm PREVIEW. Left column, stacked: the LAZY remote dm preview
  # (`model`) on top, the in-memory example dm preview (`example`) below -- each
  # in its OWN solo panel so the Key-lines DIAGRAM is always what shows (a
  # tabbed leaf would let the last-rendered downstream block steal focus). The
  # right tab strip holds everything else -- the Workflow graph, the star join,
  # the filter, the reader code, and the materialized result -- so the function
  # code is never the first thing a visitor sees.
  grids = list(
    Main = dock_grid(
      group(
        "model",
        "example",
        sizes = c(0.55, 0.45)
      ),
      # ext(), not "dag_extension": extension ids are container-owned, so the
      # class-derived name resolves to nothing and the tab silently vanishes.
      panels(ext("dag"), "joined", "filtered", "remote", "collected",
             active = "collected"),
      sizes = c(0.64, 0.36)
    )
  )
)

serve(board, "duckdb-remote-dm", plugins = custom_plugins(manage_project()))
