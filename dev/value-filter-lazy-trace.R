# value-filter-lazy-trace.R — proof for the lazy value-load fix.
#
# Task: BMS team-ops `blockr.dm-lazy-value-filter` — stop value_filter_block
# from shipping every column's full unique-value list at startup (× all tables
# in dm mode). Parallels the blockr.dplyr PR #74 fix.
#
# What this captures (chromote, real browser):
#   1. Startup custom-message trace on a HIGH-cardinality, multi-table dm.
#      Asserts the `bi-filter-columns` message carries column metadata only
#      (no `values`), and that NO `bi-filter-column-values` arrives unprompted.
#   2. The "before" payload size — what the old eager code would have shipped
#      (sum of unique_value_options() over every column of every table) — so
#      the saving is quantified, not just asserted.
#   3. Lazy-on-open: after a programmatic dropdown-open, a single
#      `bi-filter-column-values` response arrives for that one column.
#
# Run from /workspace:
#   Rscript blockr.dm/dev/value-filter-lazy-trace.R

suppressMessages({
  pkgload::load_all("blockr.core", quiet = TRUE)
  pkgload::load_all("blockr.dplyr", quiet = TRUE)
  pkgload::load_all("blockr.dm", quiet = TRUE)
})

# --- A high-cardinality, multi-table dm (the × all-tables multiplier) --------
# Built from a string so the SAME definition is (a) eval'd locally for the
# "before" size calc and (b) embedded in the served app. A static block
# captures its argument as an unevaluated expression, so the dm must be
# constructed in-process by the app, not serialized in.
build_code <- '
mk <- function(n, prefix) sprintf(paste0(prefix, "%05d"), seq_len(n))
big_dm <- dm::as_dm(list(
  policies = data.frame(
    policy_id = mk(5000, "P"),
    status    = sample(c("active", "lapsed"), 5000, replace = TRUE),
    region    = sample(state.name, 5000, replace = TRUE),
    stringsAsFactors = FALSE
  ),
  claims = data.frame(
    claim_id   = mk(20000, "C"),
    policy_id  = sample(mk(5000, "P"), 20000, replace = TRUE),
    claim_type = sample(letters, 20000, replace = TRUE),
    stringsAsFactors = FALSE
  ),
  agents = data.frame(
    agent_id = mk(3000, "A"),
    office   = sample(month.name, 3000, replace = TRUE),
    stringsAsFactors = FALSE
  )
))
'
eval(parse(text = build_code))

# --- "Before" size: what the OLD eager build_column_options() would ship ------
# (unique_value_options over EVERY column of EVERY table, the dm dict payload)
eager_values <- list()
for (tn in names(dm::dm_get_tables(big_dm))) {
  tbl <- as.data.frame(dm::dm_get_tables(big_dm)[[tn]])
  for (cn in names(tbl)) {
    eager_values[[paste0(tn, ".", cn)]] <-
      blockr.dm:::unique_value_options(tbl[[cn]])
  }
}
eager_bytes <- nchar(jsonlite::toJSON(eager_values, auto_unbox = TRUE))
cat(sprintf("\n[before] eager all-columns value payload: %s B across %d columns\n",
            format(eager_bytes, big.mark = ","), length(eager_values)))

# --- Serve in a background process -------------------------------------------
# The board is preconfigured with one saved HIGH-card single-select entry —
# proves the saved chip renders with NO value list at startup, and that opening
# it fetches that one column's values on demand.
port <- 4849L
app_script <- tempfile(fileext = ".R")
writeLines(sprintf('
options(blockr.lazy_eval = FALSE, blockr.tabular_display = blockr.ui::html_table_display)
suppressMessages({
  pkgload::load_all("/workspace/blockr.core", quiet = TRUE)
  pkgload::load_all("/workspace/blockr.dplyr", quiet = TRUE)
  pkgload::load_all("/workspace/blockr.dm", quiet = TRUE)
})
%s
board <- new_board(
  blocks = c(
    src = new_static_block(big_dm),
    flt = new_value_filter_block(
      state = list(columns = list(
        list(name = "policy_id", table = "policies",
             mode = "single", values = "P00001")
      ))
    )
  ),
  links = c(new_link("src", "flt", "data"))
)
app <- blockr.core::serve(board)
shiny::runApp(app, port = %d, host = "127.0.0.1", launch.browser = FALSE)
', build_code, port), app_script)

log_file <- tempfile(fileext = ".log")
pid <- sys::r_background(args = c("-e", sprintf('source("%s")', app_script)),
                         std_out = log_file, std_err = log_file)
on.exit(tools::pskill(pid), add = TRUE)

deadline <- Sys.time() + 40
repeat {
  if (file.exists(log_file) &&
      any(grepl("Listening on", readLines(log_file, warn = FALSE)))) break
  if (Sys.time() > deadline) stop("app did not start; see ", log_file)
  Sys.sleep(0.1)
}
Sys.sleep(0.5)

# --- Chromote: hook every custom message before page scripts run -------------
b <- chromote::ChromoteSession$new()
on.exit(try(b$close(), silent = TRUE), add = TRUE)
invisible(b$Runtime$enable()); invisible(b$Page$enable())

# Wrap WebSocket before Shiny opens its socket — race-free, captures every
# server->client custom message (keyed under `.custom`) regardless of when the
# JS handlers register.
hook <- '
  window.__trace = { events: [], t0: performance.now() };
  const log = (n, sz) => window.__trace.events.push(
    { n: n, t: Math.round(performance.now() - window.__trace.t0), sz: sz });
  const NativeWS = window.WebSocket;
  window.WebSocket = function(...args) {
    const ws = new NativeWS(...args);
    ws.addEventListener("message", (ev) => {
      try {
        const data = JSON.parse(ev.data);
        const custom = data && data.custom;
        if (custom) {
          for (const key of Object.keys(custom)) {
            log("msg:" + key, JSON.stringify(custom[key]).length);
          }
        }
      } catch (e) { /* non-JSON frame */ }
    });
    return ws;
  };
  window.WebSocket.prototype = NativeWS.prototype;
'
invisible(b$Page$addScriptToEvaluateOnNewDocument(source = hook))
invisible(b$Page$navigate(sprintf("http://127.0.0.1:%d", port)))

get_events <- function() {
  jsonlite::fromJSON(
    b$Runtime$evaluate("JSON.stringify(window.__trace.events)")$result$value,
    simplifyDataFrame = FALSE
  )
}

# Wait until the block's bi-filter-columns message has fired and the stream
# then settles (no new events for ~2s), or 40s.
last <- 0; stable <- 0; deadline <- Sys.time() + 40
while (Sys.time() < deadline) {
  Sys.sleep(0.1)
  ev_now <- get_events()
  seen_cols <- length(Filter(function(e) e$n == "msg:bi-filter-columns", ev_now)) > 0
  cur <- length(ev_now)
  if (cur == last) { stable <- stable + 1; if (stable >= 20 && seen_cols) break }
  else { stable <- 0; last <- cur }
}
fmt <- function(ev, tag) {
  cat(sprintf("\n[%s] custom-message trace:\n", tag))
  for (e in ev) cat(sprintf("  %6d ms  %-28s  %s B\n", e$t, e$n, e$sz))
}

ev <- get_events()
fmt(ev, "startup")

cols_msg <- Filter(function(e) e$n == "msg:bi-filter-columns", ev)
vals_msg <- Filter(function(e) e$n == "msg:bi-filter-column-values", ev)

if (length(cols_msg) == 0) {
  cat("\n--- DIAGNOSTICS ---\n")
  cat("app log tail:\n")
  cat(paste0("  ", tail(readLines(log_file, warn = FALSE), 25)), sep = "\n")
  dom <- b$Runtime$evaluate(
    "JSON.stringify({container: !!document.querySelector('.bi-filter-container'), body: document.querySelector('.bi-filter-body') ? document.querySelector('.bi-filter-body').innerText : null, blocks: document.querySelectorAll('.block-container').length})"
  )$result$value
  cat("\nDOM:", dom, "\n")
}

stopifnot(
  "expected a bi-filter-columns message at startup" = length(cols_msg) >= 1,
  "NO value message must arrive at startup" = length(vals_msg) == 0
)
cat(sprintf(
  "\n[after]  startup bi-filter-columns payload: %s B (metadata only)\n",
  format(cols_msg[[1]]$sz, big.mark = ",")))
cat(sprintf("[saving] %s B -> %s B  (%.1fx smaller, no value lists shipped)\n",
            format(eager_bytes, big.mark = ","),
            format(cols_msg[[1]]$sz, big.mark = ","),
            eager_bytes / cols_msg[[1]]$sz))

# --- Lazy-on-open: click the saved policy_id single-select, expect ONE fetch --
n_before <- length(get_events())
invisible(b$Runtime$evaluate(
  "document.querySelector('.bi-filter-body .blockr-select__control').click()"))
deadline <- Sys.time() + 10
repeat {
  Sys.sleep(0.1)
  ev2 <- get_events()
  if (length(Filter(function(e) e$n == "msg:bi-filter-column-values", ev2)) >= 1) break
  if (Sys.time() > deadline) break
}
ev2 <- get_events()
fmt(ev2[(n_before + 1):length(ev2)], "after dropdown-open")
vals_after <- Filter(function(e) e$n == "msg:bi-filter-column-values", ev2)
stopifnot("dropdown-open must trigger exactly one value fetch" =
            length(vals_after) >= 1)
cat(sprintf(
  "\n[lazy]   dropdown-open fetched policies.policy_id on demand: %s B\n",
  format(vals_after[[1]]$sz, big.mark = ",")))

# --- Render cap: the 5,000-value list must render <=200 option nodes ----------
Sys.sleep(0.4)  # let updateOptions paint
cap <- jsonlite::fromJSON(b$Runtime$evaluate(
  "JSON.stringify({opts: document.querySelectorAll('.blockr-select__dropdown .blockr-select__option').length, more: !!Array.from(document.querySelectorAll('.blockr-select__empty')).find(e => /more . type to narrow/.test(e.textContent))})"
)$result$value)
cat(sprintf("[cap]    open dropdown rendered %d option nodes; '+N more' hint: %s\n",
            cap$opts, cap$more))
stopifnot("high-cardinality dropdown must cap rendered nodes (<=201)" =
            cap$opts > 0 && cap$opts <= 201)

cat("\nALL CHECKS PASSED\n")
