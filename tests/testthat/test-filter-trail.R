test_that("the trail is a named character vector keyed by block", {

  d <- add_filter_trail(data.frame(x = 1:3), "ae_flags", "SAFFL = Y")
  expect_identical(filter_trail(d), c(ae_flags = "SAFFL = Y"))

  d2 <- add_filter_trail(d, "global_filter", "SEX = F")
  expect_identical(
    filter_trail(d2),
    c(ae_flags = "SAFFL = Y", global_filter = "SEX = F")
  )

  # Writing the same key again replaces the clause in place.
  d3 <- add_filter_trail(d2, "ae_flags", "SAFFL = N")
  expect_identical(
    filter_trail(d3),
    c(ae_flags = "SAFFL = N", global_filter = "SEX = F")
  )

  # A filter with nothing selected leaves no trace.
  expect_null(filter_trail(add_filter_trail(data.frame(x = 1), "k", "")))
  expect_null(filter_trail(add_filter_trail(data.frame(x = 1), "k", NULL)))
  expect_null(filter_trail(add_filter_trail(data.frame(x = 1), "k", NA)))
  expect_null(filter_trail(data.frame(x = 1)))
})

test_that("a clause reads as a sentence, and names the table only when it must", {

  expect_equal(
    crossfilter_clause(list(adsl = list(SEX = list("F"))), list()),
    "SEX = F"
  )

  # Two tables filtered: the table matters, because dm cascades a filter along
  # the key graph. A filter written against `lb` restricts `ae` as well, so a
  # clause that does not say where it was written is misleading.
  expect_equal(
    crossfilter_clause(
      list(adsl = list(SEX = list("F"))),
      list(lb = list(LBSTRESN = list(50, 99)))
    ),
    "adsl.SEX = F; lb.LBSTRESN 50 to 99"
  )

  expect_null(crossfilter_clause(list(), list()))

  # A high-cardinality pick elides rather than listing every subject.
  expect_match(
    crossfilter_clause(list(adsl = list(ID = as.list(LETTERS[1:9]))), list()),
    "A, B, C, D, E, F, …",
    fixed = TRUE
  )
})

trail_dm <- function() {
  adsl <- data.frame(
    USUBJID = paste0("S", 1:3),
    SEX = c("F", "M", "F"),
    stringsAsFactors = FALSE
  )
  ae <- data.frame(
    AEID = 1:3,
    USUBJID = c("S1", "S1", "S2"),
    TRTEMFL = c("Y", "N", "Y"),
    stringsAsFactors = FALSE
  )
  lb <- data.frame(
    LBID = 1:3,
    USUBJID = c("S1", "S2", "S3"),
    LBSTRESN = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  dm::dm(adsl = adsl, ae = ae, lb = lb) |>
    dm::dm_add_pk(adsl, USUBJID) |>
    dm::dm_add_fk(ae, USUBJID, adsl) |>
    dm::dm_add_fk(lb, USUBJID, adsl)
}

test_that("on a dm the trail sits on the tables", {

  d <- add_filter_trail(trail_dm(), "global_filter", "SEX = F")

  # Not on the dm object, which every dm verb rebuilds.
  expect_null(attr(d, "blockr_filters", exact = TRUE))
  for (tbl in dm::dm_get_tables(d)) {
    expect_identical(attr(tbl, "blockr_filters"), c(global_filter = "SEX = F"))
  }
  expect_identical(filter_trail(d), c(global_filter = "SEX = F"))

  # `tables` writes on those only, and an unknown table is skipped.
  d2 <- add_filter_trail(trail_dm(), "ae_flags", "TRTEMFL",
                         tables = c("ae", "nope"))
  expect_identical(filter_trail(d2), c(ae_flags = "TRTEMFL"))
  expect_identical(filter_trail(d2, tables = "ae"), c(ae_flags = "TRTEMFL"))
  expect_null(filter_trail(d2, tables = c("adsl", "lb")))
  d3 <- trail_dm()
  expect_identical(add_filter_trail(d3, "k", "x", tables = "nope"), d3)
})

test_that("the trail survives dm verbs and the dm-to-data-frame boundary", {

  # What the crossfilter block emits.
  filtered <- eval(
    trail_expr(
      quote(dm::dm_filter(data, adsl = SEX == "F", lb = LBSTRESN > 15)),
      "global_filter",
      "adsl.SEX = F; lb.LBSTRESN 15 to 30"
    ),
    list(data = trail_dm())
  )
  want <- c(global_filter = "adsl.SEX = F; lb.LBSTRESN 15 to 30")
  expect_identical(filter_trail(filtered), want)

  # A later dm verb rebuilds the dm and keeps the tables' attributes.
  expect_identical(filter_trail(dm::dm_filter(filtered, ae = TRUE)), want)
  expect_identical(filter_trail(dm::dm_select_tbl(filtered, adsl, lb)), want)

  # What the pull and flatten blocks emit: plain dm calls.
  flat <- dm::dm_flatten_to_tbl(filtered, lb, adsl)
  expect_identical(filter_trail(flat), want)
  expect_identical(flat$USUBJID, "S3")
  expect_identical(filter_trail(dm::pull_tbl(filtered, ae)), want)

  # Ordinary dplyr verbs carry it on from there.
  expect_identical(filter_trail(dplyr::filter(flat, LBSTRESN > 0)), want)
})

test_that("two filter blocks on one path do not overwrite each other", {

  # `expr_server.block()` calls EVERY block's own server with `id = "expr"`,
  # so keying the trail on that id filed every clause under one name and each
  # filter downstream erased the one before it. The key has to come from the
  # module namespace, which carries the board's block id.
  ns_of <- function(prefix) list(ns = function(x) paste0(prefix, "-expr-"))

  expect_false(identical(trail_key(ns_of("ae_flags")),
                         trail_key(ns_of("global_filter"))))

  d <- data.frame(x = 1:3)
  d <- add_filter_trail(d, trail_key(ns_of("ae_flags")), "TRTEMFL")
  d <- add_filter_trail(d, trail_key(ns_of("global_filter")), "SEX = F")

  expect_equal(unname(filter_trail(d)), c("TRTEMFL", "SEX = F"))

  # A session that cannot report a namespace still yields a usable key rather
  # than erroring, and the same block keeps the same one.
  expect_identical(trail_key(NULL), "block")
  expect_identical(trail_key(ns_of("x")), trail_key(ns_of("x")))
})

test_that("value_filter_clause renders columns the way the crossfilter does", {
  cols <- list(
    list(name = "SEX", mode = "multi", values = c("F", "M"), table = "adsl"),
    list(name = "AESEV", mode = "single", values = "SEVERE", table = "adae")
  )
  # Two tables filtered: qualified.
  expect_identical(
    value_filter_clause(cols),
    "adsl.SEX = F, M; adae.AESEV = SEVERE"
  )
  # One table: the column alone.
  expect_identical(value_filter_clause(cols[1L]), "SEX = F, M")
  # The or-operator reads as words; sentinels read as words.
  expect_identical(
    value_filter_clause(
      list(
        list(name = "A", values = "<NA>"),
        list(name = "B", values = c("x", "<empty>"))
      ),
      operator = "|"
    ),
    "A = NA or B = x, (empty)"
  )
  # Nothing selected leaves no trace.
  expect_null(value_filter_clause(list()))
  expect_null(value_filter_clause(list(list(name = "A", values = character()))))
  expect_null(value_filter_clause(NULL))
})

test_that("a subject-id claim reads as the clause that was clicked", {
  ids <- list(name = "USUBJID", mode = "multi", values = c("01", "02", "03"),
              label = "AEBODSYS = Gastrointestinal disorders")
  expect_identical(value_filter_clause(list(ids)),
                   "AEBODSYS = Gastrointestinal disorders")
  # No label: the ids, as before.
  ids$label <- NULL
  expect_identical(value_filter_clause(list(ids)), "USUBJID = 01, 02, 03")
})

test_that("trail_expr wraps only when there is a clause", {
  expect_identical(
    trail_expr(quote(f(data)), "k", NULL),
    quote(f(data))
  )
  expect_identical(
    trail_expr(quote(f(data)), "k", ""),
    quote(f(data))
  )
  expect_identical(
    trail_expr(quote(f(data)), "k", "SEX = F"),
    quote(blockr.dm::add_filter_trail(f(data), "k", "SEX = F"))
  )
  expect_identical(
    trail_expr(quote(f(data)), "k", "TRTEMFL", tables = "ae"),
    quote(blockr.dm::add_filter_trail(f(data), "k", "TRTEMFL", tables = "ae"))
  )
})

test_that("the devmaster shape: an AE-only flag leaves the lab branch alone", {

  # What the flag filter emits in dm mode: one table narrowed without a
  # cascade, the clause on that table only.
  flagged <- eval(
    trail_expr(
      quote(dm::dm_update_zoomed(
        dplyr::filter(dm::dm_zoom_to(data, ae), TRTEMFL == "Y")
      )),
      "ae_flags", "TRTEMFL", tables = "ae"
    ),
    list(data = trail_dm())
  )
  expect_identical(filter_trail(flagged), c(ae_flags = "TRTEMFL"))
  expect_equal(nrow(dm::pull_tbl(flagged, adsl)), 3L)

  # The crossfilter on top, on every table, because dm_filter cascades.
  both <- eval(
    trail_expr(
      quote(dm::dm_filter(data, adsl = SEX == "F")),
      "global_filter", "SEX = F"
    ),
    list(data = flagged)
  )
  # The union keeps the order the filters ran in, although adsl, the first
  # table, has only the second clause.
  expect_identical(names(filter_trail(both)), c("ae_flags", "global_filter"))

  # Leaving the dm through lb, as the Lab flatten does: the AE clause stays
  # behind and the global one comes along.
  lab <- dm::dm_flatten_to_tbl(both, lb, adsl)
  expect_identical(filter_trail(lab), c(global_filter = "SEX = F"))
  expect_identical(sort(lab$USUBJID), c("S1", "S3"))

  # Leaving through ae keeps both, in order.
  aes <- dm::dm_flatten_to_tbl(both, ae, adsl)
  expect_identical(unname(filter_trail(aes)), c("TRTEMFL", "SEX = F"))
  expect_identical(aes$USUBJID, "S1")

  # And a pull of adsl alone, as the Population view does.
  expect_identical(filter_trail(dm::pull_tbl(both, adsl)),
                   c(global_filter = "SEX = F"))
})

test_that("merge_trails keeps the order the clauses were added", {
  expect_identical(merge_trails(list()), character())
  expect_identical(
    merge_trails(list(
      adsl = c(b = "2", d = "4"),
      ae = c(a = "1", b = "2", c = "3", d = "4"),
      lb = c(b = "2", e = "5")
    )),
    c(a = "1", b = "2", e = "5", c = "3", d = "4")
  )
})

test_that("dm keeps table attributes through the verbs the trail relies on", {
  # The trail depends on this; if dm ever stops doing it, this fails first.
  d <- trail_dm()
  tbls <- dm::dm_get_tables(d)
  d <- do.call(dm::dm_mutate_tbl, c(list(d), lapply(tbls, function(t) {
    attr(t, "probe") <- "yes"
    t
  })))
  probe <- function(x) {
    if (inherits(x, "dm")) {
      unname(vapply(dm::dm_get_tables(x), attr, "", "probe"))
    } else {
      attr(x, "probe")
    }
  }
  expect_identical(probe(dm::dm_filter(d, adsl = SEX == "F")), rep("yes", 3))
  expect_identical(probe(dm::dm_filter(d, ae = TRTEMFL == "Y")), rep("yes", 3))
  zoomed <- dm::dm_update_zoomed(
    dplyr::filter(dm::dm_zoom_to(d, ae), TRTEMFL == "Y")
  )
  expect_identical(probe(zoomed), rep("yes", 3))
  expect_identical(probe(dm::pull_tbl(d, lb)), "yes")
  expect_identical(probe(dm::dm_flatten_to_tbl(d, lb, adsl)), "yes")
  expect_identical(
    probe(dm::dm_flatten_to_tbl(d, ae, adsl, .join = dplyr::inner_join)),
    "yes"
  )
})

test_that("provenance is read off the first table that has one", {
  d <- trail_dm()
  adsl <- dm::pull_tbl(d, adsl)
  attr(adsl, "blockr_provenance") <- list(dataset = "AQ-001", path = "/d/aq")
  d <- dm::dm_mutate_tbl(d, adsl = adsl)
  expect_identical(provenance(d)$dataset, "AQ-001")
  # a filter rebuilds the dm, the table keeps it
  expect_identical(provenance(dm::dm_filter(d, adsl = SEX == "F"))$path,
                   "/d/aq")
  expect_null(provenance(data.frame(x = 1)))
})
