test_that("the trail is a named character vector keyed by block id", {

  d <- add_filter_trail(data.frame(x = 1:3), NULL, "ae_flags", "SAFFL = Y")
  expect_identical(filter_trail(d), c(ae_flags = "SAFFL = Y"))

  d2 <- add_filter_trail(d, d, "global_filter", "SEX = F")
  expect_identical(
    filter_trail(d2),
    c(ae_flags = "SAFFL = Y", global_filter = "SEX = F")
  )

  # Re-evaluating a block replaces its own clause rather than appending a
  # second copy, which is the whole reason the trail is keyed.
  d3 <- add_filter_trail(d2, d2, "global_filter", "SEX = F, M")
  expect_length(filter_trail(d3), 2L)
  expect_identical(unname(filter_trail(d3)[["global_filter"]]), "SEX = F, M")

  # A filter with nothing selected leaves no trace.
  expect_null(filter_trail(add_filter_trail(data.frame(x = 1), NULL, "k", "")))
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

test_that("the trail crosses dm_filter and the dm-to-data-frame boundary", {

  skip_if_not_installed("dm")

  adsl <- data.frame(
    USUBJID = paste0("S", 1:6),
    SEX = c("F", "F", "M", "M", "F", "M"),
    stringsAsFactors = FALSE
  )
  lb <- data.frame(
    USUBJID = paste0("S", 1:6),
    LBSTRESN = c(10, 99, 99, 10, 10, 99),
    stringsAsFactors = FALSE
  )

  d <- dm::dm_add_fk(
    dm::dm_add_pk(dm::dm(adsl = adsl, lb = lb), adsl, USUBJID),
    lb, USUBJID, adsl
  )

  # What the crossfilter block emits: the dm_filter call, wrapped so the
  # result records what it did. `dm_filter()` drops attributes set on a dm,
  # which is exactly why the wrapper re-reads from the input.
  filtered <- eval(
    trail_expr(
      quote(dm::dm_filter(data, adsl = SEX == "F", lb = LBSTRESN > 50)),
      "global_filter",
      "adsl.SEX = F; lb.LBSTRESN 50 to 99"
    ),
    list(data = d)
  )

  expect_identical(
    filter_trail(filtered),
    c(global_filter = "adsl.SEX = F; lb.LBSTRESN 50 to 99")
  )

  # ...and what dm_flatten_block emits: a carry with no clause of its own.
  flat <- eval(
    trail_expr(
      quote(dm::dm_flatten_to_tbl(data, lb, adsl, .recursive = TRUE))
    ),
    list(data = filtered)
  )

  expect_identical(
    filter_trail(flat),
    c(global_filter = "adsl.SEX = F; lb.LBSTRESN 50 to 99")
  )

  # The filter really did cascade: only S2 is female with a lab value over 50.
  expect_identical(flat$USUBJID, "S2")

  # Ordinary dplyr verbs carry it on from there without help.
  expect_identical(
    filter_trail(dplyr::filter(flat, LBSTRESN > 0)),
    filter_trail(flat)
  )
})

test_that("two filter blocks on one path do not overwrite each other", {

  # The regression this guards. `expr_server.block()` calls EVERY block's own
  # server with `id = "expr"`, so keying the trail on that id filed every
  # clause under one name and each filter downstream erased the one before it:
  # a board with a flag filter above the global filter showed only the global
  # filter's clause. The key has to come from the module namespace, which
  # carries the board's block id.
  ns_of <- function(prefix) list(ns = function(x) paste0(prefix, "-expr-"))

  expect_false(identical(trail_key(ns_of("ae_flags")),
                         trail_key(ns_of("global_filter"))))

  d <- data.frame(x = 1:3)
  d <- add_filter_trail(d, NULL, trail_key(ns_of("ae_flags")), "TRTEMFL")
  d <- add_filter_trail(d, d, trail_key(ns_of("global_filter")), "SEX = F")

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
