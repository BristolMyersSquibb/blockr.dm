test_that("cdisc dm block constructor", {
  block <- new_cdisc_dm_block()
  expect_s3_class(
    block,
    c("cdisc_dm_block", "dm_block", "transform_block", "block")
  )
})

test_that("ADAM key setup - PK on ADSL, FK on children", {
  block <- new_cdisc_dm_block(dedup_cols = FALSE)

  adsl <- data.frame(
    USUBJID = c("S1", "S2", "S3"),
    STUDYID = rep("STUDY1", 3),
    AGE = c(30, 40, 50),
    SEX = c("M", "F", "M"),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("S1", "S1", "S2"),
    STUDYID = rep("STUDY1", 3),
    AGE = c(30, 30, 40),
    AEDECOD = c("Headache", "Nausea", "Fatigue"),
    stringsAsFactors = FALSE
  )
  adlb <- data.frame(
    USUBJID = c("S1", "S2", "S3", "S1"),
    STUDYID = rep("STUDY1", 4),
    AGE = c(30, 40, 50, 30),
    PARAM = c("ALT", "ALT", "ALT", "AST"),
    stringsAsFactors = FALSE
  )

  dm_input <- dm::dm(adsl = adsl, adae = adae, adlb = adlb)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "dm")

      # Check PK on ADSL
      pks <- dm::dm_get_all_pks(result)
      expect_true("adsl" %in% pks$table)

      # Check FKs on children
      fks <- dm::dm_get_all_fks(result)
      expect_true("adae" %in% fks$child_table)
      expect_true("adlb" %in% fks$child_table)
      expect_true(all(fks$parent_table == "adsl"))
    },
    args = list(
      x = block,
      data = list(data = function() dm_input)
    )
  )
})

test_that("SDTM key setup - DM table as parent", {
  block <- new_cdisc_dm_block(dedup_cols = FALSE)

  dm_tbl <- data.frame(
    USUBJID = c("S1", "S2"),
    STUDYID = rep("STUDY1", 2),
    AGE = c(30, 40),
    stringsAsFactors = FALSE
  )
  ae <- data.frame(
    USUBJID = c("S1", "S1", "S2"),
    STUDYID = rep("STUDY1", 3),
    AETERM = c("Headache", "Nausea", "Fatigue"),
    stringsAsFactors = FALSE
  )

  dm_input <- dm::dm(dm = dm_tbl, ae = ae)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "dm")

      pks <- dm::dm_get_all_pks(result)
      expect_true("dm" %in% pks$table)

      fks <- dm::dm_get_all_fks(result)
      expect_true("ae" %in% fks$child_table)
      expect_equal(fks$parent_table[fks$child_table == "ae"], "dm")
    },
    args = list(
      x = block,
      data = list(data = function() dm_input)
    )
  )
})

test_that("Column deduplication removes shared columns from children", {
  block <- new_cdisc_dm_block(dedup_cols = TRUE)

  adsl <- data.frame(
    USUBJID = c("S1", "S2"),
    STUDYID = rep("STUDY1", 2),
    AGE = c(30, 40),
    SEX = c("M", "F"),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("S1", "S2"),
    STUDYID = rep("STUDY1", 2),
    AGE = c(30, 40),
    SEX = c("M", "F"),
    AEDECOD = c("Headache", "Nausea"),
    stringsAsFactors = FALSE
  )

  dm_input <- dm::dm(adsl = adsl, adae = adae)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "dm")

      # ADSL should still have all columns
      adsl_result <- dm::pull_tbl(result, adsl)
      expect_true("AGE" %in% names(adsl_result))
      expect_true("SEX" %in% names(adsl_result))

      # ADAE should have AGE and SEX removed
      adae_result <- dm::pull_tbl(result, adae)
      expect_false("AGE" %in% names(adae_result))
      expect_false("SEX" %in% names(adae_result))

      # USUBJID (the foreign key) and ADAE's own columns are kept. STUDYID is a
      # single constant value here (one study) so it is treated as a duplicated
      # subject-level column and removed too -- multi-study is not a case we
      # support, so STUDYID carries no information beyond the parent.
      expect_true("USUBJID" %in% names(adae_result))
      expect_false("STUDYID" %in% names(adae_result))
      expect_true("AEDECOD" %in% names(adae_result))
    },
    args = list(
      x = block,
      data = list(data = function() dm_input)
    )
  )
})

test_that("No parent table warns and passes through", {
  block <- new_cdisc_dm_block()

  tbl1 <- data.frame(
    USUBJID = c("S1", "S2"),
    VALUE = c(1, 2),
    stringsAsFactors = FALSE
  )
  tbl2 <- data.frame(
    USUBJID = c("S1", "S1"),
    RESULT = c("A", "B"),
    stringsAsFactors = FALSE
  )

  dm_input <- dm::dm(exposure = tbl1, labs = tbl2)

  # The pass-through warning fires when the block first evaluates. Under
  # blockr.core's eager evaluation that happens during testServer setup, before
  # this expression runs, so wrap the whole call instead of a later flushReact()
  # (which then has nothing left to emit).
  expect_warning(
    testServer(
      blockr.core:::get_s3_method("block_server", block),
      {
        session$flushReact()

        result <- session$returned$result()
        expect_s3_class(result, "dm")
        expect_equal(sort(names(dm::dm_get_tables(result))), c("exposure", "labs"))
      },
      args = list(
        x = block,
        data = list(data = function() dm_input)
      )
    ),
    "No CDISC parent table"
  )
})

test_that("Single parent table, no children", {
  block <- new_cdisc_dm_block(dedup_cols = FALSE)

  adsl <- data.frame(
    USUBJID = c("S1", "S2", "S3"),
    STUDYID = rep("STUDY1", 3),
    AGE = c(30, 40, 50),
    stringsAsFactors = FALSE
  )

  dm_input <- dm::dm(adsl = adsl)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "dm")

      pks <- dm::dm_get_all_pks(result)
      expect_true("adsl" %in% pks$table)

      # No FKs expected
      fks <- dm::dm_get_all_fks(result)
      expect_equal(nrow(fks), 0)
    },
    args = list(
      x = block,
      data = list(data = function() dm_input)
    )
  )
})

# Expression reference stability ----------------------------------------------
#
# blockr.core's block-server.R skips re-evaluating a block only when its
# expression and its data compare equal BY OBJECT IDENTITY (`same_ref()`).
# This block builds its expression with `bquote()`, which allocates a fresh
# call tree on every read, so any spurious upstream invalidation (a dock view
# switch re-fires the chain with byte-for-byte unchanged data) used to force a
# full re-evaluation of this block and everything downstream of it. See
# blockr.cdex/dev/profiling-plan.md, "Settled" item 8.

# A dm rebuilt from scratch: equal in value, different object.
cdisc_ref_dm <- function(extra_child = FALSE) {
  tbls <- list(
    adsl = data.frame(
      USUBJID = c("S1", "S2"), STUDYID = rep("STUDY1", 2),
      AGE = c(30, 40), stringsAsFactors = FALSE
    ),
    adae = data.frame(
      USUBJID = c("S1", "S2"), STUDYID = rep("STUDY1", 2),
      AEDECOD = c("Headache", "Nausea"), stringsAsFactors = FALSE
    )
  )
  if (extra_child) {
    tbls$adlb <- data.frame(
      USUBJID = c("S1", "S2"), STUDYID = rep("STUDY1", 2),
      PARAM = c("ALT", "AST"), stringsAsFactors = FALSE
    )
  }
  do.call(dm::dm, tbls)
}

test_that("expr keeps its object identity when the rebuilt expression is equal", {
  d1 <- cdisc_ref_dm()
  d2 <- cdisc_ref_dm()

  # Premise of the regression: two different objects carrying the same content.
  # (`identical()` on two separately built dm objects is FALSE -- dm stores
  # non-comparable internals -- but their tables, which is all this block reads,
  # are equal, so the expression it builds from them must be equal too.)
  expect_false(identical(rlang::obj_address(d1), rlang::obj_address(d2)))
  expect_equal(dm::dm_get_tables(d1), dm::dm_get_tables(d2))

  block <- new_cdisc_dm_block(dedup_cols = FALSE)

  # Drive invalidation with a tick so the data reactive re-fires while the
  # object it hands back is under our control (this is what board-wide churn
  # does: the chain re-runs, the upstream value has not changed).
  box <- new.env(parent = emptyenv())
  box$d <- d1
  tick <- shiny::reactiveVal(0L)
  data_fn <- shiny::reactive({
    tick()
    box$d
  })

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      e1 <- session$returned$expr()
      expect_true(is.call(e1))

      box$d <- d2
      tick(1L)
      session$flushReact()
      e2 <- session$returned$expr()

      # The whole point: a fresh but equal rebuild must NOT hand blockr.core a
      # new object, or `same_ref()` fails and the block re-evaluates.
      expect_identical(rlang::obj_address(e1), rlang::obj_address(e2))

      # ... and it is still the right expression, not a stale placeholder.
      code <- paste(deparse(e2), collapse = " ")
      expect_match(code, "dm::dm_add_pk(result, adsl, USUBJID)", fixed = TRUE)
      expect_match(
        code, "dm::dm_add_fk(result, adae, USUBJID, adsl)", fixed = TRUE
      )
    },
    args = list(x = block, data = list(data = data_fn))
  )
})

test_that("expr does produce a new, correct expression on a real change", {
  block <- new_cdisc_dm_block(dedup_cols = FALSE)

  box <- new.env(parent = emptyenv())
  box$d <- cdisc_ref_dm()
  tick <- shiny::reactiveVal(0L)
  data_fn <- shiny::reactive({
    tick()
    box$d
  })

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      e1 <- session$returned$expr()
      code1 <- paste(deparse(e1), collapse = " ")
      expect_false(grepl("adlb", code1, fixed = TRUE))

      # A genuinely new child table with USUBJID must add a foreign key.
      box$d <- cdisc_ref_dm(extra_child = TRUE)
      tick(1L)
      session$flushReact()
      e2 <- session$returned$expr()
      code2 <- paste(deparse(e2), collapse = " ")

      expect_false(identical(rlang::obj_address(e1), rlang::obj_address(e2)))
      expect_match(
        code2, "dm::dm_add_fk(result, adlb, USUBJID, adsl)", fixed = TRUE
      )
      expect_match(
        code2, "dm::dm_add_fk(result, adae, USUBJID, adsl)", fixed = TRUE
      )

      # And the new expression actually evaluates to the intended dm.
      out <- eval(e2, list(data = box$d))
      fks <- dm::dm_get_all_fks(out)
      expect_setequal(fks$child_table, c("adae", "adlb"))
    },
    args = list(x = block, data = list(data = data_fn))
  )
})

test_that("a state change (set_keys off) yields a new, correct expression", {
  block <- new_cdisc_dm_block(dedup_cols = FALSE)
  d <- cdisc_ref_dm()

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      e1 <- session$returned$expr()
      expect_match(
        paste(deparse(e1), collapse = " "),
        "dm::dm_add_pk(result, adsl, USUBJID)", fixed = TRUE
      )

      # The block's own inputs live under blockr.core's "expr" module scope.
      session$setInputs("expr-set_keys" = FALSE)
      session$flushReact()
      e2 <- session$returned$expr()

      expect_false(identical(rlang::obj_address(e1), rlang::obj_address(e2)))
      code2 <- paste(deparse(e2), collapse = " ")
      expect_false(grepl("dm_add_pk", code2, fixed = TRUE))
      expect_false(grepl("dm_add_fk", code2, fixed = TRUE))

      out <- eval(e2, list(data = d))
      expect_equal(nrow(dm::dm_get_all_pks(out)), 0L)
    },
    args = list(x = block, data = list(data = function() d))
  )
})

test_that("the shape decision lands within a single flush", {
  # The decision `expr` reads is derived in an observer keyed on `data()`,
  # which raises the question whether a consumer can see the PREVIOUS decision
  # for one extra flush. It cannot: Shiny's flush loop drains cascading
  # invalidations within one `flushReact()`, so one flush after a data change
  # is enough for both the expression AND blockr.core's evaluated result.
  block <- new_cdisc_dm_block(dedup_cols = FALSE)

  box <- new.env(parent = emptyenv())
  box$d <- cdisc_ref_dm()
  tick <- shiny::reactiveVal(0L)
  data_fn <- shiny::reactive({
    tick()
    box$d
  })

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_setequal(
        names(dm::dm_get_tables(session$returned$result())),
        c("adsl", "adae")
      )

      box$d <- cdisc_ref_dm(extra_child = TRUE)
      tick(1L)
      session$flushReact()          # ONE flush only

      e <- session$returned$expr()
      expect_match(
        paste(deparse(e), collapse = " "),
        "dm::dm_add_fk(result, adlb, USUBJID, adsl)", fixed = TRUE
      )
      res <- session$returned$result()
      expect_setequal(
        names(dm::dm_get_tables(res)), c("adsl", "adae", "adlb")
      )
      expect_setequal(dm::dm_get_all_fks(res)$child_table, c("adae", "adlb"))

      # A second flush must be a no-op -- if it were not, the first flush had
      # served a lagging decision.
      session$flushReact()
      expect_identical(
        rlang::obj_address(e), rlang::obj_address(session$returned$expr())
      )
    },
    args = list(x = block, data = list(data = data_fn))
  )
})

test_that("input going invalid after a good value stops, it does not go stale", {
  # The failure mode a naive observer implementation has: on invalid input it
  # silently skips the write, the reactiveVal keeps its last good decision, and
  # `expr` -- which no longer reads `data()` -- happily serves that STALE
  # expression instead of propagating `req()`'s silent stop (which blockr.core
  # reads as "this block is waiting").
  block <- new_cdisc_dm_block(dedup_cols = FALSE)

  box <- new.env(parent = emptyenv())
  box$d <- cdisc_ref_dm()
  tick <- shiny::reactiveVal(0L)
  data_fn <- shiny::reactive({
    tick()
    box$d
  })

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_match(
        paste(deparse(session$returned$expr()), collapse = " "),
        "dm::dm_add_pk(result, adsl, USUBJID)", fixed = TRUE
      )

      # Upstream is no longer a dm (the old `req(inherits(dm_input, "dm"))`).
      box$d <- data.frame(x = 1)
      tick(1L)
      session$flushReact()
      expect_error(session$returned$expr(), class = "shiny.silent.error")

      # Same for a NULL / not-yet-ready upstream.
      box$d <- NULL
      tick(2L)
      session$flushReact()
      expect_error(session$returned$expr(), class = "shiny.silent.error")

      # ... and it recovers when a real dm comes back.
      box$d <- cdisc_ref_dm()
      tick(3L)
      session$flushReact()
      expect_match(
        paste(deparse(session$returned$expr()), collapse = " "),
        "dm::dm_add_pk(result, adsl, USUBJID)", fixed = TRUE
      )
    },
    args = list(x = block, data = list(data = data_fn))
  )
})

test_that("an upstream that stops propagates the stop, it does not go stale", {
  # Same contract for an upstream that `req()`s rather than handing over a
  # wrong-shaped value: the silent stop is captured in the observer (an
  # erroring observer would kill the session) and re-raised from `expr`.
  block <- new_cdisc_dm_block(dedup_cols = FALSE)

  d <- cdisc_ref_dm()
  ok <- shiny::reactiveVal(TRUE)
  data_fn <- shiny::reactive({
    shiny::req(ok())
    d
  })

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_true(is.call(session$returned$expr()))

      ok(FALSE)
      session$flushReact()
      expect_error(session$returned$expr(), class = "shiny.silent.error")

      ok(TRUE)
      session$flushReact()
      expect_true(is.call(session$returned$expr()))
    },
    args = list(x = block, data = list(data = data_fn))
  )
})

test_that("cdisc_dm_shape is identical across equal-but-fresh inputs", {
  # The reactiveVal only absorbs a re-derivation as a no-op when the derived
  # value compares `identical()` -- which is why the shape is plain vectors and
  # lists rather than the `dm_get_all_*()` tibbles.
  expect_identical(
    cdisc_dm_shape(cdisc_ref_dm()), cdisc_dm_shape(cdisc_ref_dm())
  )
  expect_false(
    identical(
      cdisc_dm_shape(cdisc_ref_dm()),
      cdisc_dm_shape(cdisc_ref_dm(extra_child = TRUE))
    )
  )
  # No CDISC parent: the marker `expr` turns into the pass-through warning.
  no_parent <- dm::dm(
    exposure = data.frame(USUBJID = "S1"), labs = data.frame(USUBJID = "S1")
  )
  expect_null(cdisc_dm_shape(no_parent)$parent)
})
