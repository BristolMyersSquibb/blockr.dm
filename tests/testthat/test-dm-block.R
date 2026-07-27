test_that("dm block constructor", {
 block <- new_dm_block()
 expect_s3_class(
   block,
   c("dm_block", "transform_block", "block")
 )
})

test_that("dm block creates dm object from multiple dataframes", {
 block <- new_dm_block()

 # Create test data
 df1 <- data.frame(id = 1:3, name = c("a", "b", "c"))
 df2 <- data.frame(parent_id = c(1, 1, 2), value = 10:12)

 testServer(
   blockr.core:::get_s3_method("block_server", block),
   {
     session$flushReact()
     result <- session$returned$result()

     # Should return a dm object
     expect_s3_class(result, "dm")

     # Should have two tables
     tables <- names(dm::dm_get_tables(result))
     expect_equal(length(tables), 2)
     expect_true("table_1" %in% tables)
     expect_true("table_2" %in% tables)

     # Tables should contain the data
     t1 <- dm::pull_tbl(result, table_1)
     expect_equal(nrow(t1), 3)
     expect_equal(t1$name, c("a", "b", "c"))
   },
   args = list(
     x = block,
     data = list(
       ...args = shiny::reactiveValues(
         `1` = df1,
         `2` = df2
       )
     )
   )
 )
})

test_that("dm block with named inputs preserves table names", {
 block <- new_dm_block()

 df1 <- data.frame(id = 1:3, name = c("a", "b", "c"))
 df2 <- data.frame(parent_id = c(1, 1, 2), value = 10:12)

 testServer(
   blockr.core:::get_s3_method("block_server", block),
   {
     session$flushReact()
     result <- session$returned$result()

     # Named inputs should become table names
     tables <- names(dm::dm_get_tables(result))
     expect_true("adsl" %in% tables)
     expect_true("adae" %in% tables)
   },
   args = list(
     x = block,
     data = list(
       ...args = shiny::reactiveValues(
         adsl = df1,
         adae = df2
       )
     )
   )
 )
})

test_that("dm block handles single input", {
 block <- new_dm_block()

 df1 <- data.frame(id = 1:3, name = c("a", "b", "c"))

 testServer(
   blockr.core:::get_s3_method("block_server", block),
   {
     session$flushReact()
     result <- session$returned$result()

     expect_s3_class(result, "dm")
     tables <- names(dm::dm_get_tables(result))
     expect_equal(length(tables), 1)
   },
   args = list(
     x = block,
     data = list(
       ...args = shiny::reactiveValues(
         `1` = df1
       )
     )
   )
 )
})

test_that("dm block handles three or more inputs", {
 block <- new_dm_block()

 df1 <- data.frame(id = 1:3, name = c("a", "b", "c"))
 df2 <- data.frame(parent_id = c(1, 1, 2), value = 10:12)
 df3 <- data.frame(other_id = c(1, 2, 3), data = c("x", "y", "z"))

 testServer(
   blockr.core:::get_s3_method("block_server", block),
   {
     session$flushReact()
     result <- session$returned$result()

     expect_s3_class(result, "dm")
     tables <- names(dm::dm_get_tables(result))
     expect_equal(length(tables), 3)
   },
   args = list(
     x = block,
     data = list(
       ...args = shiny::reactiveValues(
         `1` = df1,
         `2` = df2,
         `3` = df3
       )
     )
   )
 )
})

test_that("dm block keeps an unnamed (DAG-UI) input instead of dropping it", {
  # Regression: connecting a dataframe to the dm block by dragging an edge in
  # the DAG UI adds an *unnamed* link, which a live board stores as a positional
  # slot in the `...args` reactives object. `names()` is then NULL, which used
  # to collapse the classification loop to zero iterations -> an empty dm. Build
  # the positional `reactives` object the live board produces (reactiveValues()
  # can only hold named slots).
  block <- new_dm_block()
  df1 <- data.frame(id = 1:3, name = c("a", "b", "c"))

  args_obj <- shiny::isolate({
    ra <- blockr.core:::reactives()
    blockr.core:::append_reactive(ra, function() df1)
    ra
  })

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "dm")

      # The unnamed input is present as "table_1" (the positional display
      # fallback), carrying its data — not dropped into an empty dm.
      tables <- names(dm::dm_get_tables(result))
      expect_true("table_1" %in% tables)
      expect_equal(nrow(dm::pull_tbl(result, table_1)), 3)
    },
    args = list(x = block, data = list(...args = args_obj))
  )
})

# Expression reference stability ----------------------------------------------
#
# blockr.core's block-server.R skips re-evaluating a block only when its
# expression and its data compare equal BY OBJECT IDENTITY (`same_ref()`).
# The old `input_info()` reactive realized every variadic input on each read
# and rebuilt the expression with `bquote()` (a fresh call tree every run), so
# any spurious upstream invalidation re-evaluated this block and everything
# downstream. Same regression contract as test-cdisc-dm-block.R.

dm_ref_adsl <- function() {
  data.frame(
    USUBJID = c("S1", "S2", "S3"), AGE = c(45, 52, 38),
    stringsAsFactors = FALSE
  )
}

dm_ref_adae <- function(shared_key = FALSE) {
  df <- data.frame(
    AETERM = c("Headache", "Nausea", "Fatigue"),
    stringsAsFactors = FALSE
  )
  if (shared_key) df$USUBJID <- c("S1", "S1", "S2")
  df
}

test_that("expr keeps its object identity on equal-but-fresh inputs", {
  block <- new_dm_block()

  box <- new.env(parent = emptyenv())
  box$adsl <- dm_ref_adsl()
  box$adae <- dm_ref_adae(shared_key = TRUE)
  tick <- shiny::reactiveVal(0L)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      e1 <- session$returned$expr()
      code1 <- paste(deparse(e1), collapse = " ")
      # Keys inferred: USUBJID unique in adsl, duplicated in adae.
      expect_match(code1, "dm_add_pk", fixed = TRUE)
      expect_match(code1, "dm_add_fk", fixed = TRUE)

      # Fresh-but-equal rebuild: board-wide churn re-fires the input chain
      # with byte-for-byte identical data in new objects.
      box$adsl <- dm_ref_adsl()
      box$adae <- dm_ref_adae(shared_key = TRUE)
      tick(1L)
      session$flushReact()
      e2 <- session$returned$expr()

      expect_identical(rlang::obj_address(e1), rlang::obj_address(e2))
    },
    args = list(
      x = block,
      data = list(
        ...args = shiny::reactiveValues(
          adsl = shiny::reactive({
            tick()
            box$adsl
          }),
          adae = shiny::reactive({
            tick()
            box$adae
          })
        )
      )
    )
  )
})

test_that("a real input change yields a new, correct expression", {
  block <- new_dm_block()

  box <- new.env(parent = emptyenv())
  box$adae <- dm_ref_adae()
  tick <- shiny::reactiveVal(0L)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      e1 <- session$returned$expr()
      # No shared column: nothing to infer.
      expect_false(grepl("dm_add_pk", paste(deparse(e1), collapse = " "),
                         fixed = TRUE))

      # The USUBJID column appears in adae: PK/FK become inferable.
      box$adae <- dm_ref_adae(shared_key = TRUE)
      tick(1L)
      session$flushReact()
      e2 <- session$returned$expr()

      expect_false(identical(rlang::obj_address(e1), rlang::obj_address(e2)))
      code2 <- paste(deparse(e2), collapse = " ")
      expect_match(code2, "dm_add_pk", fixed = TRUE)
      expect_match(code2, "dm_add_fk", fixed = TRUE)

      # ... and the new expression evaluates to the intended keyed dm.
      # (suppressWarnings: the block's generated code has always used the
      # dm-1.0.0-deprecated dm_bind(); evaluating outside blockr.core's
      # condition capture surfaces that pre-existing deprecation here.)
      out <- suppressWarnings(
        eval(e2, list(adsl = dm_ref_adsl(),
                      adae = dm_ref_adae(shared_key = TRUE)))
      )
      expect_s3_class(out, "dm")
      expect_equal(dm::dm_get_all_pks(out)$table, "adsl")
      expect_equal(dm::dm_get_all_fks(out)$child_table, "adae")
    },
    args = list(
      x = block,
      data = list(
        ...args = shiny::reactiveValues(
          adsl = shiny::reactive({
            tick()
            dm_ref_adsl()
          }),
          adae = shiny::reactive({
            tick()
            box$adae
          })
        )
      )
    )
  )
})

test_that("toggling infer_keys yields a new, correct expression", {
  block <- new_dm_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      e1 <- session$returned$expr()
      expect_match(paste(deparse(e1), collapse = " "), "dm_add_pk",
                   fixed = TRUE)

      # The block's own inputs live under blockr.core's "expr" module scope.
      session$setInputs("expr-infer_keys" = FALSE)
      session$flushReact()
      e2 <- session$returned$expr()

      expect_false(identical(rlang::obj_address(e1), rlang::obj_address(e2)))
      expect_false(grepl("dm_add_pk", paste(deparse(e2), collapse = " "),
                         fixed = TRUE))
    },
    args = list(
      x = block,
      data = list(
        ...args = shiny::reactiveValues(
          adsl = dm_ref_adsl(),
          adae = dm_ref_adae(shared_key = TRUE)
        )
      )
    )
  )
})

test_that("an upstream that stops propagates the stop, it does not go stale", {
  # The failure mode a naive observer implementation has: on an invalid read
  # it silently skips the write, the reactiveVal keeps its last good shape,
  # and `expr` -- which no longer reads the inputs -- serves that STALE
  # expression instead of propagating the upstream's silent stop (which
  # blockr.core reads as "this block is waiting").
  block <- new_dm_block()

  ok <- shiny::reactiveVal(TRUE)

  testServer(
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
    args = list(
      x = block,
      data = list(
        ...args = shiny::reactiveValues(
          adsl = shiny::reactive({
            shiny::req(ok())
            dm_ref_adsl()
          })
        )
      )
    )
  )
})

test_that("adding a DAG-UI slot at runtime re-derives the expression", {
  # Slot-set changes go through the reactives container's keys reactiveVal,
  # which the shape observer depends on via dot_arg_refs()/dot_arg_values().
  block <- new_dm_block()
  df1 <- dm_ref_adsl()
  df2 <- dm_ref_adae(shared_key = TRUE)

  args_obj <- shiny::isolate({
    ra <- blockr.core:::reactives()
    blockr.core:::append_reactive(ra, function() df1)
    ra
  })

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      e1 <- session$returned$expr()
      expect_match(paste(deparse(e1), collapse = " "), "table_1",
                   fixed = TRUE)
      expect_false(grepl("table_2", paste(deparse(e1), collapse = " "),
                         fixed = TRUE))

      shiny::isolate(blockr.core:::append_reactive(args_obj, function() df2))
      session$flushReact()
      e2 <- session$returned$expr()

      expect_false(identical(rlang::obj_address(e1), rlang::obj_address(e2)))
      expect_match(paste(deparse(e2), collapse = " "), "table_2",
                   fixed = TRUE)
    },
    args = list(x = block, data = list(...args = args_obj))
  )
})

test_that("dm_block_shape is identical across equal-but-fresh inputs", {
  # The reactiveVal only absorbs a re-derivation as a no-op when the derived
  # value compares `identical()` -- which is why the shape is plain vectors
  # and lists, never the input data itself.
  refs <- stats::setNames(c("adsl", "adae"), c("adsl", "adae"))
  shape1 <- dm_block_shape(
    refs, list(dm_ref_adsl(), dm_ref_adae(shared_key = TRUE)), TRUE
  )
  shape2 <- dm_block_shape(
    refs, list(dm_ref_adsl(), dm_ref_adae(shared_key = TRUE)), TRUE
  )
  expect_identical(shape1, shape2)

  # The data itself is not retained in the shape.
  expect_named(shape1, c("nms", "display_nms", "is_dm", "pks", "fks"))

  # A real difference changes the shape.
  shape3 <- dm_block_shape(
    refs, list(dm_ref_adsl(), dm_ref_adae()), TRUE
  )
  expect_false(identical(shape1, shape3))

  # infer_keys off drops the key relations.
  shape4 <- dm_block_shape(
    refs, list(dm_ref_adsl(), dm_ref_adae(shared_key = TRUE)), FALSE
  )
  expect_length(shape4$pks, 0L)
  expect_length(shape4$fks, 0L)
})
