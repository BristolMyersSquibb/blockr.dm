test_that("dm example block constructor creates correct class hierarchy", {
  block <- new_dm_example_block()
  expect_s3_class(
    block,
    c("dm_example_block", "dm_block", "data_block", "block")
  )
})

test_that("dm_example_choices always includes bi_star_schema", {
  choices <- dm_example_choices()
  expect_true("bi_star_schema" %in% choices)
})

test_that("bi_star_schema_expr produces valid dm", {
  result <- eval(bi_star_schema_expr())

  expect_s3_class(result, "dm")

  tables <- sort(names(dm::dm_get_tables(result)))
  expect_equal(tables, c("categories", "customers", "orders", "products"))

  expect_equal(nrow(result$categories), 5L)
  expect_equal(nrow(result$products), 12L)
  expect_equal(nrow(result$customers), 8L)
  expect_equal(nrow(result$orders), 45L)

  pks <- dm::dm_get_all_pks(result)
  expect_equal(sort(pks$table), c("categories", "customers", "orders", "products"))

  fks <- dm::dm_get_all_fks(result)
  expect_true("products" %in% fks$child_table)
  expect_true("orders" %in% fks$child_table)
})

test_that("bi_star_schema_expr is deterministic", {
  r1 <- eval(bi_star_schema_expr())
  r2 <- eval(bi_star_schema_expr())
  expect_identical(r1$orders, r2$orders)
})

test_that("dm example block returns dm with default (bi_star_schema)", {
  block <- new_dm_example_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "dm")
      tables <- sort(names(dm::dm_get_tables(result)))
      expect_equal(tables, c("categories", "customers", "orders", "products"))
    },
    args = list(x = block)
  )
})

test_that("nycflights13_expr produces valid dm", {
  skip_if_not_installed("nycflights13")
  result <- eval(nycflights13_expr())

  expect_s3_class(result, "dm")
  expect_true("flights" %in% names(dm::dm_get_tables(result)))
})

test_that("dm_example_expr errors on unknown id", {
  expect_error(dm_example_expr("nonexistent"), "Unknown dm example")
})

test_that("blocks that moved to blockr.pharma still construct from here", {
  skip_if_not_installed("blockr.pharma")
  pharma <- asNamespace("blockr.pharma")
  skip_if_not(exists("new_cdisc_example_block", pharma) &&
                exists("new_cdisc_dm_block", pharma))

  blk <- new_dm_example_block(dataset = "pharmaverseadam")
  expect_s3_class(blk, "cdisc_example_block")
  expect_identical(blockr.core::ctor_pkg(attr(blk, "ctor")),
                   "blockr.pharma")

  blk <- new_cdisc_dm_block(dedup_cols = TRUE)
  expect_s3_class(blk, "cdisc_dm_block")
  expect_identical(blockr.core::ctor_pkg(attr(blk, "ctor")),
                   "blockr.pharma")
})
