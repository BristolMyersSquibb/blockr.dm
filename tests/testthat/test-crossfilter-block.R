# Tests for crossfilter_block (single-table wrapper)

# ==============================================================================
# Constructor
# ==============================================================================

test_that("crossfilter_block constructor", {
  block <- new_crossfilter_block()
  expect_s3_class(
    block,
    c("crossfilter_block", "transform_block", "block")
  )
})

# ==============================================================================
# Basic testServer
# ==============================================================================

test_that("crossfilter_block no filters returns all rows", {
  block <- new_crossfilter_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 150)
    },
    args = list(x = block, data = list(data = function() iris))
  )
})

test_that("crossfilter_block with initial categorical filter", {
  block <- new_crossfilter_block(
    filters = list(Species = "setosa")
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 50)
      expect_true(all(result$Species == "setosa"))
    },
    args = list(x = block, data = list(data = function() iris))
  )
})

test_that("crossfilter_block with initial range filter", {
  block <- new_crossfilter_block(
    range_filters = list(Sepal.Length = c(5, 6))
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_true(all(result$Sepal.Length >= 5))
      expect_true(all(result$Sepal.Length <= 6))
    },
    args = list(x = block, data = list(data = function() iris))
  )
})

# ==============================================================================
# external_ctrl: state is writable reactiveVals
# ==============================================================================

test_that("crossfilter_block state vars are reactiveVals (external_ctrl)", {
  block <- new_crossfilter_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state
      expect_true(inherits(state$filters, "reactiveVal"))
      expect_true(inherits(state$range_filters, "reactiveVal"))
      expect_true(inherits(state$active_dims, "reactiveVal"))
    },
    args = list(x = block, data = list(data = function() iris))
  )
})

test_that("crossfilter_block writing filters reactiveVal updates result", {
  block <- new_crossfilter_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Initially: all 150 rows
      result <- session$returned$result()
      expect_equal(nrow(result), 150)

      # Simulate AI setting categorical filter
      state <- session$returned$state
      state$filters(list(Species = "setosa"))
      session$flushReact()

      result2 <- session$returned$result()
      expect_equal(nrow(result2), 50)
      expect_true(all(result2$Species == "setosa"))
    },
    args = list(x = block, data = list(data = function() iris))
  )
})

test_that("crossfilter_block writing range_filters reactiveVal updates result", {
  block <- new_crossfilter_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Simulate AI setting range filter
      state <- session$returned$state
      state$range_filters(list(Sepal.Length = c(5, 6)))
      session$flushReact()

      result <- session$returned$result()
      expect_true(all(result$Sepal.Length >= 5))
      expect_true(all(result$Sepal.Length <= 6))
    },
    args = list(x = block, data = list(data = function() iris))
  )
})

test_that("crossfilter_block state reflects written values (flat format)", {
  block <- new_crossfilter_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      state <- session$returned$state

      # Write filters
      state$filters(list(Species = c("setosa", "virginica")))
      session$flushReact()

      # Read back: should be flat format (not per-table)
      f <- state$filters()
      expect_type(f, "list")
      expect_equal(f$Species, c("setosa", "virginica"))
    },
    args = list(x = block, data = list(data = function() iris))
  )
})
