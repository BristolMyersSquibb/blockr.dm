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
