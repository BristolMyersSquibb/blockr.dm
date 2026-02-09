test_that("dm_add_keys block constructor", {
 block <- new_dm_add_keys_block()
 expect_s3_class(
   block,
   c("dm_add_keys_block", "transform_block", "block")
 )

 # Test with initial values
 block2 <- new_dm_add_keys_block(
   pk_table = "adsl",
   pk_column = "USUBJID",
   fk_tables = "adae",
   fk_column = "USUBJID"
 )
 expect_s3_class(block2, "dm_add_keys_block")
})

test_that("dm_add_keys block adds primary key only", {
 block <- new_dm_add_keys_block(
   pk_table = "subjects",
   pk_column = "id"
 )

 # Create test dm
 subjects <- data.frame(id = 1:3, name = c("a", "b", "c"))
 events <- data.frame(subject_id = c(1, 1, 2), event = c("x", "y", "z"))
 test_dm <- dm::dm(subjects = subjects, events = events)

 testServer(
   blockr.core:::get_s3_method("block_server", block),
   {
     session$flushReact()
     result <- session$returned$result()

     # Should return a dm object
     expect_s3_class(result, "dm")

     # Should have primary key on subjects table
     pks <- dm::dm_get_all_pks(result)
     expect_true(nrow(pks) >= 1)
     expect_true("subjects" %in% pks$table)
   },
   args = list(
     x = block,
     data = list(data = function() test_dm)
   )
 )
})

test_that("dm_add_keys block adds primary and foreign key", {
 block <- new_dm_add_keys_block(
   pk_table = "subjects",
   pk_column = "id",
   fk_tables = "events",
   fk_column = "subject_id"
 )

 # Create test dm
 subjects <- data.frame(id = 1:3, name = c("a", "b", "c"))
 events <- data.frame(subject_id = c(1, 1, 2), event = c("x", "y", "z"))
 test_dm <- dm::dm(subjects = subjects, events = events)

 testServer(
   blockr.core:::get_s3_method("block_server", block),
   {
     session$flushReact()
     result <- session$returned$result()

     expect_s3_class(result, "dm")

     # Should have primary key
     pks <- dm::dm_get_all_pks(result)
     expect_true("subjects" %in% pks$table)

     # Should have foreign key
     fks <- dm::dm_get_all_fks(result)
     expect_true(nrow(fks) >= 1)
     expect_true("events" %in% fks$child_table)
   },
   args = list(
     x = block,
     data = list(data = function() test_dm)
   )
 )
})

test_that("dm_add_keys block state includes all parameters", {
 block <- new_dm_add_keys_block(
   pk_table = "adsl",
   pk_column = "USUBJID",
   fk_tables = "adae",
   fk_column = "USUBJID"
 )

 subjects <- data.frame(USUBJID = paste0("SUBJ-", 1:3), AGE = c(45, 52, 38))
 events <- data.frame(USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-2"), AETERM = c("x", "y", "z"))
 test_dm <- dm::dm(adsl = subjects, adae = events)

 testServer(
   blockr.core:::get_s3_method("block_server", block),
   {
     session$flushReact()

     # Check state contains all parameters
     state <- session$returned$state
     expect_true("pk_table" %in% names(state))
     expect_true("pk_column" %in% names(state))
     expect_true("fk_tables" %in% names(state))
     expect_true("fk_column" %in% names(state))

     # Values should match constructor
     expect_equal(state$pk_table(), "adsl")
     expect_equal(state$pk_column(), "USUBJID")
   },
   args = list(
     x = block,
     data = list(data = function() test_dm)
   )
 )
})
