test_that("dm_filter block constructor", {
 block <- new_dm_filter_block()
 expect_s3_class(
   block,
   c("dm_filter_block", "transform_block", "block")
 )

 # Test with initial values
 block2 <- new_dm_filter_block(
   table = "adlb",
   expr = "AVAL > 5"
 )
 expect_s3_class(block2, "dm_filter_block")
})

test_that("dm_filter block filters dm by expression", {
 block <- new_dm_filter_block(
   table = "events",
   expr = "value > 10"
 )

 # Create test dm with keys
 subjects <- data.frame(id = 1:5, name = c("a", "b", "c", "d", "e"))
 events <- data.frame(
   subject_id = c(1, 1, 2, 3, 4, 5),
   value = c(5, 15, 20, 8, 12, 3)
 )
 test_dm <- dm::dm(subjects = subjects, events = events) |>
   dm::dm_add_pk(subjects, id) |>
   dm::dm_add_fk(events, subject_id, subjects)

 testServer(
   blockr.core:::get_s3_method("block_server", block),
   {
     session$flushReact()
     result <- session$returned$result()

     expect_s3_class(result, "dm")

     # Events table should be filtered to value > 10
     events_filtered <- dm::pull_tbl(result, events)
     expect_true(all(events_filtered$value > 10))
     expect_equal(nrow(events_filtered), 3) # values 15, 20, 12

     # Subjects table should be filtered to only those with matching events
     subjects_filtered <- dm::pull_tbl(result, subjects)
     # Subjects 1, 2, and 4 have events with value > 10
     expect_true(all(subjects_filtered$id %in% c(1, 2, 4)))
   },
   args = list(
     x = block,
     data = list(data = function() test_dm)
   )
 )
})

test_that("dm_filter block with TRUE expression returns dm unchanged", {
 # When expression is TRUE, should return data unchanged
 block <- new_dm_filter_block(
   table = "events",
   expr = "TRUE"
 )

 subjects <- data.frame(id = 1:3, name = c("a", "b", "c"))
 events <- data.frame(subject_id = c(1, 1, 2), value = c(5, 15, 20))
 test_dm <- dm::dm(subjects = subjects, events = events)

 testServer(
   blockr.core:::get_s3_method("block_server", block),
   {
     session$flushReact()
     result <- session$returned$result()

     expect_s3_class(result, "dm")

     # Should return unchanged dm (TRUE filters nothing out)
     events_result <- dm::pull_tbl(result, events)
     expect_equal(nrow(events_result), 3)
   },
   args = list(
     x = block,
     data = list(data = function() test_dm)
   )
 )
})

test_that("dm_filter block filters with character conditions", {
 block <- new_dm_filter_block(
   table = "events",
   expr = "type == 'A'"
 )

 subjects <- data.frame(id = 1:3, name = c("a", "b", "c"))
 events <- data.frame(
   subject_id = c(1, 1, 2, 3),
   type = c("A", "B", "A", "B")
 )
 test_dm <- dm::dm(subjects = subjects, events = events) |>
   dm::dm_add_pk(subjects, id) |>
   dm::dm_add_fk(events, subject_id, subjects)

 testServer(
   blockr.core:::get_s3_method("block_server", block),
   {
     session$flushReact()
     result <- session$returned$result()

     events_filtered <- dm::pull_tbl(result, events)
     expect_true(all(events_filtered$type == "A"))
     expect_equal(nrow(events_filtered), 2)
   },
   args = list(
     x = block,
     data = list(data = function() test_dm)
   )
 )
})

test_that("dm_filter block state includes table and expr", {
 block <- new_dm_filter_block(
   table = "adlb",
   expr = "PARAMCD == 'NEUT' & AVAL > 5"
 )

 subjects <- data.frame(USUBJID = paste0("SUBJ-", 1:3))
 labs <- data.frame(
   USUBJID = rep(paste0("SUBJ-", 1:3), each = 2),
   PARAMCD = rep(c("NEUT", "WBC"), 3),
   AVAL = c(4.5, 8.2, 6.1, 7.5, 3.2, 6.8)
 )
 test_dm <- dm::dm(adsl = subjects, adlb = labs)

 testServer(
   blockr.core:::get_s3_method("block_server", block),
   {
     session$flushReact()

     state <- session$returned$state
     expect_true("table" %in% names(state))
     expect_true("expr" %in% names(state))
     expect_equal(state$table(), "adlb")
     expect_equal(state$expr(), "PARAMCD == 'NEUT' & AVAL > 5")
   },
   args = list(
     x = block,
     data = list(data = function() test_dm)
   )
 )
})

test_that("dm_filter block filters ADaM-style data correctly", {
 # This tests the main use case: filter subjects by lab value, cascade to AEs

 block <- new_dm_filter_block(
   table = "adlb",
   expr = "PARAMCD == 'NEUT' & AVAL > 5"
 )

 # ADaM-style test data
 adsl <- data.frame(
   USUBJID = paste0("SUBJ-", 1:5),
   AGE = c(45, 52, 38, 61, 29)
 )
 adlb <- data.frame(
   USUBJID = rep(paste0("SUBJ-", 1:5), each = 2),
   PARAMCD = rep(c("NEUT", "WBC"), 5),
   AVAL = c(4.5, 8.2, 6.1, 7.5, 3.2, 6.8, 5.5, 9.1, 4.8, 7.2)
 )
 adae <- data.frame(
   USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-2", "SUBJ-4", "SUBJ-4", "SUBJ-4"),
   AETERM = c("Headache", "Nausea", "Fatigue", "Dizziness", "Headache", "Rash")
 )

 # Create dm with keys
 test_dm <- dm::dm(adsl = adsl, adlb = adlb, adae = adae) |>
   dm::dm_add_pk(adsl, USUBJID) |>
   dm::dm_add_fk(adlb, USUBJID, adsl) |>
   dm::dm_add_fk(adae, USUBJID, adsl)

 testServer(
   blockr.core:::get_s3_method("block_server", block),
   {
     session$flushReact()
     result <- session$returned$result()

     # NEUT values > 5: SUBJ-2 (6.1), SUBJ-4 (5.5)
     # Wait, 5.5 > 5, so SUBJ-4 is included
     # And 6.1 > 5 for SUBJ-2

     # Check filtered AEs - should only have AEs for subjects 2 and 4
     ae_filtered <- dm::pull_tbl(result, adae)
     expect_true(all(ae_filtered$USUBJID %in% c("SUBJ-2", "SUBJ-4")))

     # SUBJ-2 has 1 AE, SUBJ-4 has 3 AEs = 4 total
     expect_equal(nrow(ae_filtered), 4)
   },
   args = list(
     x = block,
     data = list(data = function() test_dm)
   )
 )
})
