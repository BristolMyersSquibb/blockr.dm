test_that("dm_pluck block constructor", {
 block <- new_dm_pluck_block()
 expect_s3_class(
   block,
   c("dm_pluck_block", "transform_block", "block")
 )

 # Test with initial values
 block2 <- new_dm_pluck_block(table = "adae")
 expect_s3_class(block2, "dm_pluck_block")
})

test_that("dm_pluck block extracts table from dm", {
 block <- new_dm_pluck_block(table = "events")

 subjects <- data.frame(id = 1:3, name = c("a", "b", "c"))
 events <- data.frame(subject_id = c(1, 1, 2), value = c(5, 15, 20))
 test_dm <- dm::dm(subjects = subjects, events = events)

 testServer(
   blockr.core:::get_s3_method("block_server", block),
   {
     session$flushReact()
     result <- session$returned$result()

     # Should return a data frame, not a dm
     expect_s3_class(result, "data.frame")
     expect_false(inherits(result, "dm"))

     # Should match the events table
     expect_equal(nrow(result), 3)
     expect_equal(result$value, c(5, 15, 20))
   },
   args = list(
     x = block,
     data = list(data = function() test_dm)
   )
 )
})

test_that("dm_pluck block extracts different tables", {
 # Test extracting subjects table
 block_subjects <- new_dm_pluck_block(table = "subjects")

 subjects <- data.frame(id = 1:3, name = c("a", "b", "c"))
 events <- data.frame(subject_id = c(1, 1, 2), value = c(5, 15, 20))
 test_dm <- dm::dm(subjects = subjects, events = events)

 testServer(
   blockr.core:::get_s3_method("block_server", block_subjects),
   {
     session$flushReact()
     result <- session$returned$result()

     expect_s3_class(result, "data.frame")
     expect_equal(nrow(result), 3)
     expect_equal(result$name, c("a", "b", "c"))
   },
   args = list(
     x = block_subjects,
     data = list(data = function() test_dm)
   )
 )
})

test_that("dm_pluck block state includes table parameter", {
 block <- new_dm_pluck_block(table = "adae")

 subjects <- data.frame(USUBJID = paste0("SUBJ-", 1:3))
 events <- data.frame(USUBJID = c("SUBJ-1", "SUBJ-2"), AETERM = c("x", "y"))
 test_dm <- dm::dm(adsl = subjects, adae = events)

 testServer(
   blockr.core:::get_s3_method("block_server", block),
   {
     session$flushReact()

     state <- session$returned$state
     expect_true("table" %in% names(state))
     expect_equal(state$table(), "adae")
   },
   args = list(
     x = block,
     data = list(data = function() test_dm)
   )
 )
})

test_that("dm_pluck block works after dm_filter", {
 # This is the key use case - pluck after filtering

 # Create filtered dm
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

 test_dm <- dm::dm(adsl = adsl, adlb = adlb, adae = adae) |>
   dm::dm_add_pk(adsl, USUBJID) |>
   dm::dm_add_fk(adlb, USUBJID, adsl) |>
   dm::dm_add_fk(adae, USUBJID, adsl)

 # Filter to subjects with high neutrophils
 filtered_dm <- test_dm |>
   dm::dm_filter(adlb = (PARAMCD == "NEUT" & AVAL > 5))

 # Now test pluck block
 block <- new_dm_pluck_block(table = "adae")

 testServer(
   blockr.core:::get_s3_method("block_server", block),
   {
     session$flushReact()
     result <- session$returned$result()

     expect_s3_class(result, "data.frame")

     # Should only have AEs for subjects 2 and 4
     expect_true(all(result$USUBJID %in% c("SUBJ-2", "SUBJ-4")))
     expect_equal(nrow(result), 4)
   },
   args = list(
     x = block,
     data = list(data = function() filtered_dm)
   )
 )
})
