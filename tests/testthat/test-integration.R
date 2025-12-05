# Integration tests for full dm workflows

test_that("full ADaM workflow works end-to-end", {
  # This tests the complete workflow from the adam_workflow.R example

  # Test data
  adsl <- data.frame(
    USUBJID = paste0("SUBJ-", 1:5),
    AGE = c(45, 52, 38, 61, 29),
    stringsAsFactors = FALSE
  )

  adlb <- data.frame(
    USUBJID = rep(paste0("SUBJ-", 1:5), each = 2),
    PARAMCD = rep(c("NEUT", "WBC"), 5),
    AVAL = c(4.5, 8.2, 6.1, 7.5, 3.2, 6.8, 5.5, 9.1, 4.8, 7.2),
    stringsAsFactors = FALSE
  )

  adae <- data.frame(
    USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-2", "SUBJ-4", "SUBJ-4", "SUBJ-4"),
    AETERM = c("Headache", "Nausea", "Fatigue", "Dizziness", "Headache", "Rash"),
    stringsAsFactors = FALSE
  )

  # Create the full board
  board <- blockr.core::new_board(
    blocks = list(
      adsl_data = blockr.core::new_static_block(data = adsl),
      adlb_data = blockr.core::new_static_block(data = adlb),
      adae_data = blockr.core::new_static_block(data = adae),
      dm_obj = new_dm_block(),
      dm_keys1 = new_dm_add_keys_block(
        pk_table = "adsl_data",
        pk_column = "USUBJID",
        fk_table = "adlb_data",
        fk_column = "USUBJID"
      ),
      dm_keys2 = new_dm_add_keys_block(
        pk_table = "adsl_data",
        pk_column = "USUBJID",
        fk_table = "adae_data",
        fk_column = "USUBJID"
      ),
      filtered_dm = new_dm_filter_block(
        table = "adlb_data",
        expr = "PARAMCD == 'NEUT' & AVAL > 5"
      ),
      ae_results = new_dm_pluck_block(table = "adae_data")
    ),
    links = c(
      # Use named inputs that match table names
      blockr.core::new_link("adsl_data", "dm_obj", "adsl_data"),
      blockr.core::new_link("adlb_data", "dm_obj", "adlb_data"),
      blockr.core::new_link("adae_data", "dm_obj", "adae_data"),
      blockr.core::new_link("dm_obj", "dm_keys1", "data"),
      blockr.core::new_link("dm_keys1", "dm_keys2", "data"),
      blockr.core::new_link("dm_keys2", "filtered_dm", "data"),
      blockr.core::new_link("filtered_dm", "ae_results", "data")
    )
  )

  testServer(
    blockr.core::board_server,
    {
      # Flush multiple times to let the pipeline complete
      for (i in 1:5) {
        session$flushReact()
        Sys.sleep(0.05)
      }

      # Check dm_obj created correctly
      dm_result <- rv$blocks$dm_obj$server$result()
      expect_s3_class(dm_result, "dm")
      expect_equal(
        sort(names(dm::dm_get_tables(dm_result))),
        c("adae_data", "adlb_data", "adsl_data")
      )

      # Check keys were added correctly
      keys_result <- rv$blocks$dm_keys2$server$result()
      expect_s3_class(keys_result, "dm")

      pks <- dm::dm_get_all_pks(keys_result)
      expect_equal(pks$table, "adsl_data")

      fks <- dm::dm_get_all_fks(keys_result)
      expect_equal(sort(fks$child_table), c("adae_data", "adlb_data"))

      # Check filter worked
      filter_result <- rv$blocks$filtered_dm$server$result()
      expect_s3_class(filter_result, "dm")

      # NEUT > 5: SUBJ-2 (6.1), SUBJ-4 (5.5) - both qualify
      adsl_filtered <- dm::dm_get_tables(filter_result)$adsl_data
      expect_equal(sort(adsl_filtered$USUBJID), c("SUBJ-2", "SUBJ-4"))

      # Check final result
      ae_result <- rv$blocks$ae_results$server$result()
      expect_s3_class(ae_result, "data.frame")

      # Should have AEs for SUBJ-2 (1 AE) and SUBJ-4 (3 AEs)
      expect_equal(nrow(ae_result), 4)
      expect_true(all(ae_result$USUBJID %in% c("SUBJ-2", "SUBJ-4")))
      expect_equal(sum(ae_result$USUBJID == "SUBJ-2"), 1)
      expect_equal(sum(ae_result$USUBJID == "SUBJ-4"), 3)
    },
    args = list(x = board)
  )
})

test_that("dm workflow with flatten block works", {
  subjects <- data.frame(
    id = 1:3,
    name = c("Alice", "Bob", "Carol"),
    stringsAsFactors = FALSE
  )

  orders <- data.frame(
    order_id = 1:5,
    subject_id = c(1, 1, 2, 3, 3),
    amount = c(100, 150, 200, 50, 75),
    stringsAsFactors = FALSE
  )

  board <- blockr.core::new_board(
    blocks = list(
      subjects_data = blockr.core::new_static_block(data = subjects),
      orders_data = blockr.core::new_static_block(data = orders),
      dm_obj = new_dm_block(),
      dm_keys = new_dm_add_keys_block(
        pk_table = "subjects_data",
        pk_column = "id",
        fk_table = "orders_data",
        fk_column = "subject_id"
      ),
      flattened = new_dm_flatten_block(
        start = "orders_data",
        recursive = TRUE
      )
    ),
    links = c(
      blockr.core::new_link("subjects_data", "dm_obj", "subjects_data"),
      blockr.core::new_link("orders_data", "dm_obj", "orders_data"),
      blockr.core::new_link("dm_obj", "dm_keys", "data"),
      blockr.core::new_link("dm_keys", "flattened", "data")
    )
  )

  testServer(
    blockr.core::board_server,
    {
      for (i in 1:5) {
        session$flushReact()
        Sys.sleep(0.05)
      }

      flat_result <- rv$blocks$flattened$server$result()
      expect_s3_class(flat_result, "data.frame")

      # Should have all orders with joined subject info
      expect_equal(nrow(flat_result), 5)
      expect_true("name" %in% colnames(flat_result))
      expect_true("amount" %in% colnames(flat_result))
    },
    args = list(x = board)
  )
})
