test_that("safety dm block constructor creates correct class hierarchy", {
  skip_if_not_installed("safetyData")
  block <- new_safety_dm_block()
  expect_s3_class(
    block,
    c("safety_dm_block", "dm_block", "data_block", "block")
  )
})

test_that("safety dm block returns dm with default tables (adsl + adtte)", {
  skip_if_not_installed("safetyData")
  block <- new_safety_dm_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Should return a dm object
      expect_s3_class(result, "dm")

      # Should have 2 tables: adsl, adtte
      tables <- sort(names(dm::dm_get_tables(result)))
      expect_equal(tables, c("adsl", "adtte"))

      # Table dimensions match safetyData originals
      expect_equal(nrow(result$adsl), nrow(safetyData::adam_adsl))
      expect_equal(nrow(result$adtte), nrow(safetyData::adam_adtte))

      # PK is set on adsl (USUBJID)
      pks <- dm::dm_get_all_pks(result)
      expect_true("adsl" %in% pks$table)

      # FK is set on adtte
      fks <- dm::dm_get_all_fks(result)
      expect_true("adtte" %in% fks$child_table)
      expect_true(all(fks$parent_table == "adsl"))
    },
    args = list(x = block)
  )
})

test_that("safety dm block respects custom table selection", {
  skip_if_not_installed("safetyData")
  block <- new_safety_dm_block(tables = c("adsl", "adae", "adlbc"))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "dm")
      tables <- sort(names(dm::dm_get_tables(result)))
      expect_equal(tables, c("adae", "adlbc", "adsl"))

      # FKs on adae and adlbc
      fks <- dm::dm_get_all_fks(result)
      expect_true("adae" %in% fks$child_table)
      expect_true("adlbc" %in% fks$child_table)
    },
    args = list(x = block)
  )
})

test_that("safety dm block always includes adsl", {
  skip_if_not_installed("safetyData")
  # Pass tables without adsl — it should be added automatically
  block <- new_safety_dm_block(tables = c("adtte"))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "dm")
      tables <- sort(names(dm::dm_get_tables(result)))
      expect_true("adsl" %in% tables)
      expect_true("adtte" %in% tables)
    },
    args = list(x = block)
  )
})
