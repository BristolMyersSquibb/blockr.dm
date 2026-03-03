test_that("safety dm block constructor creates correct class hierarchy", {
  skip_if_not_installed("safetyData")
  block <- new_safety_dm_block()
  expect_s3_class(
    block,
    c("safety_dm_block", "dm_block", "data_block", "block")
  )
})

test_that("safety dm block returns dm with 3 keyed tables", {
  skip_if_not_installed("safetyData")
  block <- new_safety_dm_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Should return a dm object
      expect_s3_class(result, "dm")

      # Should have 3 tables: adsl, adae, adlb
      tables <- sort(names(dm::dm_get_tables(result)))
      expect_equal(tables, c("adae", "adlb", "adsl"))

      # Table dimensions match safetyData originals
      expect_equal(nrow(result$adsl), nrow(safetyData::adam_adsl))
      expect_equal(nrow(result$adae), nrow(safetyData::adam_adae))
      expect_equal(nrow(result$adlb), nrow(safetyData::adam_adlbc))

      # PK is set on adsl (USUBJID)
      pks <- dm::dm_get_all_pks(result)
      expect_true("adsl" %in% pks$table)

      # FKs are set on adae and adlb
      fks <- dm::dm_get_all_fks(result)
      expect_true("adae" %in% fks$child_table)
      expect_true("adlb" %in% fks$child_table)
      expect_true(all(fks$parent_table == "adsl"))
    },
    args = list(x = block)
  )
})
