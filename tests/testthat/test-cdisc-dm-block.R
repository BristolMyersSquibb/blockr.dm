test_that("cdisc dm block constructor", {
  block <- new_cdisc_dm_block()
  expect_s3_class(
    block,
    c("cdisc_dm_block", "dm_block", "transform_block", "block")
  )
})

test_that("ADAM key setup - PK on ADSL, FK on children", {
  block <- new_cdisc_dm_block(dedup_cols = FALSE)

  adsl <- data.frame(
    USUBJID = c("S1", "S2", "S3"),
    STUDYID = rep("STUDY1", 3),
    AGE = c(30, 40, 50),
    SEX = c("M", "F", "M"),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("S1", "S1", "S2"),
    STUDYID = rep("STUDY1", 3),
    AGE = c(30, 30, 40),
    AEDECOD = c("Headache", "Nausea", "Fatigue"),
    stringsAsFactors = FALSE
  )
  adlb <- data.frame(
    USUBJID = c("S1", "S2", "S3", "S1"),
    STUDYID = rep("STUDY1", 4),
    AGE = c(30, 40, 50, 30),
    PARAM = c("ALT", "ALT", "ALT", "AST"),
    stringsAsFactors = FALSE
  )

  dm_input <- dm::dm(adsl = adsl, adae = adae, adlb = adlb)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "dm")

      # Check PK on ADSL
      pks <- dm::dm_get_all_pks(result)
      expect_true("adsl" %in% pks$table)

      # Check FKs on children
      fks <- dm::dm_get_all_fks(result)
      expect_true("adae" %in% fks$child_table)
      expect_true("adlb" %in% fks$child_table)
      expect_true(all(fks$parent_table == "adsl"))
    },
    args = list(
      x = block,
      data = list(data = function() dm_input)
    )
  )
})

test_that("SDTM key setup - DM table as parent", {
  block <- new_cdisc_dm_block(dedup_cols = FALSE)

  dm_tbl <- data.frame(
    USUBJID = c("S1", "S2"),
    STUDYID = rep("STUDY1", 2),
    AGE = c(30, 40),
    stringsAsFactors = FALSE
  )
  ae <- data.frame(
    USUBJID = c("S1", "S1", "S2"),
    STUDYID = rep("STUDY1", 3),
    AETERM = c("Headache", "Nausea", "Fatigue"),
    stringsAsFactors = FALSE
  )

  dm_input <- dm::dm(dm = dm_tbl, ae = ae)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "dm")

      pks <- dm::dm_get_all_pks(result)
      expect_true("dm" %in% pks$table)

      fks <- dm::dm_get_all_fks(result)
      expect_true("ae" %in% fks$child_table)
      expect_equal(fks$parent_table[fks$child_table == "ae"], "dm")
    },
    args = list(
      x = block,
      data = list(data = function() dm_input)
    )
  )
})

test_that("Column deduplication removes shared columns from children", {
  block <- new_cdisc_dm_block(dedup_cols = TRUE)

  adsl <- data.frame(
    USUBJID = c("S1", "S2"),
    STUDYID = rep("STUDY1", 2),
    AGE = c(30, 40),
    SEX = c("M", "F"),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("S1", "S2"),
    STUDYID = rep("STUDY1", 2),
    AGE = c(30, 40),
    SEX = c("M", "F"),
    AEDECOD = c("Headache", "Nausea"),
    stringsAsFactors = FALSE
  )

  dm_input <- dm::dm(adsl = adsl, adae = adae)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "dm")

      # ADSL should still have all columns
      adsl_result <- dm::pull_tbl(result, adsl)
      expect_true("AGE" %in% names(adsl_result))
      expect_true("SEX" %in% names(adsl_result))

      # ADAE should have AGE and SEX removed
      adae_result <- dm::pull_tbl(result, adae)
      expect_false("AGE" %in% names(adae_result))
      expect_false("SEX" %in% names(adae_result))

      # ADAE should still have USUBJID, STUDYID, and its own columns
      expect_true("USUBJID" %in% names(adae_result))
      expect_true("STUDYID" %in% names(adae_result))
      expect_true("AEDECOD" %in% names(adae_result))
    },
    args = list(
      x = block,
      data = list(data = function() dm_input)
    )
  )
})

test_that("No parent table warns and passes through", {
  block <- new_cdisc_dm_block()

  tbl1 <- data.frame(
    USUBJID = c("S1", "S2"),
    VALUE = c(1, 2),
    stringsAsFactors = FALSE
  )
  tbl2 <- data.frame(
    USUBJID = c("S1", "S1"),
    RESULT = c("A", "B"),
    stringsAsFactors = FALSE
  )

  dm_input <- dm::dm(exposure = tbl1, labs = tbl2)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expect_warning(session$flushReact(), "No CDISC parent table")

      result <- session$returned$result()
      expect_s3_class(result, "dm")
      expect_equal(sort(names(dm::dm_get_tables(result))), c("exposure", "labs"))
    },
    args = list(
      x = block,
      data = list(data = function() dm_input)
    )
  )
})

test_that("Single parent table, no children", {
  block <- new_cdisc_dm_block(dedup_cols = FALSE)

  adsl <- data.frame(
    USUBJID = c("S1", "S2", "S3"),
    STUDYID = rep("STUDY1", 3),
    AGE = c(30, 40, 50),
    stringsAsFactors = FALSE
  )

  dm_input <- dm::dm(adsl = adsl)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "dm")

      pks <- dm::dm_get_all_pks(result)
      expect_true("adsl" %in% pks$table)

      # No FKs expected
      fks <- dm::dm_get_all_fks(result)
      expect_equal(nrow(fks), 0)
    },
    args = list(
      x = block,
      data = list(data = function() dm_input)
    )
  )
})
