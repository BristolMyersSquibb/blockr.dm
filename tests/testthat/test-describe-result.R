test_that("describe_result.dm gives a compact per-table overview, no column dump", {
  adsl <- data.frame(
    USUBJID = paste0("SUBJ-", 1:3),
    AGE = c(45, 52, 38)
  )
  adae <- data.frame(
    USUBJID = c("SUBJ-1", "SUBJ-2"),
    AETERM = c("Headache", "Nausea")
  )
  d <- dm::dm(adsl = adsl, adae = adae)

  out <- describe_result.dm(d)
  expect_type(out, "character")

  txt <- paste(out, collapse = "\n")

  expect_match(txt, "- adsl: 3 rows x 2 cols", fixed = TRUE)
  expect_match(txt, "- adae: 2 rows x 2 cols", fixed = TRUE)

  expect_match(txt, "dm_pull_block(table = \"<name>\")", fixed = TRUE)
  expect_match(txt, "names(<id>$adsl)", fixed = TRUE)

  expect_false(grepl("AETERM", txt, fixed = TRUE))
  expect_false(grepl("USUBJID", txt, fixed = TRUE))
})

test_that("describe_result.dm registers onto the blockr.assistant generic", {
  skip_if_not_installed("blockr.assistant")

  d <- dm::dm(
    adsl = data.frame(USUBJID = c("SUBJ-1", "SUBJ-2"), AGE = c(45, 52))
  )

  expect_match(
    paste(blockr.assistant::describe_result(d), collapse = "\n"),
    "- adsl: 2 rows x 2 cols",
    fixed = TRUE
  )
})
