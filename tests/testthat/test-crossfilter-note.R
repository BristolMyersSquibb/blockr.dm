note_dm <- function() {
  adsl <- data.frame(USUBJID = c("a", "b"), SEX = c("F", "M"))
  attr(adsl, "blockr_note") <- "CA-244-0001 · extract 2026-09-01"
  ae <- data.frame(USUBJID = c("a", "a", "b"), TRTEMFL = c("Y", "N", "Y"))
  d <- dm::dm(adsl = adsl, ae = ae)
  d <- dm::dm_add_pk(d, adsl, USUBJID)
  dm::dm_add_fk(d, ae, USUBJID, adsl)
}

test_that("crossfilter_note reads the first table's blockr_note", {
  expect_identical(
    crossfilter_note(note_dm()),
    "CA-244-0001 · extract 2026-09-01"
  )
  expect_null(crossfilter_note(dm::dm(x = data.frame(a = 1))))
})

test_that("the note survives the dm verbs a board runs before the filter", {
  d <- note_dm()
  expect_false(is.null(crossfilter_note(dm::dm_filter(d, ae = TRTEMFL == "Y"))))
  expect_false(is.null(crossfilter_note(
    do.call(dm::dm_mutate_tbl, c(list(d), dm::dm_get_tables(d)))
  )))
  expect_false(is.null(crossfilter_note(
    dm::dm_update_zoomed(dplyr::mutate(dm::dm_zoom_to(d, ae), z = 1))
  )))
})
