# The primary key is a filter dimension; foreign keys are not. USUBJID used
# to be hidden everywhere, so a reader could not pick single subjects.

pk_dm <- function() {
  adsl <- data.frame(
    USUBJID = sprintf("S%02d", 1:20),
    SEX = rep(c("F", "M"), 10)
  )
  adae <- data.frame(
    USUBJID = rep(adsl$USUBJID, each = 2),
    AESEV = rep(c("MILD", "SEVERE"), 20)
  )
  dm::dm(adsl = adsl, adae = adae) |>
    dm::dm_add_pk(adsl, USUBJID) |>
    dm::dm_add_fk(adae, USUBJID, adsl)
}

test_that("the primary key is offered, foreign keys are not", {
  d <- pk_dm()
  shiny::testServer(
    crossfilter_server(
      active_dims = list(), filters = list(), range_filters = list(),
      measure = NULL, agg_func = NULL
    ),
    args = list(data = function() d),
    {
      info <- column_info_per_table()
      expect_true("USUBJID" %in% unlist(info$adsl$dimensions))
      expect_false("USUBJID" %in% unlist(c(
        info$adae$dimensions, info$adae$range_dimensions,
        info$adae$date_dimensions
      )))
    }
  )
})

test_that("a filter on the primary key narrows the child table", {
  d <- pk_dm()
  blk <- new_crossfilter_block(
    active_dims = list(adsl = "USUBJID"),
    filters = list(adsl = list(USUBJID = list("S01", "S02")))
  )
  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() d)),
    {
      session$flushReact()
      res <- eval(session$returned$expr(), list(data = d))
      expect_setequal(res$adsl$USUBJID, c("S01", "S02"))
      expect_setequal(unique(res$adae$USUBJID), c("S01", "S02"))
      expect_equal(nrow(res$adae), 4L)
    }
  )
})
