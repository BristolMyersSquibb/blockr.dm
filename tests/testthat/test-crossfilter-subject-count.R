# The header counts subjects only when the block names what they are
# (`subject_unit`, set by blockr.pharma's population filter): R then sends how
# many keys the parent table holds (`parent_n`) and the word. The generic
# block counts rows.

subject_dm <- function(key = "USUBJID") {
  adsl <- data.frame(id = c("a", "b", "c"), SEX = c("F", "M", "F"))
  ae <- data.frame(id = c("a", "a", "b"), AESEV = c("MILD", "SEVERE", "MILD"))
  names(adsl)[1] <- key
  names(ae)[1] <- key
  d <- dm::dm(adsl = adsl, ae = ae)
  d <- dm::dm_add_pk(d, adsl, !!rlang::sym(key))
  dm::dm_add_fk(d, ae, !!rlang::sym(key), adsl)
}

test_that("the lookups carry the parent table's subject count", {
  for (key in c("USUBJID", "pid")) {
    d <- subject_dm(key)
    info <- build_crossfilter_lookups(
      dm::dm_get_tables(d),
      list(ae = "AESEV"),
      dm::dm_get_all_pks(d),
      dm::dm_get_all_fks(d)
    )
    # Three subjects, though ae only covers two of them.
    expect_identical(info$parent_n, 3L)
    expect_null(info$subject_unit)
  }
})

test_that("the generic block counts rows, whatever its key", {
  blk <- new_crossfilter_block(active_dims = list(adsl = "SEX", ae = "AESEV"))

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() subject_dm())),
    {
      sent <- list()
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        sent[[length(sent) + 1L]] <<- list(type = type, message = message)
        invisible()
      }
      session$flushReact()

      msgs <- Filter(function(m) m$type == "js-crossfilter-data", sent)
      msg <- msgs[[length(msgs)]]$message
      expect_null(msg$parent_n)
      expect_null(msg$subject_unit)
    }
  )
})

test_that("a block that names its subjects ships parent_n and the unit", {
  blk <- new_crossfilter_block(active_dims = list(adsl = "SEX", ae = "AESEV"),
                               subject_unit = "patients")

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() subject_dm())),
    {
      sent <- list()
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        sent[[length(sent) + 1L]] <<- list(type = type, message = message)
        invisible()
      }
      session$flushReact()

      msgs <- Filter(function(m) m$type == "js-crossfilter-data", sent)
      expect_gte(length(msgs), 1L)
      msg <- msgs[[length(msgs)]]$message
      expect_identical(msg$parent_n, 3L)
      expect_identical(msg$subject_unit, "patients")
    }
  )
})

test_that("without a parent table the message leaves the subject count out", {
  blk <- new_crossfilter_block(active_dims = list(.tbl = "Species"))

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() iris)),
    {
      sent <- list()
      root <- session$rootScope()
      root$sendCustomMessage <- function(type, message) {
        sent[[length(sent) + 1L]] <<- list(type = type, message = message)
        invisible()
      }
      session$flushReact()

      msgs <- Filter(function(m) m$type == "js-crossfilter-data", sent)
      msg <- msgs[[length(msgs)]]$message
      expect_null(msg$parent_n)
      expect_null(msg$subject_unit)
    }
  )
})
