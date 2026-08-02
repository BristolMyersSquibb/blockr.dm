# External control for the dm transform blocks. Each one's expression has to
# be a function of the state it exposes, and each one's widget has to follow
# that state -- otherwise "controllable" means the block reports one thing and
# does another.

ctrl_dm <- function() {
  dm::dm(
    adsl = data.frame(USUBJID = c("a", "b"), AGE = c(34, 61)),
    adae = data.frame(USUBJID = c("a", "a"), AETERM = c("x", "y")),
    adlb = data.frame(USUBJID = "b", LBTEST = "z")
  )
}

test_that("dm_pull_block follows an externally set table", {

  block <- new_dm_pull_block(table = "adsl")

  expect_setequal(
    blockr.core::external_ctrl_vars(block), c("table", "block_name")
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_identical(names(session$returned$result()), c("USUBJID", "AGE"))

      session$returned$state$table("adae")
      session$flushReact()

      expect_identical(
        names(session$returned$result()), c("USUBJID", "AETERM")
      )
      expect_identical(session$returned$state$table(), "adae")
    },
    args = list(x = block, data = list(data = function() ctrl_dm()))
  )
})

test_that("dm_select_block follows an externally set table set", {

  block <- new_dm_select_block(tables = c("adsl", "adae"))

  expect_setequal(
    blockr.core::external_ctrl_vars(block), c("tables", "block_name")
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_setequal(
        names(dm::dm_get_tables(session$returned$result())),
        c("adsl", "adae")
      )

      session$returned$state$tables(c("adsl", "adlb"))
      session$flushReact()

      expect_setequal(
        names(dm::dm_get_tables(session$returned$result())),
        c("adsl", "adlb")
      )
    },
    args = list(x = block, data = list(data = function() ctrl_dm()))
  )
})

test_that("cdisc_dm_block follows externally set switches", {

  block <- new_cdisc_dm_block(set_keys = TRUE, dedup_cols = TRUE)

  expect_setequal(
    blockr.core::external_ctrl_vars(block),
    c("set_keys", "dedup_cols", "block_name")
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      with_keys <- rlang::expr_text(session$returned$expr())
      expect_match(with_keys, "dm_add_pk")

      session$returned$state$set_keys(FALSE)
      session$flushReact()

      # The expression is rebuilt from the switch, so the keys step is gone.
      expect_no_match(
        rlang::expr_text(session$returned$expr()), "dm_add_pk"
      )
      expect_false(session$returned$state$set_keys())
    },
    args = list(x = block, data = list(data = function() ctrl_dm()))
  )
})

test_that("a board update drives a dm_pull block", {

  # The path an assistant's modify_block takes: validated against
  # external_ctrl_vars(), then written into the live block server.
  board <- blockr.core::new_board(
    blocks = c(
      src = blockr.core::new_static_block(ctrl_dm()),
      pull = new_dm_pull_block(table = "adsl")
    ),
    links = blockr.core::links(from = "src", to = "pull")
  )

  expect_silent(
    blockr.core::validate_board_update(
      list(blocks = list(mod = list(pull = list(table = "adae")))),
      board
    )
  )

  expect_error(
    blockr.core::validate_board_update(
      list(blocks = list(mod = list(pull = list(nonsense = 1)))),
      board
    ),
    class = "board_update_blocks_mod_not_ctrl"
  )

  shiny::testServer(
    blockr.core:::get_s3_method("board_server", board),
    {
      session$flushReact()

      pre_server <- rv$blocks$pull$server
      expect_identical(
        names(rv$blocks$pull$server$result()), c("USUBJID", "AGE")
      )

      board_update(
        list(blocks = list(mod = list(pull = list(table = "adae"))))
      )
      session$flushReact()

      expect_identical(rv$blocks$pull$server, pre_server)
      expect_identical(
        names(rv$blocks$pull$server$result()), c("USUBJID", "AETERM")
      )
    },
    args = list(x = board)
  )
})
