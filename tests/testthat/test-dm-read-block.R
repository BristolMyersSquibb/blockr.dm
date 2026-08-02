# The dm read block's expression is a function of its state (`path`,
# `selected_tables`), not of a confirm button -- that is what makes the two
# variables externally controllable, and what the tests below pin down.

make_study_dir <- function(tables) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  for (nm in names(tables)) {
    utils::write.csv(
      tables[[nm]], file.path(dir, paste0(nm, ".csv")), row.names = FALSE
    )
  }
  dir
}

test_that("path and selected_tables are externally controllable", {

  expect_setequal(
    blockr.core::external_ctrl_vars(new_dm_read_block()),
    c("path", "selected_tables", "block_name")
  )
})

test_that("a restored path and selection read without a confirm step", {

  dir <- make_study_dir(list(adsl = data.frame(x = 1:2),
                             adae = data.frame(y = 3:4)))

  block <- new_dm_read_block(path = dir, selected_tables = "adsl")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "dm")
      expect_identical(names(dm::dm_get_tables(result)), "adsl")
    },
    args = list(x = block, data = list())
  )
})

test_that("writing state re-reads: the external control path", {

  dir_a <- make_study_dir(list(adsl = data.frame(x = 1:2),
                               adae = data.frame(y = 3:4)))
  dir_b <- make_study_dir(list(adsl = data.frame(x = 5:6),
                               adlb = data.frame(z = 7:8)))

  block <- new_dm_read_block(path = dir_a, selected_tables = "adsl")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_identical(
        dm::dm_get_tables(session$returned$result())$adsl$x, c(1, 2)
      )

      # What `apply_block_mod_delta()` does on a board update: write the
      # state reactiveVals of the live block, no reconstruction.
      session$returned$state$path(dir_b)
      session$flushReact()

      expect_identical(
        dm::dm_get_tables(session$returned$result())$adsl$x, c(5, 6)
      )

      # A second table at the new path, again by writing state only.
      session$returned$state$selected_tables(c("adsl", "adlb"))
      session$flushReact()

      expect_setequal(
        names(dm::dm_get_tables(session$returned$result())),
        c("adsl", "adlb")
      )
    },
    args = list(x = block, data = list())
  )
})

test_that("no selection holds the read back", {

  dir <- make_study_dir(list(adsl = data.frame(x = 1:2),
                             adae = data.frame(y = 3:4)))

  block <- new_dm_read_block(path = dir)

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      # Tables were discovered, but pointing at a directory does not read all
      # of it: the block waits for a selection. `req()` in the expression is
      # how blockr.core is told "waiting", and it surfaces as shiny's silent
      # error rather than as data.
      expect_error(
        session$returned$result(),
        class = "shiny.silent.error"
      )
    },
    args = list(x = block, data = list())
  )
})

test_that("a single data file says what it is, not 'not found'", {

  # The path autocomplete lists the CSVs inside a directory, so clicking one
  # is a normal mis-click. It is not a dm source, but it does exist -- saying
  # "no such file" there would send the user looking for the wrong problem.
  dir <- make_study_dir(list(adsl = data.frame(x = 1:2)))
  csv <- file.path(dir, "adsl.csv")

  block <- new_dm_read_block(path = csv, selected_tables = "adsl")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      txt <- rlang::expr_text(session$returned$expr())
      expect_match(txt, "is not a dm source")
      expect_no_match(txt, "No such file")
    },
    args = list(x = block, data = list())
  )
})

test_that("a board update points the block at another study", {

  # The path an external controller (the assistant's modify_block, a URL
  # handler, a parent app) actually takes: a `mod` delta on the board, which
  # blockr.core validates against `external_ctrl_vars()` and then writes into
  # the live block server. Nothing here reconstructs the block.
  dir_a <- make_study_dir(list(adsl = data.frame(x = 1:2),
                               adae = data.frame(y = 3:4)))
  dir_b <- make_study_dir(list(adsl = data.frame(x = 5:6),
                               adlb = data.frame(z = 7:8)))

  board <- blockr.core::new_board(
    blocks = c(rd = new_dm_read_block(path = dir_a, selected_tables = "adsl"))
  )

  expect_silent(
    blockr.core::validate_board_update(
      list(blocks = list(mod = list(rd = list(path = dir_b)))),
      board
    )
  )

  # A non-controllable argument is still refused: the block opted `path` and
  # `selected_tables` in, not everything.
  expect_error(
    blockr.core::validate_board_update(
      list(blocks = list(mod = list(rd = list(class = "nope")))),
      board
    ),
    class = "board_update_blocks_mod_not_ctrl"
  )

  shiny::testServer(
    blockr.core:::get_s3_method("board_server", board),
    {
      session$flushReact()

      pre_server <- rv$blocks$rd$server

      expect_identical(
        dm::dm_get_tables(rv$blocks$rd$server$result())$adsl$x, c(1, 2)
      )

      board_update(
        list(
          blocks = list(
            mod = list(
              rd = list(path = dir_b, selected_tables = c("adsl", "adlb"))
            )
          )
        )
      )

      session$flushReact()

      # Same server object: the block was controlled, not replaced.
      expect_identical(rv$blocks$rd$server, pre_server)

      result <- rv$blocks$rd$server$result()
      expect_setequal(names(dm::dm_get_tables(result)), c("adsl", "adlb"))
      expect_identical(dm::dm_get_tables(result)$adsl$x, c(5, 6))

      # And the state the block exposes (what a save writes out) is the new
      # path, not the one it booted with.
      expect_identical(unname(rv$blocks$rd$server$state$path()), dir_b)
      expect_identical(
        rv$blocks$rd$server$state$selected_tables(), c("adsl", "adlb")
      )
    },
    args = list(x = board)
  )
})

test_that("an unreadable path becomes a stop() inside the expression", {

  missing <- file.path(withr::local_tempdir(), "not-there")

  block <- new_dm_read_block(path = missing, selected_tables = "adsl")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      # As in blockr.io's read block: the failure rides in the expression so
      # blockr.core's per-block error boundary reports it, rather than the
      # block silently keeping whatever it read last.
      expect_match(
        rlang::expr_text(session$returned$expr()),
        "No such file or directory"
      )
      expect_null(session$returned$result())
    },
    args = list(x = block, data = list())
  )
})
