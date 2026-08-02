# Browser-level cover for the dm read block. `testServer` cannot see the two
# things that matter most here: that the app ever goes idle, and that picking a
# table in the widget is the whole interaction.

test_that("dm read block reaches idle and reads without a confirm step", {
  skip_if_not_installed("shinytest2")

  library(shinytest2)

  dir <- withr::local_tempdir()
  utils::write.csv(head(mtcars), file.path(dir, "adsl.csv"), row.names = FALSE)
  utils::write.csv(head(iris), file.path(dir, "adae.csv"), row.names = FALSE)

  board <- blockr.core::new_board(
    blocks = c(rd = new_dm_read_block(path = dir, selected_tables = "adsl"))
  )

  # `AppDriver$new()` waits for the app to become stable, and that wait IS the
  # regression test: while `path` was an unforced promise it was evaluated in
  # the driver's fresh R process, where this function's `dir` does not exist,
  # so the block died before its first flush and the app never went idle.
  app <- AppDriver$new(
    shinyApp(
      ui = bslib::page_fluid(
        theme = bslib::bs_theme(version = 5),
        blockr.core::board_ui("board", board)
      ),
      server = function(input, output, session) {
        blockr.core::board_server("board", board)
      }
    ),
    name = "dm_read_block"
  )

  on.exit(app$stop(), add = TRUE)

  html <- app$get_html("body")

  # No arming button: selecting is what triggers the read.
  expect_false(grepl("blockr-table-confirm-btn", html, fixed = TRUE))
  expect_false(grepl("load_data", html, fixed = TRUE))

  # The restored path and selection produced a dm, unprompted. The viewer
  # reports how many tables are in it, which is what changes below.
  expect_match(app$get_text("body"), "1 independent tables")

  # And picking a second table in the widget is the whole interaction.
  app$set_inputs(`board-block_rd-expr-table_select` = c("adsl", "adae"))
  app$wait_for_idle()

  expect_match(app$get_text("body"), "2 independent tables")
})
