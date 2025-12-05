test_that("dm block renders output without error", {
  skip_if_not_installed("shinytest2")

  library(shinytest2)

  # Create test data
  df1 <- data.frame(id = 1:3, name = c("a", "b", "c"))
  df2 <- data.frame(parent_id = c(1, 1, 2), value = 10:12)

  app <- AppDriver$new(
    shinyApp(
      ui = bslib::page_fluid(
        theme = bslib::bs_theme(version = 5),
        blockr.core::board_ui("board",
          blockr.core::new_board(
            blocks = list(
              tbl1 = blockr.core::new_static_block(data = df1),
              tbl2 = blockr.core::new_static_block(data = df2),
              dm_obj = new_dm_block()
            ),
            links = c(
              blockr.core::new_link("tbl1", "dm_obj", "tbl1"),
              blockr.core::new_link("tbl2", "dm_obj", "tbl2")
            )
          )
        )
      ),
      server = function(input, output, session) {
        blockr.core::board_server("board",
          blockr.core::new_board(
            blocks = list(
              tbl1 = blockr.core::new_static_block(data = df1),
              tbl2 = blockr.core::new_static_block(data = df2),
              dm_obj = new_dm_block()
            ),
            links = c(
              blockr.core::new_link("tbl1", "dm_obj", "tbl1"),
              blockr.core::new_link("tbl2", "dm_obj", "tbl2")
            )
          )
        )
      }
    ),
    name = "dm_block_test"
  )

  on.exit(app$stop(), add = TRUE)

  # Wait for app to initialize
  Sys.sleep(3)

  # Get the HTML to check for errors and dm output
  html <- app$get_html("body")

  # Check there's no "argument is of length zero" error
  expect_false(grepl("argument is of length zero", html, ignore.case = TRUE))

 # Check that dm diagram is displayed (grViz widget renders SVG)
  expect_true(grepl("grViz|svg", html, ignore.case = TRUE))
})
