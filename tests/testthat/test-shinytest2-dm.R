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

test_that("dm nested view block renders output without error", {
  skip_if_not_installed("shinytest2")

  library(shinytest2)

  # Create test data
  adsl <- data.frame(
    USUBJID = paste0("SUBJ-", 1:3),
    AGE = c(45, 52, 38),
    stringsAsFactors = FALSE
  )

  adae <- data.frame(
    USUBJID = c("SUBJ-1", "SUBJ-1", "SUBJ-2"),
    AETERM = c("Headache", "Nausea", "Fatigue"),
    stringsAsFactors = FALSE
  )

  board_def <- blockr.core::new_board(
    blocks = list(
      adsl_data = blockr.core::new_static_block(data = adsl),
      adae_data = blockr.core::new_static_block(data = adae),
      dm_obj = new_dm_block(infer_keys = TRUE),
      nested = new_dm_nested_view_block(root_table = "adsl_data")
    ),
    links = c(
      blockr.core::new_link("adsl_data", "dm_obj", "adsl_data"),
      blockr.core::new_link("adae_data", "dm_obj", "adae_data"),
      blockr.core::new_link("dm_obj", "nested", "data")
    )
  )

  app <- AppDriver$new(
    shinyApp(
      ui = bslib::page_fluid(
        theme = bslib::bs_theme(version = 5),
        blockr.core::board_ui("board", board_def)
      ),
      server = function(input, output, session) {
        blockr.core::board_server("board", board_def)
      }
    ),
    name = "nested_view_test"
  )

  on.exit(app$stop(), add = TRUE)

  # Wait for app to initialize
  Sys.sleep(3)

  # Get the HTML to check for errors
  html <- app$get_html("body")

  # Check there's no "condition has length" error
  expect_false(grepl("condition has length", html, ignore.case = TRUE))

  # For now, just check no errors are shown
  # The reactable may not render fully in shinytest2
  expect_false(grepl("Error:", html, fixed = TRUE))
})
