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

test_that("path, selected_tables and args are externally controllable", {

  expect_setequal(
    blockr.core::external_ctrl_vars(new_dm_read_block()),
    c("path", "selected_tables", "args", "block_name")
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

test_that("a widget mounting late does not wipe the restored state", {

  # What a dock does to a block that is not in the active view: the UI enters
  # the DOM only when the panel is first shown, and every input in it then
  # reports its value to the server for the first time. The path field
  # reports the path the block itself had written into it, so the first thing
  # a lazily mounted block hears is its own path coming back. Read as a user
  # picking a new path, that cleared the table selection, and the restored
  # board came up with no data at all.
  dir <- make_study_dir(list(adsl = data.frame(x = 1:2),
                             adae = data.frame(y = 3:4)))

  block <- new_dm_read_block(path = dir, selected_tables = c("adsl", "adae"))

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      session$setInputs(`expr-file_path-path_text` = dir)
      session$flushReact()

      expect_identical(
        session$returned$state$selected_tables(), c("adsl", "adae")
      )
      expect_setequal(
        names(dm::dm_get_tables(session$returned$result())),
        c("adsl", "adae")
      )
    },
    args = list(x = block, data = list())
  )
})

test_that("an unreachable path keeps the selection it was saved with", {

  # A board restored while its data is not there -- an unmounted drive, a
  # study directory that has not synced yet -- has a selection and nothing to
  # put in the picker. No picker is mounted, so nothing reports back, and the
  # saved selection is still there when the path comes good.
  missing <- file.path(withr::local_tempdir(), "not-there")

  block <- new_dm_read_block(path = missing, selected_tables = "adsl")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      expect_identical(session$returned$state$selected_tables(), "adsl")
    },
    args = list(x = block, data = list())
  )
})

test_that("the picker is mounted by message, selection included", {

  # The widget is put on screen by `dm-table-picker`, the queued custom
  # message every table picker in this package uses -- not by an
  # `update*Input()` aimed at an element that may not be in the DOM yet.
  sent <- list()
  fake_session <- list(
    ns = function(x) paste0("blk-", x),
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    }
  )

  blockr.dm:::dm_picker_mount(
    fake_session, "table_select",
    list(list(value = "adsl", label = "CSV 30 B")),
    "adsl", "multi", placeholder = "Select tables to load..."
  )

  expect_length(sent, 1L)
  expect_identical(sent[[1]]$type, "dm-table-picker")

  msg <- sent[[1]]$message

  expect_identical(msg$id, "blk-table_select")
  expect_identical(msg$mode, "multi")
  expect_identical(msg$placeholder, "Select tables to load...")
  # A lone table has to stay an array of one: auto_unbox would otherwise
  # hand JS a bare string and the picker would show nothing.
  expect_identical(msg$selected, list("adsl"))
  expect_identical(msg$options[[1]]$value, "adsl")
})

test_that("the user can still empty the selection", {

  # What the None link and removing the last tag both come down to. The
  # picker reports an empty list, which -- unlike a stock input's bind-time
  # announcement -- really is somebody's decision.
  dir <- make_study_dir(list(adsl = data.frame(x = 1:2),
                             adae = data.frame(y = 3:4)))

  block <- new_dm_read_block(path = dir, selected_tables = "adsl")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      session$setInputs(`expr-table_select` = list())
      session$flushReact()

      expect_null(session$returned$state$selected_tables())
    },
    args = list(x = block, data = list())
  )
})

test_that("All and None drive the selection", {

  dir <- make_study_dir(list(adsl = data.frame(x = 1:2),
                             adae = data.frame(y = 3:4)))

  block <- new_dm_read_block(path = dir)

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      session$setInputs(`expr-select_all_tables` = 1)
      session$flushReact()
      expect_setequal(
        session$returned$state$selected_tables(), c("adsl", "adae")
      )

      session$setInputs(`expr-select_none_tables` = 1)
      session$flushReact()
      expect_null(session$returned$state$selected_tables())
    },
    args = list(x = block, data = list())
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

test_that("the emitted read is layered: registry list, dm wrap", {
  dir <- withr::local_tempdir()
  write.csv(data.frame(x = 1:3), file.path(dir, "adsl.csv"), row.names = FALSE)
  write.csv(data.frame(y = 4:6), file.path(dir, "adae.csv"), row.names = FALSE)

  # No cache configured: explicit member reads from blockr.io's registry,
  # wrapped in new_dm() by this block. No blockr call in the emitted code.
  expr <- dm_read_expr(dir, selected = c("adsl", "adae"))
  txt <- rlang::expr_text(expr)
  expect_match(txt, "^dm::new_dm\\(list\\(")
  expect_match(txt, "adsl = readr::read_csv")
  expect_no_match(txt, "blockr")

  result <- eval(expr)
  expect_s3_class(result, "dm")
  expect_setequal(names(result), c("adsl", "adae"))

  # With a parquet cache configured the directory read keeps routing through
  # dm_read_tables(), the cache seam -- members still discovered at build
  # time, as literals.
  cache <- withr::local_tempdir()
  withr::local_options(blockr.dm_read_cache_dir = cache)

  cached <- dm_read_expr(dir, selected = "adsl")
  cached_txt <- rlang::expr_text(cached)
  expect_match(cached_txt, "blockr.dm::dm_read_tables")
  expect_no_match(cached_txt, "list.files")
  expect_s3_class(eval(cached), "dm")
})

test_that("an rds holding a dm stays a native read, lists go to the registry", {
  dm_path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(dm::dm(adsl = data.frame(x = 1)), dm_path)

  expr <- dm_read_expr(dm_path, selected = "adsl")
  expect_match(rlang::expr_text(expr), "dm::dm_select_tbl\\(readRDS")

  list_path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(adsl = data.frame(x = 1), adae = data.frame(y = 2)), list_path)

  expr <- dm_read_expr(list_path, selected = "adae")
  expect_match(rlang::expr_text(expr), "^dm::new_dm\\(readRDS")
  expect_setequal(names(eval(expr)), "adae")

  # discovery agrees: native rds-dm labels, registry labels for the list
  expect_identical(dm_discover_tables(dm_path)$name, "adsl")
  expect_identical(dm_discover_tables(list_path)$name, c("adsl", "adae"))
})

test_that("uniform member options reach every member: semicolon CSVs", {
  dir <- withr::local_tempdir()
  writeLines(c("x;y", "1;a", "2;b"), file.path(dir, "adsl.csv"))
  writeLines(c("z;w", "9;c"), file.path(dir, "adae.csv"))

  expr <- dm_read_expr(dir, args = list(sep = ";"))
  expect_match(rlang::expr_text(expr), 'delim = ";"')

  result <- eval(expr)
  expect_s3_class(result, "dm")
  expect_identical(names(dm::dm_get_tables(result)$adsl), c("x", "y"))

  # options and the cache do not combine: with args set, a configured cache
  # backend is bypassed in favor of the registry expression
  withr::local_options(blockr.dm_read_cache_dir = withr::local_tempdir())
  with_args <- dm_read_expr(dir, args = list(sep = ";"))
  expect_no_match(rlang::expr_text(with_args), "dm_read_tables")
  without <- dm_read_expr(dir)
  expect_match(rlang::expr_text(without), "dm_read_tables")
})

test_that("args is externally controllable state", {
  dir <- withr::local_tempdir()
  writeLines(c("x;y", "1;a"), file.path(dir, "adsl.csv"))

  block <- new_dm_read_block(path = dir, selected_tables = "adsl")
  expect_contains(blockr.core::external_ctrl_vars(block), "args")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_no_match(
        rlang::expr_text(session$returned$expr()), 'delim = ";"'
      )

      session$returned$state$args(list(sep = ";"))
      session$flushReact()

      expect_match(
        rlang::expr_text(session$returned$expr()), 'delim = ";"'
      )
      expect_identical(
        names(session$returned$result()$adsl), c("x", "y")
      )
    },
    args = list(x = block, data = list())
  )
})

test_that("the gear band writes args, and args mirror back", {
  dir <- withr::local_tempdir()
  writeLines(c("x;y", "1;a"), file.path(dir, "adsl.csv"))

  block <- new_dm_read_block(path = dir, selected_tables = "adsl")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # widget -> state: picking the semicolon delimiter lands in args
      # (default keys dropped, so only the deviation is recorded)
      session$setInputs(`expr-opt_sep` = ";", `expr-opt_skip` = 0)
      session$flushReact()
      expect_identical(session$returned$state$args(), list(sep = ";"))
      expect_match(
        rlang::expr_text(session$returned$expr()), 'delim = ";"'
      )

      # back to the default empties the args again
      session$setInputs(`expr-opt_sep` = ",")
      session$flushReact()
      expect_identical(session$returned$state$args(), list())

      # an external write is not echoed back into a self-write loop
      session$returned$state$args(list(sep = ";", skip = 2))
      session$flushReact()
      expect_identical(
        session$returned$state$args(), list(sep = ";", skip = 2)
      )
    },
    args = list(x = block, data = list())
  )
})

test_that("the gear only shows for sources with delimited-text members", {
  dir <- withr::local_tempdir()
  writeLines(c("x;y", "1;a"), file.path(dir, "adsl.csv"))
  expect_true(dm_source_has_text_members(dir))

  # a folder without text members has no options to offer
  pq_dir <- withr::local_tempdir()
  saveRDS(data.frame(x = 1), file.path(pq_dir, "not_text.rds"))
  expect_false(dm_source_has_text_members(pq_dir))

  # an rds or a workbook is not a text source
  rds <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(a = data.frame(x = 1)), rds)
  expect_false(dm_source_has_text_members(rds))

  # a zip is judged by its central directory
  archive <- file.path(dir, "a.zip")
  zip::zip(archive, "adsl.csv", root = dir, mode = "cherry-pick")
  expect_true(dm_source_has_text_members(archive))
})
