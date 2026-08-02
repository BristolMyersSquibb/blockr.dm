#' Read multiple files into a dm object
#'
#' A block for reading multiple data files into a dm (data model) object.
#' Supports reading from Excel files (each sheet becomes a table), ZIP archives
#' (containing multiple data files), directories (reading all data files), or
#' serialization formats (RDS, QS, RData).
#'
#' @param path Character. Path to file or directory. Can be:
#'   - An Excel file (.xlsx, .xls): Each sheet becomes a dm table
#'   - A ZIP file (.zip): Extracted files become dm tables
#'   - A directory path: All data files in directory become dm tables
#'   - An RDS file (.rds): Can contain a dm, data.frame, or list of data.frames
#'   - An RData file (.rdata, .rda): All data.frames become dm tables
#' @param selected_tables Character vector naming the tables to read. `NULL`
#'   (the default) means nothing has been chosen yet and the block waits:
#'   pointing it at a directory of study data must not read all of it before
#'   a table is asked for.
#' @param ... Forwarded to [blockr.core::new_data_block()]
#'
#' @section External control:
#' `path` and `selected_tables` are externally controllable (see
#' [blockr.core::external_ctrl_vars()]), so a board update, the assistant or
#' any other controller can point the block at a different dataset with a
#' `mod` delta instead of replacing the block. This works because the block's
#' expression is a pure function of those two state variables, the way
#' [blockr.io::new_read_block()]'s is: whoever writes them, the read follows
#' on the next flush. There is no separate confirm step that could leave the
#' reported path and the data on screen disagreeing.
#'
#' @details
#' ## File Handling
#'
#' **Excel files (.xlsx, .xls):**
#' - Each sheet becomes a table in the dm object
#' - Table names are derived from sheet names
#'
#' **ZIP files (.zip):**
#' - Archive is extracted to a temporary directory
#' - All recognized data files (CSV, Excel, Parquet, etc.) are read
#' - Table names are derived from file names (without extension)
#'
#' **Directories:**
#' - All recognized data files in the directory are read
#' - Subdirectories are not traversed (flat read)
#' - Table names are derived from file names (without extension)
#'
#' **Serialized files (.rds):**
#' - If the file contains a dm object, it is returned directly
#' - If the file contains a data.frame, it is wrapped in a dm
#' - If the file contains a list of data.frames, each becomes a dm table
#'
#' **RData files (.rdata, .rda):**
#' - All data.frame objects in the file become dm tables
#'
#' @return A blockr data block that reads files and returns a dm object.
#'
#' @examples
#' # Create a dm read block (interactive mode)
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dm)
#'   serve(new_dm_read_block())
#' }
#'
#' @importFrom shinyjs useShinyjs
#' @export
new_dm_read_block <- function(
  path = character(),
  selected_tables = NULL,
  ...
) {
  # Both arguments are read inside the server closure, i.e. long after this
  # call returns. Left as promises they carry the caller's environment with
  # them, and any route that revives the app object in a FRESH R process
  # (shinytest2's AppDriver, a callr worker, a serialized app) forces them
  # there, where the caller's local variables no longer exist: the block dies
  # with "object 'x' not found" before it ever reaches its first flush.
  force(path)
  force(selected_tables)

  upload_path <- blockr.core::blockr_option(
    "upload_path",
    tools::R_user_dir("blockr", "data")
  )
  upload_path <- path.expand(upload_path)

  blockr.core::new_data_block(
    server = function(id) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # The block's two state variables, and the only two things anything
          # writes: everything else below is derived from them. The path is
          # kept as given, whether or not it resolves in this session -- a
          # board can be restored before its data lands, and an external
          # controller may point the block at a path that is not there yet.
          # Unreadable is reported (badge, block error), never silently
          # swallowed by dropping the path.
          initial_path <- if (length(path) > 0 && nzchar(path[[1]])) {
            stats::setNames(path, basename(path))
          } else {
            character()
          }
          r_path <- shiny::reactiveVal(initial_path)

          # NULL / empty = no table chosen yet, which holds the read back.
          r_selected_tables <- shiny::reactiveVal(selected_tables)

          # Set by the input observers just before they write the state they
          # own, so the observers that mirror that state back into the widgets
          # can tell "the user did this" from "someone else did this" and skip
          # the echo. Same guard blockr.dplyr's `js_block_state()` uses.
          self_write <- new.env(parent = emptyenv())
          self_write$tables <- FALSE

          set_selected <- function(val) {
            if (!length(val)) {
              val <- NULL
            }
            if (identical(val, shiny::isolate(r_selected_tables()))) {
              return(invisible(FALSE))
            }
            self_write$tables <- TRUE
            r_selected_tables(val)
            invisible(TRUE)
          }

          # The file-access policy applies to user-chosen filesystem paths, not
          # to the app's own sandboxes: uploads land under upload_path, so a
          # path there is exempt (mirrors blockr.io leaving uploads alone).
          is_policed_path <- function(p) {
            np <- normalizePath(p, winslash = "/", mustWork = FALSE)
            roots <- normalizePath(
              c(tempdir(), upload_path), winslash = "/", mustWork = FALSE
            )
            !any(startsWith(np, paste0(roots, "/")))
          }

          # Data directory from board options
          data_dir_reactive <- shiny::reactive({
            blockr.core::coal(
              blockr.core::get_board_option_or_null("data_dir", session),
              ""
            )
          })

          # The path as it will actually be read: relative paths resolve
          # against the board's data directory, absolute ones are left alone.
          resolved_path <- shiny::reactive({
            p <- r_path()

            if (!length(p) || !nzchar(p[[1]])) {
              return(character())
            }

            path_val <- unname(p[[1]])
            data_dir <- data_dir_reactive()

            if (nzchar(data_dir) && !grepl("^(/|~|[A-Za-z]:)", path_val)) {
              path_val <- file.path(data_dir, path_val)
            }

            path_val
          })

          # Non-empty when the deployment's file-access policy rejects the
          # current path. Derived rather than checked in the path observer, so
          # a restored board and an externally set path are policed on exactly
          # the same terms as a typed one.
          policy_error <- shiny::reactive({
            p <- resolved_path()

            if (!length(p) || !is_policed_path(p)) {
              return("")
            }

            tryCatch(
              {
                blockr.io::resolve_and_check(p, "read")
                ""
              },
              error = function(e) conditionMessage(e)
            )
          })

          input_type <- shiny::reactive({
            p <- resolved_path()

            if (!length(p) || nzchar(policy_error())) {
              return("unknown")
            }

            detect_dm_input_type(p)
          })

          # Path input module. `value` makes the module responsible for
          # keeping the field in step with the block's path, including when
          # this block's dock panel mounts long after the first push -- the
          # case a block pushing on its own cannot get right, because the
          # widget's script is not loaded yet to receive it.
          file_path <- blockr.io::path_input_server(
            "file_path",
            data_dir = data_dir_reactive,
            mode = "file",
            value = r_path
          )

          # JS -> R: the field commits on Enter, blur and browse, so this is
          # one write per user decision, not one per keystroke. Validation is
          # not this observer's job any more -- it records what was asked for
          # and the derived reactives above say what came of it.
          shiny::observeEvent(file_path(), {
            path_val <- file_path()
            shiny::req(nzchar(path_val))

            # A field reports its value the moment it binds, and inside a
            # dock that happens after the observer above has already written
            # the restored path into it -- so the first thing a lazily
            # mounted block hears is its own path coming back. Read as a user
            # action it clears the table selection, which is how a restored
            # board used to arrive with no data.
            current <- r_path()

            if (length(current) && identical(unname(current[[1]]), path_val)) {
              return()
            }

            r_selected_tables(NULL)
            r_path(stats::setNames(path_val, basename(path_val)))
          }, ignoreInit = TRUE)

          # Handle file upload with persistence
          shiny::observeEvent(input$file_upload, {
            shiny::req(input$file_upload)

            r_selected_tables(NULL)

            # Create upload directory if it doesn't exist
            upload_dir <- upload_path
            dir.create(upload_dir, recursive = TRUE, showWarnings = FALSE)

            # Process uploaded file
            temp_path <- input$file_upload$datapath
            original_name <- input$file_upload$name

            # Generate unique filename with timestamp
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S_%OS3")
            safe_name <- gsub("[^A-Za-z0-9._-]", "_", original_name)
            permanent_path <- file.path(
              upload_dir,
              paste0(timestamp, "_", safe_name)
            )

            # Copy file to permanent storage
            file.copy(temp_path, permanent_path, overwrite = FALSE)

            names(permanent_path) <- original_name

            # The `r_path()` observer above mirrors this into the text field.
            r_path(permanent_path)
          })

          # Cheap table discovery (no full data read). Never lists files
          # outside the allowed roots. Returns an empty frame rather than
          # `req()`ing, because "there are no tables here" is an answer the
          # observers below have to be able to act on.
          available_tables <- shiny::reactive({
            resolved <- resolved_path()

            if (!length(resolved) || nzchar(policy_error())) {
              return(empty_table_info())
            }

            discover_dm_tables(resolved, input_type())
          })

          # Whether the tables section has anything to show. The picker is
          # mounted by a message, so an empty container is all there is until
          # a path resolves.
          output$has_tables <- shiny::reactive({
            nrow(available_tables()) > 0
          })
          shiny::outputOptions(output, "has_tables", suspendWhenHidden = FALSE)

          # The picker's option list: one entry per discovered file, with the
          # format and size as the grey secondary label `Blockr.Select`
          # renders next to the name.
          table_options <- shiny::reactive({
            tbl_info <- available_tables()
            lapply(seq_len(nrow(tbl_info)), function(i) {
              list(
                value = tbl_info$name[[i]],
                label = trimws(paste(tbl_info$ext[[i]], tbl_info$size[[i]]))
              )
            })
          })

          push_picker <- function(selected) {
            dm_picker_mount(
              session, "table_select", table_options(), selected, "multi",
              placeholder = "Select tables to load..."
            )
          }

          # R -> JS. One entry point for every write, whoever made it: the
          # message handler mounts the picker on first receipt, reconciles it
          # on later ones, and queues by element id, so a block in a dock
          # panel that has not been opened yet is served when it opens. That
          # is the whole reason blocks push at a container rather than
          # `update*Input()` at a widget.
          shiny::observeEvent(available_tables(), {
            tbl_info <- available_tables()
            shiny::req(nrow(tbl_info) > 0)

            # What still exists at the new path stays selected, the rest
            # drops (and with it, the read of it).
            keep <- intersect(shiny::isolate(r_selected_tables()),
                              tbl_info$name)

            push_picker(keep)
            set_selected(keep)
          })

          # The container announcing that it is on screen. A data block
          # discovers its tables once, at boot, and a block in a dock panel
          # nobody has opened yet has not even loaded the picker's script by
          # then -- so that one push was dropped by a Shiny with no handler
          # for it, out of reach of any client-side queue. This is the
          # answer to the question the widget asks on arrival.
          shiny::observeEvent(input$table_select_ready, {
            tbl_info <- available_tables()
            shiny::req(nrow(tbl_info) > 0)

            push_picker(intersect(r_selected_tables(), tbl_info$name))
          })

          shiny::observeEvent(r_selected_tables(), {

            if (self_write$tables) {
              self_write$tables <- FALSE
              return()
            }

            shiny::req(nrow(available_tables()) > 0)

            push_picker(
              intersect(r_selected_tables(), available_tables()$name)
            )
          }, ignoreInit = TRUE, ignoreNULL = FALSE)

          # JS -> R. The picker reports only what a user did to it: it is
          # mounted with its selection already in it and does not announce
          # itself the way a stock Shiny input does, so there is no bind-time
          # empty value here to mistake for someone clearing the field.
          shiny::observeEvent(input$table_select, {
            val <- input$table_select

            if (is.null(val)) {
              return()
            }

            set_selected(unlist(val, use.names = FALSE))
          }, ignoreNULL = FALSE)

          # Written straight to the state rather than through `set_selected()`
          # so the mirror above picks them up and moves the picker.
          shiny::observeEvent(input$select_all_tables, {
            tbl_info <- available_tables()
            shiny::req(nrow(tbl_info) > 0)
            r_selected_tables(tbl_info$name)
          }, ignoreInit = TRUE)

          shiny::observeEvent(input$select_none_tables, {
            r_selected_tables(NULL)
          }, ignoreInit = TRUE)

          # Status badge for file type. Re-sent when the widget reports it
          # is on screen, for the same reason the value is: the first push
          # can predate the panel that carries the badge.
          shiny::observe({
            input[["file_path-path_text_ready"]]
            type <- input_type()
            resolved <- resolved_path()

            type_labels <- c(
              excel = "Excel", zip = "ZIP", directory = "Directory",
              serialized = "R data", rdata = "RData"
            )

            if (nzchar(policy_error())) {
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("file_path-path_text"),
                text = "Blocked",
                state = "error"
              ))
            } else if (length(resolved) > 0 && type != "unknown") {
              label <- unname(type_labels[type]) %||% "File"
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("file_path-path_text"),
                text = label,
                state = "success"
              ))
            } else if (length(resolved) > 0) {
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("file_path-path_text"),
                text = if (file.exists(resolved)) "Unsupported" else "Not found",
                state = "error"
              ))
            } else {
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("file_path-path_text"),
                text = "",
                state = "none"
              ))
            }
          })

          list(
            # The read is a function of the block's state, not of a button
            # press: whatever sets `path` / `selected_tables` -- the user, a
            # restored board, or an external controller -- gets a new
            # expression on the next flush. A path that cannot be read becomes
            # a `stop()` INSIDE the expression (as blockr.io's read block
            # does), so it lands in blockr.core's per-block error boundary
            # instead of throwing out of this reactive.
            expr = shiny::reactive({
              resolved <- resolved_path()
              shiny::req(length(resolved) > 0)

              blocked <- policy_error()

              if (nzchar(blocked)) {
                return(bquote(stop(.(blocked), call. = FALSE)))
              }

              type <- input_type()

              if (identical(type, "unknown")) {
                # Two different failures, and the difference is the whole
                # message: browsing to a single CSV is a normal mis-click (the
                # path autocomplete lists those files), not a missing path.
                msg <- if (file.exists(resolved)) {
                  paste0(
                    "'", basename(resolved), "' is not a dm source. Pick the ",
                    "directory that contains the data files, or an Excel / ",
                    "ZIP / RDS / RData file."
                  )
                } else {
                  paste0("No such file or directory: '", resolved, "'.")
                }

                return(bquote(stop(.(msg), call. = FALSE)))
              }

              # No table chosen yet is "not ready", not "read everything":
              # pointing the block at a directory of study data must not read
              # all of it before anyone asks for a table.
              selected <- r_selected_tables()
              shiny::req(length(selected) > 0)

              dm_read_expr(resolved, type, selected)
            }),
            state = list(
              path = r_path,
              selected_tables = r_selected_tables
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shinyjs::useShinyjs(),
        block_responsive_css(),
        dm_table_picker_deps(),
        shiny::div(
          class = "block-container dm-read-block-container",
          shiny::tags$style(shiny::HTML("
            .dm-read-block-container .shiny-input-container {
              width: 100% !important;
            }

            /* Read report: muted by default, coloured only when it carries
               news (a conversion happened, or the cache is unreachable). */
            .dm-read-status {
              margin-top: 8px;
              font-size: 0.75rem;
            }
            .dm-read-status--cached { color: #047857; }
            .dm-read-status--converted { color: #b45309; }
            .dm-read-status--error { color: #b91c1c; }

            .blockr-select-all-link a {
              margin-left: 6px;
              color: #9ca3af;
              cursor: pointer;
              text-decoration: none;
            }
            .blockr-select-all-link a:hover {
              color: #2563eb;
              text-decoration: underline;
            }
          ")),

          # File Location section
          shiny::div(
            class = "block-section",
            shiny::tags$h4("File Location", class = "mb-3"),
            shiny::tags$p(
              class = "blockr-path-hint",
              "Enter path to an Excel file,",
              "ZIP archive, directory, or",
              "serialized R object."
            ),
            shiny::div(
              style = "display: none;",
              shiny::fileInput(
                inputId = ns("file_upload"),
                label = NULL,
                multiple = FALSE,
                accept = c(".xlsx", ".xls", ".zip", ".rds", ".rdata", ".rda")
              )
            ),
            blockr.io::path_input_ui(
              shiny::NS(id, "file_path"),
              upload_id = ns("file_upload")
            )
          ),

          # Tables section. The picker is an empty container here and is
          # mounted by the server's `dm-table-picker` message, the same way
          # every other table picker in this package works.
          shiny::conditionalPanel(
            condition = "output.has_tables",
            ns = ns,
            shiny::div(
              class = "block-section",
              shiny::tags$label(
                class = "control-label",
                style = paste0(
                  "display: flex; align-items: baseline;",
                  " justify-content: space-between;",
                  " margin-top: 16px; width: 100%;"
                ),
                `for` = ns("table_select"),
                shiny::span("Tables to include"),
                shiny::span(
                  class = "blockr-select-all-link",
                  shiny::actionLink(ns("select_all_tables"), "All"),
                  shiny::actionLink(ns("select_none_tables"), "None")
                )
              ),
              shiny::div(
                id = ns("table_select"),
                class = "dm-read-tables-picker",
                # Opts this container into the picker's arrival announce;
                # the server answers on `table_select_ready`.
                `data-dm-picker` = "true"
              )
            )
          )
        )
      )
    },
    class = "dm_read_block",
    allow_empty_state = TRUE,
    external_ctrl = c("path", "selected_tables"),
    ...
  )
}


#' The "no tables here" answer from [discover_dm_tables()]
#'
#' @return A zero-row data frame with the columns table discovery returns.
#' @noRd
empty_table_info <- function() {
  data.frame(
    name = character(), ext = character(), size = character(),
    stringsAsFactors = FALSE
  )
}


#' Format byte sizes for display
#' @param bytes Numeric byte count
#' @return Formatted string (e.g. "1.2 KB")
#' @noRd
format_bytes <- function(bytes) {
  if (is.na(bytes) || bytes < 0) return("")
  if (bytes < 1024) return(paste0(bytes, " B"))
  if (bytes < 1024 * 1024) return(paste0(round(bytes / 1024, 1), " KB"))
  paste0(round(bytes / (1024 * 1024), 1), " MB")
}


#' Discover table names without reading data
#'
#' Uses cheap operations (sheet listing, file listing) to discover available
#' table names from a dm source path. Avoids full data reads for
#' format types where discovery is possible (Excel, ZIP, directory).
#'
#' @param path Path to file or directory
#' @param input_type Character: "excel", "zip", "directory",
#'   "serialized", "rdata"
#' @return A data.frame with columns `name`, `ext`, and `size`
#' @noRd
discover_dm_tables <- function(path, input_type) {
  empty <- empty_table_info()
  tryCatch({
    switch(input_type,
      excel = {
        sheets <- readxl::excel_sheets(path)
        nms <- make.names(sheets, unique = TRUE)
        data.frame(name = nms, ext = rep("Sheet", length(nms)),
                   size = rep("", length(nms)), stringsAsFactors = FALSE)
      },
      zip = {
        zip_info <- utils::unzip(path, list = TRUE)
        exts <- blockr.io::file_extensions()
        pattern <- paste0(
          "\\.(", paste(exts, collapse = "|"), ")$"
        )
        keep <- grepl(pattern, zip_info$Name, ignore.case = TRUE)
        data_info <- zip_info[keep, , drop = FALSE]
        nms <- make.names(
          tools::file_path_sans_ext(basename(data_info$Name)),
          unique = TRUE
        )
        file_exts <- toupper(tools::file_ext(data_info$Name))
        sizes <- vapply(data_info$Length, format_bytes, character(1))
        data.frame(name = nms, ext = file_exts, size = sizes,
                   stringsAsFactors = FALSE)
      },
      directory = {
        files <- list_data_files(path)
        nms <- make.names(
          tools::file_path_sans_ext(basename(files)),
          unique = TRUE
        )
        file_exts <- toupper(tools::file_ext(files))
        sizes <- vapply(file.size(files), format_bytes, character(1))
        data.frame(name = nms, ext = file_exts, size = sizes,
                   stringsAsFactors = FALSE)
      },
      serialized = {
        obj <- readRDS(path)
        if (inherits(obj, "dm")) {
          nms <- names(obj)
          sizes <- vapply(nms, function(n) {
            paste0(nrow(obj[[n]]), " rows")
          }, character(1))
          data.frame(name = nms, ext = rep("Table", length(nms)),
                     size = sizes, stringsAsFactors = FALSE)
        } else if (inherits(obj, "data.frame")) {
          data.frame(name = "data", ext = "Table",
                     size = paste0(nrow(obj), " rows"),
                     stringsAsFactors = FALSE)
        } else if (is.list(obj)) {
          are_dfs <- vapply(obj, inherits, logical(1), "data.frame")
          dfs <- obj[are_dfs]
          nms <- names(dfs)
          if (is.null(nms)) nms <- paste0("table", seq_len(sum(are_dfs)))
          sizes <- vapply(dfs, function(d) paste0(nrow(d), " rows"),
                          character(1))
          data.frame(name = nms, ext = rep("Table", length(nms)),
                     size = sizes, stringsAsFactors = FALSE)
        } else {
          empty
        }
      },
      rdata = {
        env <- new.env()
        load(path, envir = env)
        objs <- as.list(env)
        are_dfs <- vapply(objs, inherits, logical(1), "data.frame")
        dfs <- objs[are_dfs]
        nms <- names(dfs)
        sizes <- vapply(dfs, function(d) paste0(nrow(d), " rows"),
                        character(1))
        data.frame(name = nms, ext = rep("Table", length(nms)),
                   size = sizes, stringsAsFactors = FALSE)
      },
      empty
    )
  }, error = function(e) empty)
}


#' Detect dm input type from path
#' @param path Path to file or directory
#' @return Character: "excel", "zip", "directory",
#'   "serialized", "rdata", or "unknown"
#' @noRd
detect_dm_input_type <- function(path) {
  if (dir.exists(path)) {
    return("directory")
  }

  ext <- tolower(tools::file_ext(path))

  switch(ext,
    xlsx = , xls = "excel",
    zip = "zip",
    rds = "serialized",
    rdata = , rda = "rdata",
    "unknown"
  )
}


#' List data files in a directory
#' @noRd
list_data_files <- function(dir_path) {
  extensions <- blockr.io::file_extensions()
  pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")$")
  list.files(dir_path, pattern = pattern, ignore.case = TRUE, full.names = TRUE)
}


#' Build expression to read files into dm
#' @noRd
dm_read_expr <- function(path, input_type, selected = NULL) {
  switch(input_type,
    excel = dm_read_expr_excel(path, selected),
    zip = dm_read_expr_zip(path, selected),
    directory = dm_read_expr_directory(path, selected),
    serialized = dm_read_expr_serialized(path, selected),
    rdata = dm_read_expr_rdata(path, selected),
    stop("Unknown input type: ", input_type)
  )
}


#' Read Excel file - each sheet becomes a table
#' @noRd
dm_read_expr_excel <- function(path, selected = NULL) {
  bquote(
    local({
      sheets <- readxl::excel_sheets(.(path))
      table_names <- make.names(sheets, unique = TRUE)

      # Filter to selected tables
      if (!is.null(.(selected))) {
        keep <- table_names %in% .(selected)
        sheets <- sheets[keep]
        table_names <- table_names[keep]
      }

      tables <- lapply(sheets, function(sheet) {
        readxl::read_excel(.(path), sheet = sheet)
      })
      names(tables) <- table_names
      do.call(dm::dm, tables)
    })
  )
}


#' Read ZIP file - extract and read all data files
#' @noRd
dm_read_expr_zip <- function(path, selected = NULL) {
  bquote(
    local({
      temp_dir <- tempfile("dm_zip_")
      dir.create(temp_dir, showWarnings = FALSE)
      on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

      utils::unzip(.(path), exdir = temp_dir)

      # Find all data files
      extensions <- blockr.io::file_extensions()
      pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")$")
      files <- list.files(temp_dir, pattern = pattern, ignore.case = TRUE,
                          full.names = TRUE, recursive = TRUE)

      if (length(files) == 0) {
        stop("No data files found in ZIP archive")
      }

      # Get table names and filter to selected
      table_names <- make.names(
        tools::file_path_sans_ext(basename(files)),
        unique = TRUE
      )
      if (!is.null(.(selected))) {
        keep <- table_names %in% .(selected)
        files <- files[keep]
        table_names <- table_names[keep]
      }

      # Read each file
      tables <- .(dm_read_tables_expr(cache = FALSE))

      names(tables) <- table_names
      do.call(dm::dm, tables)
    })
  )
}


#' Read directory - read all data files
#' @noRd
dm_read_expr_directory <- function(path, selected = NULL) {
  bquote(
    local({
      # Find all data files
      extensions <- blockr.io::file_extensions()
      pattern <- paste0(
        "\\.(", paste(extensions, collapse = "|"), ")$"
      )
      files <- list.files(
        .(path), pattern = pattern,
        ignore.case = TRUE, full.names = TRUE
      )

      if (length(files) == 0) {
        stop("No data files found in directory")
      }

      # Get table names and filter to selected
      table_names <- make.names(
        tools::file_path_sans_ext(basename(files)),
        unique = TRUE
      )
      if (!is.null(.(selected))) {
        keep <- table_names %in% .(selected)
        files <- files[keep]
        table_names <- table_names[keep]
      }

      # Read each file
      tables <- .(dm_read_tables_expr())

      names(tables) <- table_names
      do.call(dm::dm, tables)
    })
  )
}


#' Read serialized file (RDS or QS) into dm
#' @noRd
dm_read_expr_serialized <- function(path, selected = NULL) {
  bquote(
    local({
      obj <- readRDS(.(path))

      if (inherits(obj, "dm")) {
        if (!is.null(.(selected))) {
          return(dm::dm_select_tbl(obj, dplyr::all_of(.(selected))))
        }
        return(obj)
      }
      if (inherits(obj, "data.frame")) {
        return(dm::dm(data = obj))
      }
      if (is.list(obj) && length(obj) > 0) {
        are_dfs <- vapply(obj, inherits, logical(1), "data.frame")
        if (all(are_dfs)) {
          if (is.null(names(obj))) names(obj) <- paste0("table", seq_along(obj))
          names(obj) <- make.names(names(obj), unique = TRUE)
          # Filter to selected
          if (!is.null(.(selected))) {
            obj <- obj[names(obj) %in% .(selected)]
          }
          return(do.call(dm::dm, obj))
        }
      }
      stop("File must contain a dm, data.frame, or list of data.frames")
    })
  )
}


#' Read RData file into dm
#' @noRd
dm_read_expr_rdata <- function(path, selected = NULL) {
  bquote(
    local({
      env <- new.env()
      load(.(path), envir = env)
      objs <- as.list(env)

      # Keep only data.frames
      are_dfs <- vapply(objs, inherits, logical(1), "data.frame")
      tables <- objs[are_dfs]

      if (length(tables) == 0) {
        stop("RData file contains no data.frames")
      }

      names(tables) <- make.names(names(tables), unique = TRUE)

      # Filter to selected
      if (!is.null(.(selected))) {
        tables <- tables[names(tables) %in% .(selected)]
      }

      do.call(dm::dm, tables)
    })
  )
}


#' Custom output for dm_read_block
#' @inheritParams block_output.dm_block
#' @method block_output dm_read_block
#' @export
block_output.dm_read_block <- function(x, result, session) {

  # Rendered from here rather than from the block server because this is the
  # one place that runs AFTER the read: blockr.core calls `block_output()`
  # with each new result, so the report lands with the data it describes.
  session$output$read_status <- shiny::renderUI({

    if (!inherits(result, "dm")) {
      return(NULL)
    }

    status <- dm_read_status(names(dm::dm_get_tables(result)))

    shiny::tags$p(
      class = paste0("blockr-path-hint dm-read-status dm-read-status--",
                     status$state),
      status$text
    )
  })

  block_output.dm_block(x, result, session)
}


#' Custom UI for dm_read_block
#'
#' The read report belongs to the block, not to the expression UI: it is
#' written by `block_output()`, which runs in the block's own namespace (the
#' same one `dm_table_preview` lives in), and it describes the result rather
#' than the controls.
#'
#' @inheritParams block_ui.dm_block
#' @method block_ui dm_read_block
#' @export
block_ui.dm_read_block <- function(id, x, ...) {
  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "read_status")),
    block_ui.dm_block(id, x, ...)
  )
}


#' @method block_render_trigger dm_read_block
#' @export
block_render_trigger.dm_read_block <- function(
  x,
  session = blockr.core::get_session()
) {
  NULL
}
