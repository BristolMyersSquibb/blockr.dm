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

          # Path input module
          file_path <- blockr.io::path_input_server(
            "file_path",
            data_dir = data_dir_reactive,
            mode = "file"
          )

          # R -> JS: the field shows the block's path, whoever set it --
          # constructor, board restore, upload, or an external controller.
          # `silent` suppresses the change event, so this cannot loop back
          # through `file_path()`, and the handler queues the message when the
          # element has not bound yet (a block in a deferred dock panel).
          shiny::observe({
            p <- r_path()
            session$sendCustomMessage("blockr-path-set-value", list(
              id = session$ns("file_path-path_text"),
              value = if (length(p)) unname(p[[1]]) else "",
              silent = TRUE
            ))
          })

          # JS -> R: the field commits on Enter, blur and browse, so this is
          # one write per user decision, not one per keystroke. Validation is
          # not this observer's job any more -- it records what was asked for
          # and the derived reactives above say what came of it.
          shiny::observeEvent(file_path(), {
            path_val <- file_path()
            shiny::req(nzchar(path_val))

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
          # outside the allowed roots.
          available_tables <- shiny::reactive({
            resolved <- resolved_path()
            shiny::req(length(resolved) > 0, !nzchar(policy_error()))

            discover_dm_tables(resolved, input_type())
          })

          # Output for conditional panel
          output$has_tables <- shiny::reactive({
            nrow(available_tables()) > 0
          })
          shiny::outputOptions(output, "has_tables", suspendWhenHidden = FALSE)

          # Update table selection UI when tables change. Carries the current
          # selection across a path change: what still exists at the new path
          # stays selected, the rest drops (and with it, the read of it).
          shiny::observeEvent(available_tables(), {
            tbl_info <- available_tables()
            shiny::req(nrow(tbl_info) > 0)
            labels <- paste(tbl_info$name, tbl_info$ext, tbl_info$size,
                            sep = "|||")
            choices <- stats::setNames(tbl_info$name, labels)
            current_selected <- r_selected_tables()
            if (!is.null(current_selected)) {
              current_selected <- intersect(current_selected, tbl_info$name)
            }
            shiny::updateSelectizeInput(
              session, "table_select",
              choices = choices,
              selected = current_selected
            )
          })

          # JS -> R
          shiny::observeEvent(input$table_select, {
            val <- input$table_select

            if (!length(val)) {
              val <- NULL
            }

            if (identical(val, r_selected_tables())) {
              return()
            }

            self_write$tables <- TRUE
            r_selected_tables(val)
          }, ignoreInit = TRUE, ignoreNULL = FALSE)

          # R -> JS: mirror an externally set selection into the widget, so
          # what the block reads and what the block shows cannot drift apart.
          shiny::observeEvent(r_selected_tables(), {

            if (self_write$tables) {
              self_write$tables <- FALSE
              return()
            }

            # Only mirror once the widget has choices: a selection pushed at a
            # choice-less selectize is silently dropped, and the empty input
            # coming back would wipe the very state we are displaying. Until
            # then the choices observer above carries it.
            tbl_info <- available_tables()
            shiny::req(nrow(tbl_info) > 0)

            shiny::updateSelectizeInput(
              session, "table_select",
              selected = intersect(r_selected_tables(), tbl_info$name)
            )
          }, ignoreInit = TRUE, ignoreNULL = FALSE)

          # Status badge for file type
          shiny::observe({
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
        shiny::div(
          class = "block-container dm-read-block-container",
          shiny::tags$style(shiny::HTML("
            .dm-read-block-container .shiny-input-container {
              width: 100% !important;
            }

            /* Table selector */
            .blockr-table-selector {
              display: flex;
              align-items: stretch;
            }
            .blockr-table-selector .shiny-input-container {
              margin-bottom: 0 !important;
            }
            .blockr-table-selector .selectize-control {
              flex: 1;
              min-width: 0;
            }
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

            /* Dropdown item styling */
            .blockr-table-item {
              display: flex; align-items: center; gap: 8px;
              padding: 4px 8px;
            }
            .blockr-table-name {
              flex: 1; min-width: 0;
              overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
            }
            .blockr-table-badge {
              flex-shrink: 0; font-size: 0.7rem;
              padding: 1px 6px; border-radius: 4px;
              background: #f3f4f6; color: #6b7280; border: 1px solid #e5e7eb;
            }
            .blockr-table-size {
              flex-shrink: 0; font-size: 0.75rem; color: #999;
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

          # Tables section (visible when tables are available)
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
                  shiny::tags$a(
                    id = ns("select_all_tables"),
                    href = "#", "All"
                  ),
                  shiny::tags$a(
                    id = ns("select_none_tables"),
                    href = "#", "None"
                  )
                )
              ),
              shiny::div(
                class = "blockr-table-selector",
                shiny::selectizeInput(
                  ns("table_select"),
                  label = NULL,
                  choices = NULL,
                  multiple = TRUE,
                  width = "100%",
                  options = list(
                    placeholder = "Select tables to load...",
                    render = I("{
  option: function(data, escape) {
    var p = data.label.split('|||');
    var nm = p[0] || '';
    var ext = p[1] || '';
    var sz = p[2] || '';
    return '<div class=\"blockr-table-item\">' +
      '<span class=\"blockr-table-name\">' +
      escape(nm) + '</span>' +
      (ext ? '<span class=\"blockr-table-badge\">' +
      escape(ext) + '</span>' : '') +
      (sz ? '<span class=\"blockr-table-size\">' +
      escape(sz) + '</span>' : '') +
      '</div>';
  },
  item: function(data, escape) {
    var nm = data.label.split('|||')[0];
    return '<div>' + escape(nm) + '</div>';
  }
}")
                  )
                )
              ),
              # Selectize conveniences only: opening the dropdown when there is
              # nothing selected yet, and the All / None shortcuts. Selecting
              # is what triggers the read, so neither of these needs to talk
              # to R -- they drive the same input the user would.
              shiny::tags$script(shiny::HTML(sprintf(
                "
                $(document).on('shiny:value', function(e) {
                  if (e.name !== '%s') return;
                  setTimeout(function() {
                    var sel = $('#%s')[0];
                    if (!sel || !sel.selectize) return;
                    var sz = sel.selectize;
                    var noItems = sz.items.length === 0;
                    var hasOpts = Object.keys(sz.options).length > 0;
                    if (noItems && hasOpts) {
                      sz.open();
                    }
                    // All / None links
                    var allSel = '#%s';
                    $(allSel).off('click.selall').on(
                      'click.selall', function(e) {
                      e.preventDefault();
                      sz.setValue(Object.keys(sz.options));
                    });
                    var noneSel = '#%s';
                    $(noneSel).off('click.selnone').on(
                      'click.selnone', function(e) {
                      e.preventDefault();
                      sz.clear();
                    });
                  }, 100);
                });
                ",
                ns("has_tables"),
                ns("table_select"),
                ns("select_all_tables"),
                ns("select_none_tables")
              )))
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
  empty <- data.frame(name = character(), ext = character(),
                      size = character(), stringsAsFactors = FALSE)
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
  block_output.dm_block(x, result, session)
}


#' Custom UI for dm_read_block
#' @inheritParams block_ui.dm_block
#' @method block_ui dm_read_block
#' @export
block_ui.dm_read_block <- function(id, x, ...) {
  block_ui.dm_block(id, x, ...)
}


#' @method block_render_trigger dm_read_block
#' @export
block_render_trigger.dm_read_block <- function(
  x,
  session = blockr.core::get_session()
) {
  NULL
}
