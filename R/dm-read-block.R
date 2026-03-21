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
#'   - A QS file (.qs): Can contain a dm, data.frame, or list of data.frames
#'   - An RData file (.rdata, .rda): All data.frames become dm tables
#' @param selected_tables Character vector. Optional subset of tables to include
#'   in the output dm. Default is NULL (all tables).
#' @param ... Forwarded to [blockr.core::new_data_block()]
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
#' **Serialized files (.rds, .qs):**
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
          # Path storage
          if (length(path) > 0 && (file.exists(path) || dir.exists(path))) {
            initial_path <- stats::setNames(path, basename(path))
          } else {
            initial_path <- character()
          }
          r_path <- shiny::reactiveVal(initial_path)
          r_file_path <- shiny::reactiveVal(initial_path)

          # Detected input type
          initial_type <- if (length(path) > 0) {
            detect_dm_input_type(path)
          } else {
            "unknown"
          }
          r_input_type <- shiny::reactiveVal(initial_type)

          # Selected tables (NULL = all)
          r_selected_tables <- shiny::reactiveVal(selected_tables)

          # Load button gating: armed expression
          r_expr_armed <- shiny::reactiveVal(NULL)

          # Data directory from board options
          data_dir_reactive <- shiny::reactive({
            blockr.core::coal(
              blockr.core::get_board_option_or_null("data_dir", session),
              ""
            )
          })

          # Path input module
          file_path <- blockr.io::path_input_server(
            "file_path",
            data_dir = data_dir_reactive,
            mode = "file"
          )

          # Populate path text input on restore / init
          if (length(path) > 0 && nzchar(path[[1]])) {
            shiny::observe({
              session$sendCustomMessage("blockr-path-set-value", list(
                id = session$ns("file_path-path_text"),
                value = unname(path[1]),
                silent = TRUE
              ))
            }) |> shiny::bindEvent(TRUE, once = TRUE)
          }

          # Handle path input changes
          shiny::observeEvent(file_path(), {
            path_val <- file_path()
            shiny::req(nzchar(path_val))

            r_selected_tables(NULL)
            r_expr_armed(NULL)

            # Resolve relative paths against data directory
            resolved <- path_val
            data_dir <- data_dir_reactive()
            if (
              nzchar(data_dir) &&
              !grepl("^(/|~|[A-Za-z]:)", path_val)
            ) {
              resolved <- file.path(data_dir, path_val)
            }

            if (file.exists(resolved) || dir.exists(resolved)) {
              named_path <- stats::setNames(path_val, basename(resolved))
              r_path(named_path)
              r_file_path(named_path)
              r_input_type(detect_dm_input_type(resolved))
            } else {
              r_file_path(character())
              r_input_type("unknown")
            }
          }, ignoreInit = TRUE)

          # Handle file upload with persistence
          shiny::observeEvent(input$file_upload, {
            shiny::req(input$file_upload)

            r_selected_tables(NULL)
            r_expr_armed(NULL)

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

            r_path(permanent_path)
            r_file_path(permanent_path)
            r_input_type(detect_dm_input_type(permanent_path))

            # Show uploaded file path in the text input
            session$sendCustomMessage("blockr-path-set-value", list(
              id = session$ns("file_path-path_text"),
              value = unname(permanent_path),
              silent = TRUE
            ))
          })

          # Cheap table discovery (no full data read)
          available_tables <- shiny::reactive({
            shiny::req(length(r_file_path()) > 0)
            path_val <- r_file_path()[1]

            # Resolve relative path
            data_dir <- data_dir_reactive()
            resolved <- path_val
            if (
              nzchar(data_dir) &&
              !grepl("^(/|~|[A-Za-z]:)", path_val)
            ) {
              resolved <- file.path(data_dir, path_val)
            }

            discover_dm_tables(resolved, r_input_type())
          })

          # Output for conditional panel
          output$has_tables <- shiny::reactive({
            nrow(available_tables()) > 0
          })
          shiny::outputOptions(output, "has_tables", suspendWhenHidden = FALSE)

          # Update table selection UI when tables change
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
            shiny::updateSelectizeInput(session, "table_select",
              choices = choices, selected = current_selected)
          })

          shiny::observeEvent(input$table_select, {
            r_selected_tables(input$table_select)
          }, ignoreInit = TRUE)

          # Load button handler
          shiny::observeEvent(input$load_data, {
            shiny::req(length(r_file_path()) > 0)
            path_val <- r_file_path()[1]
            input_type <- r_input_type()
            selected <- r_selected_tables()
            shiny::req(!is.null(selected), length(selected) > 0)

            # Resolve relative path
            data_dir <- data_dir_reactive()
            resolved <- path_val
            if (
              nzchar(data_dir) &&
              !grepl("^(/|~|[A-Za-z]:)", path_val)
            ) {
              resolved <- file.path(data_dir, path_val)
            }

            r_expr_armed(dm_read_expr(resolved, input_type, selected))
          })

          # Auto-arm on restore so block produces output immediately
          if (length(path) > 0 && !is.null(selected_tables)) {
            shiny::observe({
              resolved <- path[1]
              data_dir <- data_dir_reactive()
              if (
                nzchar(data_dir) &&
                !grepl("^(/|~|[A-Za-z]:)", resolved)
              ) {
                resolved <- file.path(data_dir, resolved)
              }
              r_expr_armed(
                dm_read_expr(resolved, detect_dm_input_type(resolved), selected_tables)
              )
            }) |> shiny::bindEvent(TRUE, once = TRUE)
          }

          # Status badge for file type
          shiny::observe({
            input_type <- r_input_type()
            paths <- r_file_path()

            type_labels <- c(
              excel = "Excel", zip = "ZIP", directory = "Directory",
              serialized = "R data", rdata = "RData"
            )

            if (length(paths) > 0 && input_type != "unknown") {
              label <- unname(type_labels[input_type]) %||% "File"
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("file_path-path_text"),
                text = label,
                state = "success"
              ))
            } else if (length(paths) == 0 && nzchar(file_path())) {
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("file_path-path_text"),
                text = "Not found",
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
            expr = shiny::reactive({ r_expr_armed() }),
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

            /* Table selector: integrated field with confirm button */
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
            .blockr-table-confirm-btn {
              flex-shrink: 0;
              align-self: stretch;
              display: none;
              align-items: center;
              justify-content: center;
              width: 36px;
              border: 1px solid var(--blockr-color-border, #e5e7eb);
              border-radius: 0 8px 8px 0;
              background: var(--blockr-color-bg-input, #f9fafb);
              color: #6c757d;
              cursor: pointer;
              padding: 0;
              transition: color 0.15s, background-color 0.15s;
            }
            .blockr-table-confirm-btn.has-selection {
              display: flex;
              background-color: #0d6efd;
              color: #fff;
              border-color: #0d6efd;
            }
            .blockr-table-confirm-btn.has-selection:hover {
              background-color: #0b5ed7;
              border-color: #0a58ca;
            }
            .blockr-table-confirm-btn.has-selection + .dummy,
            .blockr-table-selector:has(.blockr-table-confirm-btn.has-selection) .selectize-input {
              border-right: none !important;
              border-top-right-radius: 0 !important;
              border-bottom-right-radius: 0 !important;
            }
            .blockr-table-selector:has(.blockr-table-confirm-btn.has-selection) .selectize-dropdown {
              border-top-right-radius: 0;
            }
            .blockr-table-confirm-btn.confirmed {
              background-color: var(--blockr-color-bg-input, #f9fafb);
              color: #6c757d;
              border-color: var(--blockr-color-border, #e5e7eb);
            }
            .blockr-select-all-link {
              color: #9ca3af;
              cursor: pointer;
              text-decoration: none;
            }
            .blockr-select-all-link:hover {
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
              "Enter path to an Excel file, ZIP archive, directory, or serialized R object."
            ),
            shiny::div(
              style = "display: none;",
              shiny::fileInput(
                inputId = ns("file_upload"),
                label = NULL,
                multiple = FALSE,
                accept = c(".xlsx", ".xls", ".zip", ".rds", ".qs", ".rdata", ".rda")
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
                style = "display: flex; align-items: baseline; justify-content: space-between; margin-top: 16px; width: 100%;",
                `for` = ns("table_select"),
                shiny::span("Tables to include"),
                shiny::tags$a(
                  id = ns("select_all_tables"),
                  href = "#",
                  class = "blockr-select-all-link",
                  "Select all"
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
                        var parts = data.label.split('|||');
                        var name = parts[0] || '', ext = parts[1] || '', size = parts[2] || '';
                        return '<div class=\"blockr-table-item\">' +
                          '<span class=\"blockr-table-name\">' + escape(name) + '</span>' +
                          (ext ? '<span class=\"blockr-table-badge\">' + escape(ext) + '</span>' : '') +
                          (size ? '<span class=\"blockr-table-size\">' + escape(size) + '</span>' : '') +
                        '</div>';
                      },
                      item: function(data, escape) {
                        var name = data.label.split('|||')[0];
                        return '<div>' + escape(name) + '</div>';
                      }
                    }")
                  )
                ),
                shiny::tags$button(
                  id = ns("load_data"),
                  class = "blockr-table-confirm-btn action-button",
                  type = "button",
                  title = "Confirm selection",
                  `aria-label` = "Confirm table selection and load",
                  shiny::HTML(paste0(
                    '<svg xmlns="http://www.w3.org/2000/svg" width="16" ',
                    'height="16" viewBox="0 0 24 24" fill="none" ',
                    'stroke="currentColor" stroke-width="2.5" ',
                    'stroke-linecap="round" stroke-linejoin="round">',
                    '<line x1="5" y1="12" x2="19" y2="12"></line>',
                    '<polyline points="12 5 19 12 12 19"></polyline></svg>'
                  ))
                )
              ),
              shiny::tags$script(shiny::HTML(sprintf(
                "
                $(document).on('shiny:value', function(e) {
                  if (e.name !== '%s') return;
                  setTimeout(function() {
                    var sel = $('#%s')[0];
                    if (!sel || !sel.selectize) return;
                    var sz = sel.selectize;
                    var btn = document.getElementById('%s');
                    var arrowIcon = '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2.5\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><line x1=\"5\" y1=\"12\" x2=\"19\" y2=\"12\"></line><polyline points=\"12 5 19 12 12 19\"></polyline></svg>';
                    var checkIcon = '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2.5\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><polyline points=\"20 6 9 17 4 12\"></polyline></svg>';
                    function syncBtn() {
                      if (!btn) return;
                      btn.classList.remove('confirmed');
                      btn.innerHTML = arrowIcon;
                      if (sz.items.length > 0) {
                        btn.classList.add('has-selection');
                      } else {
                        btn.classList.remove('has-selection');
                      }
                    }
                    // Initial state: if tables already selected, show as confirmed (green)
                    if (btn && sz.items.length > 0) {
                      btn.classList.add('has-selection');
                      btn.classList.add('confirmed');
                      btn.innerHTML = checkIcon;
                    }
                    if (sz.items.length === 0 && Object.keys(sz.options).length > 0) {
                      sz.open();
                    }
                    // User-driven changes: show blue (needs confirmation)
                    sz.on('change', syncBtn);
                    // Select all link
                    $('#%s').off('click.selall').on('click.selall', function(e) {
                      e.preventDefault();
                      sz.setValue(Object.keys(sz.options));
                    });
                  }, 100);
                });
                $('#%s').on('click', function() {
                  var sel = $('#%s')[0];
                  if (sel && sel.selectize && sel.selectize.items.length > 0) {
                    this.classList.add('confirmed');
                    this.innerHTML = '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2.5\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><polyline points=\"20 6 9 17 4 12\"></polyline></svg>';
                  }
                });
                ",
                ns("has_tables"),
                ns("table_select"),
                ns("load_data"),
                ns("select_all_tables"),
                ns("load_data"),
                ns("table_select")
              )))
            )
          )
        )
      )
    },
    class = "dm_read_block",
    allow_empty_state = TRUE,
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
#' @param input_type Character: "excel", "zip", "directory", "serialized", "rdata"
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
        obj <- if (tolower(tools::file_ext(path)) == "qs") {
          qs::qread(path)
        } else {
          readRDS(path)
        }
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
#' @return Character: "excel", "zip", "directory", "serialized", "rdata", or "unknown"
#' @noRd
detect_dm_input_type <- function(path) {
  if (dir.exists(path)) {
    return("directory")
  }

  ext <- tolower(tools::file_ext(path))

  switch(ext,
    xlsx = , xls = "excel",
    zip = "zip",
    rds = , qs = "serialized",
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
      table_names <- make.names(tools::file_path_sans_ext(basename(files)), unique = TRUE)
      if (!is.null(.(selected))) {
        keep <- table_names %in% .(selected)
        files <- files[keep]
        table_names <- table_names[keep]
      }

      # Read each file
      tables <- lapply(files, function(f) {
        ext <- tolower(tools::file_ext(f))
        if (ext %in% c("csv", "tsv")) {
          readr::read_csv(f, show_col_types = FALSE)
        } else if (ext %in% c("xlsx", "xls")) {
          readxl::read_excel(f)
        } else if (ext == "parquet") {
          arrow::read_parquet(f)
        } else if (ext == "feather") {
          arrow::read_feather(f)
        } else if (ext %in% c("rds")) {
          readRDS(f)
        } else if (ext == "rda") {
          e <- new.env()
          load(f, envir = e)
          as.list(e)[[1]]
        } else {
          rio::import(f)
        }
      })

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
      pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")$")
      files <- list.files(.(path), pattern = pattern, ignore.case = TRUE, full.names = TRUE)

      if (length(files) == 0) {
        stop("No data files found in directory")
      }

      # Get table names and filter to selected
      table_names <- make.names(tools::file_path_sans_ext(basename(files)), unique = TRUE)
      if (!is.null(.(selected))) {
        keep <- table_names %in% .(selected)
        files <- files[keep]
        table_names <- table_names[keep]
      }

      # Read each file
      tables <- lapply(files, function(f) {
        ext <- tolower(tools::file_ext(f))
        if (ext %in% c("csv", "tsv")) {
          readr::read_csv(f, show_col_types = FALSE)
        } else if (ext %in% c("xlsx", "xls")) {
          readxl::read_excel(f)
        } else if (ext == "parquet") {
          arrow::read_parquet(f)
        } else if (ext == "feather") {
          arrow::read_feather(f)
        } else if (ext %in% c("rds")) {
          readRDS(f)
        } else if (ext == "rda") {
          e <- new.env()
          load(f, envir = e)
          as.list(e)[[1]]
        } else {
          rio::import(f)
        }
      })

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
      obj <- if (tolower(tools::file_ext(.(path))) == "qs") {
        qs::qread(.(path))
      } else {
        readRDS(.(path))
      }

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
  DiagrammeR::renderGrViz({
    if (!inherits(result, "dm")) {
      return(NULL)
    }
    dm::dm_draw(result, view_type = "keys_only")
  })
}


#' Custom UI for dm_read_block
#' @inheritParams block_ui.dm_block
#' @method block_ui dm_read_block
#' @export
block_ui.dm_read_block <- function(id, x, ...) {
  shiny::tagList(
    DiagrammeR::grVizOutput(shiny::NS(id, "result"), height = "300px")
  )
}


#' @method block_render_trigger dm_read_block
#' @export
block_render_trigger.dm_read_block <- function(x, session = blockr.core::get_session()) {
  NULL
}
