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
#' @param source Either "upload" for file upload widget or "path" for file browser.
#'   Default: "upload".
#' @param selected_tables Character vector. Optional subset of tables to include
#'   in the output dm. Default is NULL (all tables).
#' @param ... Forwarded to [blockr.core::new_data_block()]
#'
#' @section Configuration:
#' The following settings are retrieved from options and not stored in block state:
#' - **volumes**: File browser mount points. Set via `options(blockr.volumes = c(name = "path"))`
#'   or environment variable `BLOCKR_VOLUMES`. Default: `c(temp = tempdir())`
#' - **upload_path**: Directory for persistent file storage. Set via
#'   `options(blockr.upload_path = "/path")` or environment variable `BLOCKR_UPLOAD_PATH`.
#'   Default: `rappdirs::user_data_dir("blockr")`
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
#' @importFrom rappdirs user_data_dir
#' @importFrom bslib navset_pill nav_panel
#' @export
new_dm_read_block <- function(
    path = character(),
    source = "upload",
    selected_tables = NULL,
    ...
) {
  # Validate parameters
  source <- match.arg(source, c("upload", "path"))

  # Get volumes and upload_path from options (runtime configuration)
  volumes <- blockr_option("volumes", c(temp = tempdir()))
  upload_path <- blockr_option("upload_path", rappdirs::user_data_dir("blockr"))

  # Handle volumes parameter
  if (is.character(volumes)) {
    volumes <- path.expand(volumes)
  }

  if (is_string(volumes) && grepl(":", volumes)) {
    volumes <- strsplit(volumes, ":", fixed = TRUE)[[1L]]
  }

  if (is.null(names(volumes))) {
    if (length(volumes) == 1L) {
      names(volumes) <- "volume"
    } else if (length(volumes) > 1L) {
      names(volumes) <- paste0("volume", seq_along(volumes))
    }
  }

  # Expand and validate upload path
  upload_path <- path.expand(upload_path)

  blockr.core::new_data_block(
    server = function(id) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Reactive values for state
          r_source <- shiny::reactiveVal(source)

          # Path storage
          if (length(path) > 0 && file.exists(path)) {
            initial_path <- stats::setNames(path, basename(path))
          } else {
            initial_path <- character()
          }
          r_path <- shiny::reactiveVal(initial_path)
          r_file_path <- shiny::reactiveVal(initial_path)

          # Detected input type: "excel", "zip", "directory", "serialized", "rdata"
          initial_type <- if (length(path) > 0) detect_dm_input_type(path) else "unknown"
          r_input_type <- shiny::reactiveVal(initial_type)

          # Selected tables (NULL = all)
          r_selected_tables <- shiny::reactiveVal(selected_tables)

          # Initialize shinyFiles browser for files
          shinyFiles::shinyFileChoose(
            input,
            "file_browser",
            roots = volumes,
            session = session,
            filetypes = c("xlsx", "xls", "zip", "rds", "qs", "rdata", "rda")
          )

          # Initialize shinyFiles browser for directories
          shinyFiles::shinyDirChoose(
            input,
            "dir_browser",
            roots = volumes,
            session = session
          )

          # Handle file browser selection
          selected_file <- shiny::reactive({
            if (!is.null(input$file_browser) && !identical(input$file_browser, "")) {
              shinyFiles::parseFilePaths(volumes, input$file_browser)$datapath
            } else {
              character()
            }
          })

          shiny::observeEvent(selected_file(), {
            if (length(selected_file()) > 0) {
              selected_path <- stats::setNames(
                selected_file(),
                basename(selected_file())
              )
              r_path(selected_path)
              r_file_path(selected_path)
              r_input_type(detect_dm_input_type(selected_file()[1]))
              r_source("path")
            }
          })

          # Handle directory browser selection
          selected_dir <- shiny::reactive({
            if (!is.null(input$dir_browser) && !identical(input$dir_browser, "")) {
              path_result <- shinyFiles::parseDirPath(volumes, input$dir_browser)
              if (length(path_result) > 0) path_result else character()
            } else {
              character()
            }
          })

          shiny::observeEvent(selected_dir(), {
            if (length(selected_dir()) > 0) {
              selected_path <- stats::setNames(
                selected_dir(),
                basename(selected_dir())
              )
              r_path(selected_path)
              r_file_path(selected_path)
              r_input_type("directory")
              r_source("path")
            }
          })

          # Handle file upload with persistence
          shiny::observeEvent(input$file_upload, {
            shiny::req(input$file_upload)

            # Create upload directory if it doesn't exist
            upload_dir <- upload_path
            dir.create(upload_dir, recursive = TRUE, showWarnings = FALSE)

            # Process uploaded file
            temp_path <- input$file_upload$datapath
            original_name <- input$file_upload$name

            # Generate unique filename with timestamp
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S_%OS3")
            safe_name <- gsub("[^A-Za-z0-9._-]", "_", original_name)
            permanent_path <- file.path(upload_dir, paste0(timestamp, "_", safe_name))

            # Copy file to permanent storage
            file.copy(temp_path, permanent_path, overwrite = FALSE)

            names(permanent_path) <- original_name

            r_path(permanent_path)
            r_file_path(permanent_path)
            r_input_type(detect_dm_input_type(permanent_path))
            r_source("upload")
          })

          # File info for display
          output$file_info <- shiny::renderText({
            current_path <- r_file_path()
            if (length(current_path) == 0) {
              return("No file or directory selected")
            }

            file_name <- names(current_path)[1]
            if (is.null(file_name) || file_name == "") {
              file_name <- basename(current_path[1])
            }

            input_type <- r_input_type()
            type_label <- switch(input_type,
              "excel" = "Excel file",
              "zip" = "ZIP archive",
              "directory" = "Directory",
              "serialized" = "Serialized file",
              "rdata" = "RData file",
              "Unknown"
            )

            paste("Selected:", file_name, paste0("(", type_label, ")"))
          })

          # Preview info
          output$preview_info <- shiny::renderText({
            current_path <- r_file_path()
            if (length(current_path) == 0) return("")

            input_type <- r_input_type()
            path_val <- current_path[1]

            tryCatch({
              if (input_type == "excel") {
                sheets <- readxl::excel_sheets(path_val)
                paste("Found", length(sheets), "sheets:", paste(sheets, collapse = ", "))
              } else if (input_type == "zip") {
                files <- utils::unzip(path_val, list = TRUE)$Name
                data_files <- files[grepl("\\.(csv|xlsx?|parquet|feather|rds)$", files, ignore.case = TRUE)]
                paste("Found", length(data_files), "data files in archive")
              } else if (input_type == "directory") {
                files <- list_data_files(path_val)
                paste("Found", length(files), "data files in directory")
              } else if (input_type == "serialized") {
                obj <- if (tolower(tools::file_ext(path_val)) == "qs") {
                  qs::qread(path_val)
                } else {
                  readRDS(path_val)
                }
                if (inherits(obj, "dm")) {
                  paste("Found dm with", length(names(obj)), "tables:",
                        paste(names(obj), collapse = ", "))
                } else if (inherits(obj, "data.frame")) {
                  paste("Found single data.frame with", ncol(obj), "columns")
                } else if (is.list(obj)) {
                  are_dfs <- vapply(obj, inherits, logical(1), "data.frame")
                  paste("Found list with", sum(are_dfs), "data.frames")
                } else {
                  "Unknown content type"
                }
              } else if (input_type == "rdata") {
                env <- new.env()
                load(path_val, envir = env)
                objs <- as.list(env)
                are_dfs <- vapply(objs, inherits, logical(1), "data.frame")
                df_names <- names(objs)[are_dfs]
                paste("Found", sum(are_dfs), "data.frames:",
                      paste(df_names, collapse = ", "))
              } else {
                ""
              }
            }, error = function(e) {
              paste("Error reading:", e$message)
            })
          })

          # Pre-read data for table discovery
          dm_data <- shiny::reactive({
            current_path <- r_file_path()
            shiny::req(length(current_path) > 0)
            input_type <- r_input_type()
            path_val <- current_path[1]

            base_expr <- dm_read_expr(path_val, input_type, selected = NULL)
            tryCatch(eval(base_expr), error = function(e) NULL)
          })

          # Available tables from the loaded dm
          available_tables <- shiny::reactive({
            dm_obj <- dm_data()
            shiny::req(inherits(dm_obj, "dm"))
            names(dm_obj)
          })

          # Output for conditional panel
          output$has_tables <- shiny::reactive({
            length(available_tables()) > 0
          })
          shiny::outputOptions(output, "has_tables", suspendWhenHidden = FALSE)

          # Update table selection UI when tables change
          shiny::observeEvent(available_tables(), {
            tables <- available_tables()
            current_selected <- r_selected_tables()
            # If no selection yet, select all
            if (is.null(current_selected)) {
              current_selected <- tables
            } else {
              # Keep only tables that still exist
              current_selected <- intersect(current_selected, tables)
            }
            shiny::updateSelectInput(
              session, "table_select",
              choices = tables,
              selected = current_selected
            )
          })

          shiny::observeEvent(input$table_select, {
            r_selected_tables(input$table_select)
          }, ignoreInit = TRUE)

          list(
            expr = shiny::reactive({
              current_path <- r_file_path()
              shiny::req(length(current_path) > 0)

              input_type <- r_input_type()
              path_val <- current_path[1]
              selected <- r_selected_tables()
              shiny::req(!is.null(selected))

              dm_read_expr(path_val, input_type, selected)
            }),
            state = list(
              path = r_path,
              source = r_source,
              selected_tables = r_selected_tables
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        block_responsive_css(),
        shiny::div(
          class = "block-container dm-read-block-container",
          shiny::tags$style(shiny::HTML("
            .nav-pills {
              display: inline-flex;
              overflow: hidden;
            }
            .nav-pills .nav-link {
              background-color: rgb(249, 249, 250);
              color: rgb(104, 107, 130);
              border: none;
              border-radius: 8px;
              margin: 8px;
              margin-left: 0;
              padding: 6px 10px;
              font-size: 0.8rem;
            }
            .nav-pills .nav-link:hover {
              background-color: #f8f9fa;
              z-index: 1;
            }
            .nav-pills .nav-link.active {
              background-color: rgb(236, 236, 236);
              color: rgb(104, 107, 130);
              border-color: rgb(236, 236, 236);
              z-index: 2;
            }
            .dm-read-block-container .shiny-input-container {
              width: 100% !important;
            }
          ")),

          shiny::div(
            class = "block-section",
            shiny::tags$h4("Source", class = "mb-3"),
            shiny::div(
              class = "mb-3",
              bslib::navset_pill(
                id = ns("source_pills"),
                selected = source,
                bslib::nav_panel(
                  title = "From Browser",
                  value = "upload",
                  shiny::div(
                    class = "block-input-wrapper mt-3",
                    shiny::tags$h4("Upload data file", class = "mb-2"),
                    shiny::div(
                      class = "block-help-text mb-3",
                      "Excel/ZIP/RDS/QS/RData: each table/sheet becomes a dm table."
                    ),
                    shiny::fileInput(
                      inputId = ns("file_upload"),
                      label = NULL,
                      multiple = FALSE,
                      accept = c(".xlsx", ".xls", ".zip", ".rds", ".qs", ".rdata", ".rda")
                    )
                  )
                ),
                bslib::nav_panel(
                  title = "From Server",
                  value = "path",
                  shiny::div(
                    class = "block-input-wrapper mt-3",
                    shiny::tags$h4("Select file or directory", class = "mb-2"),
                    shiny::div(
                      class = "block-help-text mb-3",
                      "Pick a data file (Excel/ZIP/RDS/QS/RData) or a directory."
                    ),
                    shiny::div(
                      class = "d-flex gap-2",
                      shinyFiles::shinyFilesButton(
                        ns("file_browser"),
                        label = "Browse Files...",
                        title = "Select Excel or ZIP file",
                        multiple = FALSE
                      ),
                      shinyFiles::shinyDirButton(
                        ns("dir_browser"),
                        label = "Browse Directory...",
                        title = "Select directory with data files"
                      )
                    )
                  )
                )
              )
            )
          ),

          shiny::div(
            class = "block-form-grid",
            shiny::div(
              class = "block-section",
              shiny::tags$h4("File Information", class = "mt-3"),
              shiny::div(
                class = "block-help-text",
                shiny::textOutput(ns("file_info"))
              ),
              shiny::div(
                class = "block-help-text",
                shiny::textOutput(ns("preview_info"))
              )
            ),

            # Advanced Options Toggle
            css_advanced_toggle(ns("advanced-options"), use_subgrid = TRUE),
            shiny::div(
              class = "block-section",
              shiny::div(
                class = "block-advanced-toggle text-muted",
                id = ns("advanced-toggle"),
                onclick = sprintf(
                  "
                  const section = document.getElementById('%s');
                  const chevron = document.querySelector('#%s .block-chevron');
                  section.classList.toggle('expanded');
                  chevron.classList.toggle('rotated');
                  ",
                  ns("advanced-options"),
                  ns("advanced-toggle")
                ),
                shiny::tags$span(class = "block-chevron", "\u203A"),
                "Advanced Options"
              )
            ),

            # Advanced Options Content (collapsed by default)
            shiny::div(
              id = ns("advanced-options"),
              shiny::conditionalPanel(
                condition = "output.has_tables",
                ns = ns,
                shiny::div(
                  class = "block-section",
                  shiny::tags$h4("Select Tables"),
                  shiny::div(
                    class = "block-input-wrapper",
                    shiny::selectInput(
                      ns("table_select"),
                      label = NULL,
                      choices = character(),
                      selected = character(),
                      multiple = TRUE,
                      selectize = TRUE
                    )
                  )
                )
              )
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
  extensions <- c("csv", "tsv", "xlsx", "xls", "parquet", "feather", "rds", "rda")
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
      extensions <- c("csv", "tsv", "xlsx", "xls", "parquet", "feather", "rds", "rda")
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
      extensions <- c("csv", "tsv", "xlsx", "xls", "parquet", "feather", "rds", "rda")
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


#' @noRd
is_string <- function(x) {
  is.character(x) && length(x) == 1L
}

#' @noRd
blockr_option <- function(name, default = NULL) {
  opt_name <- paste0("blockr.", name)
  getOption(opt_name, default)
}
