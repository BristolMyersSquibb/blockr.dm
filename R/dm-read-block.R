#' Read multiple files into a dm object
#'
#' A block for reading multiple data files into a dm (data model) object.
#' Supports reading from Excel files (each sheet becomes a table), ZIP archives
#' (containing multiple data files), or directories (reading all data files).
#'
#' @param path Character. Path to file or directory. Can be:
#'   - An Excel file (.xlsx, .xls): Each sheet becomes a dm table
#'   - A ZIP file (.zip): Extracted files become dm tables
#'   - A directory path: All data files in directory become dm tables
#' @param source Either "upload" for file upload widget or "path" for file browser.
#'   Default: "upload".
#' @param infer_keys Logical, whether to automatically infer primary and foreign
#'   key relationships from columns with matching names. Default is `TRUE`.
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
    infer_keys = TRUE,
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
          r_infer_keys <- shiny::reactiveVal(infer_keys)

          # Path storage
          if (length(path) > 0 && file.exists(path)) {
            initial_path <- stats::setNames(path, basename(path))
          } else {
            initial_path <- character()
          }
          r_path <- shiny::reactiveVal(initial_path)
          r_file_path <- shiny::reactiveVal(initial_path)

          # Detected input type: "excel", "zip", "directory"
          initial_type <- if (length(path) > 0) detect_dm_input_type(path) else "unknown"
          r_input_type <- shiny::reactiveVal(initial_type)

          # Update infer_keys from UI
          shiny::observeEvent(input$infer_keys, {
            r_infer_keys(input$infer_keys)
          }, ignoreInit = TRUE)

          # Initialize shinyFiles browser for files
          shinyFiles::shinyFileChoose(
            input,
            "file_browser",
            roots = volumes,
            session = session,
            filetypes = c("xlsx", "xls", "zip")
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
              } else {
                ""
              }
            }, error = function(e) {
              paste("Error reading:", e$message)
            })
          })

          # Pre-read data for key analysis
          dm_data <- shiny::reactive({
            current_path <- r_file_path()
            shiny::req(length(current_path) > 0)
            input_type <- r_input_type()
            path_val <- current_path[1]

            base_expr <- dm_read_expr(path_val, input_type)
            tryCatch(eval(base_expr), error = function(e) NULL)
          })

          # Analyze keys from the data
          key_analysis <- shiny::reactive({
            dm_obj <- dm_data()
            shiny::req(inherits(dm_obj, "dm"))

            if (!r_infer_keys()) {
              return(list(pks = list(), fks = list()))
            }

            # Find keys using the helper function
            analyze_dm_keys(dm_obj)
          })

          list(
            expr = shiny::reactive({
              current_path <- r_file_path()
              shiny::req(length(current_path) > 0)

              input_type <- r_input_type()
              path_val <- current_path[1]

              # Build base dm expression
              base_expr <- dm_read_expr(path_val, input_type)

              keys <- key_analysis()

              if (length(keys$pks) == 0 && length(keys$fks) == 0) {
                return(base_expr)
              }

              # Build expression with hardcoded key additions
              expr_parts <- list(bquote(dm_obj <- .(base_expr)))

              # Add PK expressions with actual table/column names
              for (pk in keys$pks) {
                table_sym <- as.name(pk$table)
                col_sym <- as.name(pk$column)
                expr_parts <- c(expr_parts, list(
                  bquote(dm_obj <- dm::dm_add_pk(dm_obj, .(table_sym), .(col_sym)))
                ))
              }

              # Add FK expressions with actual table/column names
              for (fk in keys$fks) {
                child_sym <- as.name(fk$child_table)
                col_sym <- as.name(fk$column)
                parent_sym <- as.name(fk$parent_table)
                expr_parts <- c(expr_parts, list(
                  bquote(dm_obj <- dm::dm_add_fk(dm_obj, .(child_sym), .(col_sym), .(parent_sym)))
                ))
              }

              # Return dm_obj
              expr_parts <- c(expr_parts, list(quote(dm_obj)))

              # Combine into local({ ... })
              as.call(c(quote(local), list(as.call(c(quote(`{`), expr_parts)))))
            }),
            state = list(
              path = r_path,
              source = r_source,
              infer_keys = r_infer_keys
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
                    shiny::tags$h4("Upload Excel or ZIP file", class = "mb-2"),
                    shiny::div(
                      class = "block-help-text mb-3",
                      "Excel: each sheet becomes a table. ZIP: all data files become tables."
                    ),
                    shiny::fileInput(
                      inputId = ns("file_upload"),
                      label = NULL,
                      multiple = FALSE,
                      accept = c(".xlsx", ".xls", ".zip")
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
                      "Pick an Excel/ZIP file or a directory containing data files."
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
            shiny::div(
              class = "block-section mt-3",
              shiny::checkboxInput(
                ns("infer_keys"),
                "Infer relationships from column names",
                value = infer_keys
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


#' Analyze dm object for potential PK/FK relationships
#'
#' Returns structured info about keys to add, which can then be
#' hardcoded into the expression.
#'
#' @param dm_obj A dm object
#' @return List with `pks` (list of list(table, column)) and `fks` (list of list(child_table, column, parent_table))
#' @noRd
analyze_dm_keys <- function(dm_obj) {
  pks <- list()
  fks <- list()


  table_names <- names(dm_obj)
  if (length(table_names) < 2) {
    return(list(pks = pks, fks = fks))
  }

  # Get all column names for each table
  all_cols <- lapply(table_names, function(tbl) names(dm_obj[[tbl]]))
  names(all_cols) <- table_names

  # Find columns that appear in multiple tables
  col_counts <- table(unlist(all_cols))
  shared_cols <- names(col_counts[col_counts > 1])

  if (length(shared_cols) == 0) {
    return(list(pks = pks, fks = fks))
  }

  existing_pks <- dm::dm_get_all_pks(dm_obj)
  existing_fks <- dm::dm_get_all_fks(dm_obj)

  for (col in shared_cols) {
    tables_with_col <- table_names[vapply(all_cols, function(cols) col %in% cols, logical(1))]
    if (length(tables_with_col) < 2) next

    # Check uniqueness
    uniqueness <- vapply(tables_with_col, function(tbl) {
      vals <- dm_obj[[tbl]][[col]]
      !anyNA(vals) && !anyDuplicated(vals)
    }, logical(1))

    pk_tables <- tables_with_col[uniqueness]
    fk_tables <- tables_with_col[!uniqueness]

    # If at least one table has unique values and others don't, establish relationship
    # When multiple PK candidates exist, pick the first one
    if (length(pk_tables) >= 1 && length(fk_tables) >= 1) {
      pk_table <- pk_tables[1]

      # Add PK if not already set
      if (!(pk_table %in% existing_pks$table)) {
        pks <- c(pks, list(list(table = pk_table, column = col)))
      }

      # Add FKs
      for (fk_table in fk_tables) {
        has_fk <- any(
          existing_fks$child_table == fk_table &
            existing_fks$parent_table == pk_table
        )
        if (!has_fk) {
          fks <- c(fks, list(list(
            child_table = fk_table,
            column = col,
            parent_table = pk_table
          )))
        }
      }
    }
  }

  list(pks = pks, fks = fks)
}


#' Detect dm input type from path
#' @param path Path to file or directory
#' @return Character: "excel", "zip", "directory", or "unknown"
#' @noRd
detect_dm_input_type <- function(path) {
  if (dir.exists(path)) {
    return("directory")
  }

  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls")) {
    return("excel")
  }

  if (ext == "zip") {
    return("zip")
  }

  "unknown"
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
dm_read_expr <- function(path, input_type) {
  if (input_type == "excel") {
    dm_read_expr_excel(path)
  } else if (input_type == "zip") {
    dm_read_expr_zip(path)
  } else if (input_type == "directory") {
    dm_read_expr_directory(path)
  } else {
    stop("Unknown input type: ", input_type)
  }
}


#' Read Excel file - each sheet becomes a table
#' @noRd
dm_read_expr_excel <- function(path) {
  bquote(
    local({
      sheets <- readxl::excel_sheets(.(path))
      tables <- lapply(sheets, function(sheet) {
        readxl::read_excel(.(path), sheet = sheet)
      })
      names(tables) <- make.names(sheets, unique = TRUE)
      do.call(dm::dm, tables)
    })
  )
}


#' Read ZIP file - extract and read all data files
#' @noRd
dm_read_expr_zip <- function(path) {
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

      # Name tables from filenames (without extension)
      names(tables) <- make.names(tools::file_path_sans_ext(basename(files)), unique = TRUE)
      do.call(dm::dm, tables)
    })
  )
}


#' Read directory - read all data files
#' @noRd
dm_read_expr_directory <- function(path) {
  bquote(
    local({
      # Find all data files
      extensions <- c("csv", "tsv", "xlsx", "xls", "parquet", "feather", "rds", "rda")
      pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")$")
      files <- list.files(.(path), pattern = pattern, ignore.case = TRUE, full.names = TRUE)

      if (length(files) == 0) {
        stop("No data files found in directory")
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

      # Name tables from filenames (without extension)
      names(tables) <- make.names(tools::file_path_sans_ext(basename(files)), unique = TRUE)
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
