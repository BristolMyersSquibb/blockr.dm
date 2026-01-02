#' Write dm object to files
#'
#' A block for writing dm (data model) objects to files. Supports writing to
#' Excel files (each table as a sheet), ZIP archives (containing multiple files),
#' or directories (one file per table).
#'
#' @param directory Character. Default directory for file output (browse mode only).
#'   Can be configured via `options(blockr.write_dir = "/path")`. Default: `tempdir()`.
#' @param filename Character. Optional fixed filename (without extension).
#'   - **If provided**: Writes to same file path on every upstream change
#'   - **If empty** (default): Generates timestamped filename
#' @param format Character. Output format: "excel", "csv", or "parquet".
#'   Default: "excel" (best for multi-table dm objects).
#' @param mode Character. Either "download" (triggers browser download),
#'   or "browse" (writes to server filesystem). Default: "download"
#' @param auto_write Logical. When TRUE, automatically writes files when data changes
#'   (browse mode only). Default: FALSE.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @details
#' ## Output Formats
#'
#' **Excel (.xlsx):**
#' - Single Excel file with multiple sheets
#' - Each dm table becomes a sheet
#' - Sheet names derived from table names
#'
#' **CSV/Parquet (in ZIP):**
#' - Single ZIP file containing individual files
#' - Each dm table becomes a separate file
#' - Filenames derived from table names
#'
#' **Directory:**
#' - Only available in browse mode
#' - Each dm table written as separate file
#' - Files named from table names
#'
#' @return A blockr transform block that writes dm objects to files
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dm)
#'   serve(new_dm_write_block())
#' }
#'
#' @export
new_dm_write_block <- function(
    directory = "",
    filename = "",
    format = "excel",
    mode = "download",
    auto_write = FALSE,
    ...
) {
  # Validate parameters
  format <- match.arg(format, c("excel", "csv", "parquet"))
  mode <- match.arg(mode, c("browse", "download"))

  # Get default directory from options
  if (directory == "") {
    directory <- blockr_option("write_dir", tempdir())
  }

  # Get volumes for directory browser
  volumes <- blockr_option("volumes", c(temp = tempdir()))

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

  directory <- path.expand(directory)

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Reactive values for state
          r_directory <- shiny::reactiveVal(directory)
          r_filename <- shiny::reactiveVal(filename)
          r_format <- shiny::reactiveVal(format)
          r_mode <- shiny::reactiveVal(mode)
          r_auto_write <- shiny::reactiveVal(auto_write)
          r_write_status <- shiny::reactiveVal("")

          # Initialize shinyFiles directory browser
          shinyFiles::shinyDirChoose(
            input,
            "dir_browser",
            roots = volumes,
            session = session
          )

          # Handle directory browser selection
          selected_dir <- shiny::reactive({
            if (!is.null(input$dir_browser) && !identical(input$dir_browser, "")) {
              path <- shinyFiles::parseDirPath(volumes, input$dir_browser)
              if (length(path) > 0) path else NULL
            } else {
              NULL
            }
          })

          shiny::observeEvent(selected_dir(), {
            if (!is.null(selected_dir())) {
              r_directory(selected_dir())
            }
          })

          # Update state from inputs
          shiny::observeEvent(input$mode_pills, {
            if (!is.null(input$mode_pills)) {
              r_mode(input$mode_pills)
            }
          })

          shiny::observeEvent(input$auto_write, r_auto_write(input$auto_write))
          shiny::observeEvent(input$filename, r_filename(input$filename))
          shiny::observeEvent(input$format, r_format(input$format))

          # Reactive for write expression (set when submit clicked)
          r_write_expression_set <- shiny::reactiveVal(NULL)

          # Directory creation
          shiny::observeEvent(r_directory(), {
            shiny::req(r_directory())
            tryCatch({
              dir.create(r_directory(), recursive = TRUE, showWarnings = FALSE)
            }, error = function(e) {
              r_write_status(sprintf("\u2717 Directory error: %s", conditionMessage(e)))
            })
          })

          # Submit button handler for browse mode
          shiny::observeEvent(input$submit_write, {
            shiny::req(data)
            shiny::req(r_directory())
            shiny::req(r_mode() == "browse")
            shiny::req(!r_auto_write())

            expr <- dm_write_expr(
              directory = r_directory(),
              filename = r_filename(),
              format = r_format()
            )

            r_write_expression_set(bquote({
              .(expr)
              data
            }))

            base_filename <- generate_dm_filename(r_filename())
            ext <- switch(r_format(),
              "excel" = ".xlsx",
              "csv" = ".zip",
              "parquet" = ".zip"
            )
            full_path <- file.path(r_directory(), paste0(base_filename, ext))
            timestamp <- format(Sys.time(), "%H:%M:%S")
            r_write_status(sprintf("\u2713 Saved to %s at %s", full_path, timestamp))
          })

          # Generate expression based on mode
          r_write_expression <- shiny::reactive({
            if (r_mode() == "browse") {
              if (r_auto_write()) {
                shiny::req(data)
                shiny::req(r_directory())

                expr <- dm_write_expr(
                  directory = r_directory(),
                  filename = r_filename(),
                  format = r_format()
                )

                bquote({
                  .(expr)
                  data
                })
              } else {
                r_write_expression_set()
              }
            } else {
              # Download mode - just pass data through
              quote(data)
            }
          })

          # Download handler
          output$download_data <- shiny::downloadHandler(
            filename = function() {
              base <- generate_dm_filename(r_filename())
              ext <- switch(r_format(),
                excel = ".xlsx",
                csv = ".zip",
                parquet = ".zip"
              )
              paste0(base, ext)
            },
            content = function(file) {
              dm_obj <- data()
              shiny::req(inherits(dm_obj, "dm"))

              tables <- dm::dm_get_tables(dm_obj)
              table_names <- names(tables)

              if (r_format() == "excel") {
                # Write as Excel with multiple sheets
                table_list <- lapply(table_names, function(nm) tables[[nm]])
                names(table_list) <- table_names
                writexl::write_xlsx(table_list, file)
              } else {
                # Write as ZIP with multiple files
                temp_dir <- tempfile("dm_write_")
                dir.create(temp_dir, showWarnings = FALSE)
                on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

                ext <- if (r_format() == "csv") ".csv" else ".parquet"
                files_to_zip <- character()

                for (nm in table_names) {
                  file_path <- file.path(temp_dir, paste0(nm, ext))
                  if (r_format() == "csv") {
                    readr::write_csv(tables[[nm]], file_path)
                  } else {
                    arrow::write_parquet(tables[[nm]], file_path)
                  }
                  files_to_zip <- c(files_to_zip, paste0(nm, ext))
                }

                zip::zip(file, files = files_to_zip, root = temp_dir, mode = "cherry-pick")
              }
            }
          )

          # Output: Current directory display
          output$current_directory <- shiny::renderText({
            dir <- r_directory()
            if (!is.null(dir) && nzchar(dir)) {
              paste("Current directory:", dir)
            } else {
              "No directory selected"
            }
          })

          # Output: Write status
          output$write_status <- shiny::renderText({
            r_write_status()
          })

          # Output: Table info
          output$table_info <- shiny::renderText({
            dm_obj <- data()
            if (!inherits(dm_obj, "dm")) {
              return("Waiting for dm input...")
            }
            tables <- dm::dm_get_tables(dm_obj)
            paste("Tables to write:", paste(names(tables), collapse = ", "))
          })

          list(
            expr = r_write_expression,
            state = list(
              directory = r_directory,
              filename = r_filename,
              format = r_format,
              mode = r_mode,
              auto_write = r_auto_write
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
          class = "block-container dm-write-block-container",
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
            }
            .nav-pills .nav-link.active {
              background-color: rgb(236, 236, 236);
              color: rgb(104, 107, 130);
            }
            .dm-write-block-container .shiny-input-container {
              width: 100% !important;
            }
          ")),

          # Mode selector
          shiny::div(
            class = "block-section",
            shiny::tags$h4("Output Mode", class = "mb-3"),
            bslib::navset_pill(
              id = ns("mode_pills"),
              selected = mode,
              bslib::nav_panel(
                title = "To Browser",
                value = "download",
                shiny::div(
                  class = "mt-3",
                  shiny::tags$h4("Export dm to your computer", class = "mb-2"),
                  shiny::div(
                    class = "block-help-text mb-3",
                    "Downloads Excel (multi-sheet) or ZIP (multiple files)."
                  ),
                  shiny::downloadButton(
                    ns("download_data"),
                    "Download",
                    class = "btn-outline-secondary"
                  )
                )
              ),
              bslib::nav_panel(
                title = "To Server",
                value = "browse",
                shiny::div(
                  class = "mt-3",
                  shiny::tags$h4("Save dm to the server", class = "mb-2"),
                  shiny::div(
                    class = "block-help-text mb-3",
                    "When running locally, this is your computer."
                  ),
                  shinyFiles::shinyDirButton(
                    ns("dir_browser"),
                    label = "Select Directory...",
                    title = "Choose output directory",
                    multiple = FALSE,
                    class = "btn-outline-secondary"
                  ),
                  shiny::div(
                    class = "block-help-text mt-2",
                    shiny::textOutput(ns("current_directory"))
                  ),
                  shiny::div(
                    class = "mt-3",
                    shiny::checkboxInput(
                      ns("auto_write"),
                      "Auto-write: automatically save when data changes",
                      value = auto_write
                    )
                  ),
                  shiny::conditionalPanel(
                    condition = "!input.auto_write",
                    ns = ns,
                    shiny::div(
                      class = "mt-2",
                      shiny::actionButton(
                        ns("submit_write"),
                        "Save to File",
                        class = "btn-outline-secondary"
                      )
                    )
                  )
                )
              )
            )
          ),

          # File Configuration
          shiny::div(
            class = "block-form-grid",
            shiny::div(
              class = "block-section",
              shiny::tags$h4("File Configuration", class = "mt-3"),
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::textInput(
                    inputId = ns("filename"),
                    label = "Filename (optional)",
                    value = filename,
                    placeholder = "Leave empty for auto-timestamp"
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    inputId = ns("format"),
                    label = "Format",
                    choices = c(
                      "Excel (multi-sheet)" = "excel",
                      "CSV (in ZIP)" = "csv",
                      "Parquet (in ZIP)" = "parquet"
                    ),
                    selected = format
                  )
                )
              )
            ),

            # Info and status
            shiny::div(
              class = "block-section",
              shiny::div(
                class = "block-help-text",
                shiny::textOutput(ns("table_info"))
              ),
              shiny::div(
                class = "block-help-text",
                shiny::textOutput(ns("write_status"))
              )
            )
          )
        )
      )
    },
    dat_valid = function(data) {
      if (!inherits(data, "dm")) {
        stop("Input must be a dm object")
      }
    },
    allow_empty_state = TRUE,
    class = "dm_write_block",
    ...
  )
}


#' Generate filename with optional timestamp
#' @noRd
generate_dm_filename <- function(filename, timestamp = Sys.time()) {
  if (nzchar(filename)) {
    filename
  } else {
    paste0("dm_", format(timestamp, "%Y%m%d_%H%M%S"))
  }
}


#' Build expression to write dm to files
#' @noRd
dm_write_expr <- function(directory, filename, format) {
  base_filename <- if (nzchar(filename)) {
    filename
  } else {
    bquote(paste0("dm_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  }

  if (format == "excel") {
    bquote(
      local({
        dm_obj <- data
        tables <- dm::dm_get_tables(dm_obj)
        table_names <- names(tables)
        table_list <- lapply(table_names, function(nm) tables[[nm]])
        names(table_list) <- table_names
        file_path <- file.path(.(directory), paste0(.(base_filename), ".xlsx"))
        writexl::write_xlsx(table_list, file_path)
        file_path
      })
    )
  } else {
    # CSV or Parquet in ZIP
    ext <- if (format == "csv") ".csv" else ".parquet"
    write_fn <- if (format == "csv") quote(readr::write_csv) else quote(arrow::write_parquet)

    bquote(
      local({
        dm_obj <- data
        tables <- dm::dm_get_tables(dm_obj)
        table_names <- names(tables)

        temp_dir <- tempfile("dm_write_")
        dir.create(temp_dir, showWarnings = FALSE)
        on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

        files_to_zip <- character()
        for (nm in table_names) {
          file_name <- paste0(nm, .(ext))
          file_path <- file.path(temp_dir, file_name)
          .(write_fn)(tables[[nm]], file_path)
          files_to_zip <- c(files_to_zip, file_name)
        }

        zip_path <- file.path(.(directory), paste0(.(base_filename), ".zip"))
        zip::zip(zip_path, files = files_to_zip, root = temp_dir, mode = "cherry-pick")
        zip_path
      })
    )
  }
}


#' @method block_output dm_write_block
#' @export
block_output.dm_write_block <- function(x, result, session) {
  # Write block passes through the dm, display it
  DiagrammeR::renderGrViz({
    if (!inherits(result, "dm")) {
      return(NULL)
    }
    dm::dm_draw(result, view_type = "keys_only")
  })
}


#' @method block_ui dm_write_block
#' @export
block_ui.dm_write_block <- function(id, x, ...) {
  shiny::tagList(
    DiagrammeR::grVizOutput(shiny::NS(id, "result"), height = "300px")
  )
}


#' @method block_render_trigger dm_write_block
#' @export
block_render_trigger.dm_write_block <- function(x, session = blockr.core::get_session()) {
  NULL
}
