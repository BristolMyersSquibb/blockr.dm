#' Write dm object to files
#'
#' A block for writing dm (data model) objects to files. Supports writing to
#' Excel files (each table as a sheet), ZIP archives (containing multiple files),
#' or directories (one file per table).
#'
#' @param directory Character. Default directory for file output.
#'   Can be configured via `options(blockr.write_dir = "/path")`. Default: `""`.
#' @param filename Character. Optional fixed filename (without extension).
#'   - **If provided**: Writes to same file path on every upstream change
#'   - **If empty** (default): Generates timestamped filename
#' @param format Character. Output format: "excel", "csv", or "parquet".
#'   Default: "excel" (best for multi-table dm objects).
#' @param auto_write Logical. When TRUE, automatically writes files when data changes
#'   (requires a non-empty directory). Default: FALSE.
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
#' **CSV/Parquet (server save):**
#' - Individual files written into a subdirectory
#' - Subdirectory named after the base filename
#' - Each dm table becomes a separate file
#'
#' **CSV/Parquet (download):**
#' - Single ZIP archive containing individual files
#' - Each dm table becomes a separate file
#'
#' ## Download vs Server Save
#'
#' Both options are always available in a flat layout:
#'
#' **Download to Browser:**
#' - Always available via the download button
#' - Triggers a download to your browser's download folder
#'
#' **Save to Server:**
#' - Active when a server directory path is set (non-empty)
#' - User enters a directory path in the path input
#' - Manual or auto mode via segmented toggle
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
#' @importFrom shinyjs useShinyjs
#' @export
new_dm_write_block <- function(
    directory = "",
    filename = "",
    format = "excel",
    auto_write = FALSE,
    ...
) {
  # Validate parameters
  format <- match.arg(format, c("excel", "csv", "parquet"))

  # Expand directory path if non-empty
  if (nzchar(directory)) {
    directory <- path.expand(directory)
  }

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Reactive values for state
          r_directory <- shiny::reactiveVal(directory)
          r_filename <- shiny::reactiveVal(filename)
          r_format <- shiny::reactiveVal(format)
          r_auto_write <- shiny::reactiveVal(auto_write)
          r_write_status <- shiny::reactiveVal("")

          # Data directory from board options
          data_dir_reactive <- shiny::reactive({
            blockr.core::coal(
              blockr.core::get_board_option_or_null("data_dir", session),
              ""
            )
          })

          # Path input module for directory selection
          dir_path <- blockr.io::path_input_server(
            "dir_path",
            data_dir = data_dir_reactive,
            mode = "directory"
          )

          # Populate path text input on restore / init
          if (nzchar(directory)) {
            shiny::observe({
              display_path <- directory
              dd <- data_dir_reactive()
              if (nzchar(dd)) {
                prefix <- paste0(dd, "/")
                if (startsWith(directory, prefix)) {
                  display_path <- substr(
                    directory, nchar(prefix) + 1, nchar(directory)
                  )
                }
              }
              session$sendCustomMessage("blockr-path-set-value", list(
                id = session$ns("dir_path-path_text"),
                value = display_path,
                silent = TRUE
              ))
            }) |> shiny::bindEvent(TRUE, once = TRUE)
          }

          # Handle directory path changes
          shiny::observeEvent(dir_path(), {
            path_val <- dir_path()
            shiny::req(nzchar(path_val))

            # Resolve relative paths against data directory
            resolved <- path_val
            data_dir <- data_dir_reactive()
            if (
              nzchar(data_dir) &&
              !grepl("^(/|~|[A-Za-z]:)", path_val)
            ) {
              resolved <- file.path(data_dir, path_val)
            }

            r_directory(resolved)
          }, ignoreInit = TRUE)

          # Update state from inputs
          shiny::observeEvent(input$write_mode, {
            r_auto_write(identical(input$write_mode, "auto"))
          })

          shiny::observeEvent(input$filename, r_filename(input$filename))
          shiny::observeEvent(input$format, r_format(input$format))

          # Reactive to store the write expression (set when submit clicked)
          r_write_expression_set <- shiny::reactiveVal(NULL)

          # Track whether directory existed before we created it
          r_dir_existed <- shiny::reactiveVal(
            nzchar(directory) && dir.exists(path.expand(directory))
          )

          # Directory creation
          shiny::observeEvent(r_directory(), {
            shiny::req(r_directory())
            existed <- dir.exists(r_directory())
            r_dir_existed(existed)
            if (!existed) {
              tryCatch({
                dir.create(r_directory(), recursive = TRUE, showWarnings = FALSE)
                if (!dir.exists(r_directory())) {
                  r_write_status(sprintf(
                    "\u2717 Cannot create directory: %s", r_directory()
                  ))
                }
              }, error = function(e) {
                r_write_status(sprintf(
                  "\u2717 Directory error: %s", conditionMessage(e)
                ))
              })
            }
          })

          # Submit button handler for server save
          shiny::observeEvent(input$submit_write, {
            shiny::req(data)
            shiny::req(nzchar(r_directory()))
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
            if (r_format() == "excel") {
              full_path <- file.path(
                r_directory(), paste0(base_filename, ".xlsx")
              )
            } else {
              full_path <- file.path(r_directory(), base_filename)
            }
            timestamp <- format(Sys.time(), "%H:%M:%S")
            r_write_status(sprintf(
              "\u2713 Saved to %s at %s", full_path, timestamp
            ))
          })

          # Generate expression based on directory state and auto_write
          r_write_expression <- shiny::reactive({
            if (nzchar(r_directory())) {
              if (r_auto_write()) {
                shiny::req(data)

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
              # No directory set - download handler handles writing
              NULL
            }
          })

          # Update status when auto-write generates a new expression
          shiny::observe({
            shiny::req(nzchar(r_directory()))
            shiny::req(r_auto_write())
            shiny::req(r_write_expression())

            # Depend on data to trigger when it changes
            data()

            base_filename <- generate_dm_filename(r_filename())
            if (r_format() == "excel") {
              full_path <- file.path(
                r_directory(), paste0(base_filename, ".xlsx")
              )
            } else {
              full_path <- file.path(r_directory(), base_filename)
            }
            timestamp <- format(Sys.time(), "%H:%M:%S")
            r_write_status(sprintf(
              "\u2713 Saved to %s at %s", full_path, timestamp
            ))
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

                zip::zip(
                  file,
                  files = files_to_zip,
                  root = temp_dir,
                  mode = "cherry-pick"
                )
              }
            }
          )

          # Status badge for directory validation
          shiny::observe({
            dir <- r_directory()
            existed <- r_dir_existed()
            if (nzchar(dir) && existed) {
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("dir_path-path_text"),
                text = "Directory",
                state = "success"
              ))
            } else if (nzchar(dir)) {
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("dir_path-path_text"),
                text = "New directory",
                state = "info"
              ))
            } else {
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("dir_path-path_text"),
                text = "",
                state = "none"
              ))
            }
          })

          # Output: Write status
          output$write_status <- shiny::renderText({
            r_write_status()
          })

          list(
            expr = r_write_expression,
            state = list(
              directory = r_directory,
              filename = r_filename,
              format = r_format,
              auto_write = r_auto_write
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
          class = "block-container dm-write-block-container",

          shiny::tags$style(shiny::HTML("
            /* Make inputs full width */
            .dm-write-block-container .shiny-input-container {
              width: 100% !important;
            }
            .dm-write-block-container .selectize-control {
              width: 100% !important;
            }
            /* Tighten spacing in file config grid */
            .dm-write-block-container .block-form-grid .shiny-input-container {
              margin-bottom: 0;
            }
            .dm-write-block-container .block-help-text {
              margin-top: 8px;
            }
            /* Execution mode toggle */
            .blockr-exec-toggle {
              display: inline-flex;
              align-items: center;
              gap: 2px;
              background-color: #f3f4f6;
              border-radius: 8px;
              padding: 2px;
            }
            .blockr-exec-toggle button {
              padding: 0.25rem 0.5rem;
              font-size: 0.875rem;
              line-height: 1.5;
              font-weight: 500;
              color: #6b7280;
              background: transparent;
              border: none;
              border-radius: 6px;
              cursor: pointer;
              transition: all 0.15s ease;
              white-space: nowrap;
            }
            .blockr-exec-toggle button:hover {
              color: #374151;
              background-color: #e5e7eb;
            }
            .blockr-exec-toggle button.active {
              color: #111827;
              background-color: #fff;
              box-shadow: 0 1px 2px rgb(0 0 0 / 0.06);
            }
            /* Auto-save info banner */
            .blockr-exec-auto-hint {
              font-size: 0.8rem;
              color: #0d6efd;
              background: #e7f1ff;
              border: 1px solid #b6d4fe;
              border-radius: 6px;
              padding: 8px 12px;
            }
            /* Status text */
            .blockr-exec-status {
              font-size: 0.8rem;
              color: #6b7280;
              min-height: 1.2em;
            }
            /* OR divider */
            .blockr-or-divider {
              display: flex;
              align-items: center;
              gap: 12px;
              margin: 16px 0;
            }
            .blockr-or-divider::before,
            .blockr-or-divider::after {
              content: '';
              flex: 1;
              border-top: 1px solid #e5e7eb;
            }
            .blockr-or-divider span {
              font-size: 0.75rem;
              font-weight: 500;
              color: #9ca3af;
              text-transform: uppercase;
              letter-spacing: 0.05em;
            }
          ")),

          # Hidden input to track mode
          shiny::div(
            style = "display:none;",
            shiny::textInput(
              ns("write_mode"),
              label = NULL,
              value = if (auto_write) "auto" else "manual"
            )
          ),

          # --- File Configuration ---
          shiny::div(
            class = "block-form-grid",
            style = "padding-bottom: 0; margin-bottom: 0;",
            shiny::div(
              class = "block-section",
              shiny::tags$h4("File Configuration"),
              shiny::div(
                class = "block-section-grid",
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::textInput(
                    inputId = ns("filename"),
                    label = "Filename (optional)",
                    value = filename,
                    placeholder = "Leave empty for auto-timestamp"
                  ),
                  shiny::div(
                    class = "block-help-text",
                    style = "font-size: 0.75rem;",
                    "Fixed filename overwrites on each change.",
                    "Empty generates unique timestamped files."
                  )
                ),
                shiny::div(
                  class = "block-input-wrapper",
                  shiny::selectInput(
                    inputId = ns("format"),
                    label = "Format",
                    choices = c(
                      "Excel" = "excel",
                      "CSV" = "csv",
                      "Parquet" = "parquet"
                    ),
                    selected = format
                  )
                )
              )
            )
          ),

          # --- Separator ---
          shiny::tags$hr(
            style = "border-top: 1px solid #e5e7eb; margin: 16px 0;"
          ),

          # --- Download to Browser ---
          shiny::div(
            class = "block-section",
            shiny::tags$h4("Download to Browser", class = "mb-3"),
            shiny::tags$p(
              class = "blockr-path-hint",
              "Download directly without saving to server"
            ),
            shiny::downloadButton(
              ns("download_data"),
              "Download",
              class = "btn-outline-secondary btn-sm"
            )
          ),

          # --- OR divider ---
          shiny::div(
            class = "blockr-or-divider",
            shiny::tags$span("or")
          ),

          # --- Save to Server ---
          shiny::div(
            class = "block-section blockr-file-location",
            shiny::tags$h4("Save to Server", class = "mb-3"),
            shiny::tags$p(
              class = "blockr-path-hint",
              "Choose a server path to save"
            ),
            blockr.io::path_input_ui(shiny::NS(id, "dir_path")),
            # Mode toggle + save button row
            shiny::div(
              class = "mt-2",
              style = "display: flex; align-items: center; gap: 8px;",
              shiny::div(
                class = "blockr-exec-toggle",
                shiny::tags$button(
                  "Manual",
                  class = if (!auto_write) "active" else "",
                  onclick = sprintf(
                    "
                    document.getElementById('%s').value = 'manual';
                    document.getElementById('%s').dispatchEvent(new Event('change'));
                    this.classList.add('active');
                    this.nextElementSibling.classList.remove('active');
                    ",
                    ns("write_mode"), ns("write_mode")
                  )
                ),
                shiny::tags$button(
                  "Auto",
                  class = if (auto_write) "active" else "",
                  onclick = sprintf(
                    "
                    document.getElementById('%s').value = 'auto';
                    document.getElementById('%s').dispatchEvent(new Event('change'));
                    this.classList.add('active');
                    this.previousElementSibling.classList.remove('active');
                    ",
                    ns("write_mode"), ns("write_mode")
                  )
                )
              ),
              shiny::conditionalPanel(
                condition = "input.write_mode === 'manual'",
                ns = shiny::NS(id),
                shiny::actionButton(
                  ns("submit_write"),
                  "Save to Server",
                  class = "btn-primary btn-sm"
                )
              )
            ),
            # Auto-save info box
            shiny::conditionalPanel(
              condition = "input.write_mode === 'auto'",
              ns = shiny::NS(id),
              shiny::div(
                class = "blockr-exec-auto-hint mt-2",
                "Auto-save enabled. File updates automatically on data changes."
              )
            ),
            # Status message
            shiny::div(
              class = "blockr-exec-status mt-2",
              shiny::textOutput(ns("write_status"))
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
    # CSV or Parquet as individual files in a subdirectory
    ext <- if (format == "csv") ".csv" else ".parquet"
    write_fn <- if (format == "csv") quote(readr::write_csv) else quote(arrow::write_parquet)

    bquote(
      local({
        dm_obj <- data
        tables <- dm::dm_get_tables(dm_obj)
        table_names <- names(tables)

        out_dir <- file.path(.(directory), .(base_filename))
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

        for (nm in table_names) {
          file_path <- file.path(out_dir, paste0(nm, .(ext)))
          .(write_fn)(tables[[nm]], file_path)
        }

        out_dir
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
