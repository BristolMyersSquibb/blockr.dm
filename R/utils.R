#' Generate responsive CSS for blockr blocks
#'
#' Creates CSS for responsive grid layout using 'block-' prefix.
#' Can be reused across different blockr packages.
#'
#' @return HTML style tag with responsive CSS
#' @noRd
block_responsive_css <- function() {
  shiny::tags$style(shiny::HTML(
    "
    .block-container {
      width: 100%;
      margin: 0px;
      padding: 0px;
      padding-bottom: 15px;
    }

    /* One shared grid across the whole form */
    .block-form-grid {
      display: grid;
      gap: 15px;
      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
      align-items: start;
    }

    /* Flatten wrappers so all controls share the same tracks */
    .block-section,
    .block-section-grid {
      display: contents;
    }

    /* Headings/help span full width */
    .block-section h4,
    .block-help-text {
      grid-column: 1 / -1;
    }

    .block-section h4 {
      margin-top: 5px;
      margin-bottom: 0px;
      font-size: 1.1rem;
      font-weight: 600;
      color: #333;
    }

    .block-section:not(:first-child) {
      margin-top: 20px;
    }

    .block-input-wrapper {
      width: 100%;
    }

    .block-input-wrapper .form-group {
      margin-bottom: 10px;
    }

    .block-help-text {
      margin-top: 0px;
      padding-top: 0px;
      font-size: 0.875rem;
      color: #666;
    }
    "
  ))
}

#' Generate container query script for responsive blocks
#'
#' Sets up container queries if supported by the browser.
#'
#' @return HTML script tag
#' @noRd
block_container_script <- function() {
  shiny::tags$script(shiny::HTML(
    "
    // Set up container queries if supported
    if ('container' in document.documentElement.style) {
      var container = document.querySelector('.block-container');
      if (container) container.style.containerType = 'inline-size';
    }
    "
  ))
}


#' CSS for collapsible advanced options section
#'
#' Provides standardized CSS for expandable/collapsible sections with
#' animated chevron indicator.
#'
#' @param id Character string, the namespaced ID for the advanced options div.
#' @param use_subgrid Logical, whether to use CSS subgrid for better grid integration.
#' @return HTML style tag with advanced toggle CSS
#' @noRd
css_advanced_toggle <- function(id, use_subgrid = FALSE) {
  subgrid_css <- if (use_subgrid) {
    "
    grid-column: 1 / -1;
    display: grid;
    grid-template-columns: subgrid;
    gap: 15px;
    "
  } else {
    ""
  }

  shiny::tags$style(shiny::HTML(sprintf(
    "
    #%s {
      max-height: 0;
      overflow: hidden;
      transition: max-height 0.3s ease-out;
      %s
    }
    #%s.expanded {
      max-height: 500px;
      overflow: visible;
      transition: max-height 0.5s ease-in;
    }
    .block-advanced-toggle {
      cursor: pointer;
      user-select: none;
      padding: 8px 0;
      margin-bottom: 8px;
      display: flex;
      align-items: center;
      gap: 6px;
      color: #6c757d;
      font-size: 0.875rem;
    }
    .block-chevron {
      transition: transform 0.2s;
      display: inline-block;
      font-size: 14px;
      font-weight: bold;
    }
    .block-chevron.rotated {
      transform: rotate(90deg);
    }
    ",
    id,
    subgrid_css,
    id
  )))
}
