#' HTML dependency for the dm-table-picker JS handler
#'
#' Ships a small IIFE that registers the `dm-table-picker` Shiny custom
#' message handler. The handler mounts `Blockr.Select` (from blockr.dplyr)
#' in single or multi mode and wires it to a Shiny input.
#'
#' Loading this dependency does *not* pull in the `Blockr.Select` assets —
#' consumer UI must also include `blockr.dplyr::blockr_select_dep()`.
#'
#' @return An `htmltools::htmlDependency`.
#' @keywords internal
dm_table_picker_dep <- function() {
  htmltools::htmlDependency(
    name = "dm-table-picker-js",
    version = utils::packageVersion("blockr.dm"),
    src = system.file("js", package = "blockr.dm"),
    script = "dm-table-picker.js"
  )
}

#' Convenience: all assets needed for a dm table picker
#'
#' Bundles [blockr.dplyr::blockr_select_dep()] (component + shared CSS) with
#' the dm-table-picker handler. Use this in the UI of any dm block that
#' renders a table picker.
#'
#' @return An `htmltools::tagList` of `htmlDependency` objects.
#' @keywords internal
dm_table_picker_deps <- function() {
  htmltools::tagList(
    blockr.dplyr::blockr_core_js_dep(),
    blockr.dplyr::blockr_blocks_css_dep(),
    blockr.dplyr::blockr_select_dep(),
    dm_table_picker_dep()
  )
}
