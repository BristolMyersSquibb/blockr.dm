# aaa- prefix so it collates first: the *_dep() builders below call memoise0()
# at package-load time to build their wrappers, so it must already be defined.

#' Run a nullary builder at most once per process, caching the result in a
#' private environment. Base-R memoise (no dependency, no `<<-`): the closure
#' captures a mutable env and mutates it in place, so nothing rebinds an
#' enclosing frame. The `exists()` guard (not `is.null`) caches a genuine `NULL`
#' result too.
#'
#' Used for the block htmlDependency builders: each takes no data-dependent args
#' and returns the same immutable object for the life of the process, yet was
#' re-running disk I/O (packageVersion -> read.dcf + system.file) on every block
#' construction/render. htmlDependency objects are immutable value lists
#' htmltools already shares across sessions, so one cached copy per process
#' shared across Shiny sessions is correct. Mirrors blockr.viz / blockr.dplyr.
#' @noRd
memoise0 <- function(build) {
  force(build)
  cache <- new.env(parent = emptyenv())
  function() {
    if (!exists("v", cache, inherits = FALSE)) cache$v <- build()
    cache$v
  }
}
