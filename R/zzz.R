.onLoad <- function(libname, pkgname) {
  register_dm_blocks()
  vctrs::s3_register("blockr.assistant::describe_result", "dm")
  invisible(NULL)
}
