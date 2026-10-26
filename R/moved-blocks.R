# Blocks that moved to blockr.pharma. A saved board names a block's
# constructor by package and function, so boards saved before the move still
# look for these here. They hand over to blockr.pharma, and the block they
# return records the blockr.pharma constructor, so the next save points there.

new_cdisc_dm_block <- function(...) {
  forward_moved_block("new_cdisc_dm_block", ...)
}

# Calls `blockr.pharma::<fun>(...)` as a call, not through do.call() on the
# function: blockr.core reads the package and name of a block's constructor
# off the call that made it.
forward_moved_block <- function(fun, ...) {
  rlang::check_installed("blockr.pharma",
                         reason = paste0("for `", fun, "()`."))
  args <- list(...)
  args[c("ctor", "ctor_pkg")] <- NULL
  call <- rlang::call2(rlang::call2("::", quote(blockr.pharma), rlang::sym(fun)),
                       !!!args)
  eval(call)
}
