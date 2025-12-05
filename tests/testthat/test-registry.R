test_that("register_dm_blocks function exists and runs", {
 # Just test that the function can be called without error
 expect_no_error(register_dm_blocks())
})

test_that("dm blocks are registered after calling register_dm_blocks", {
 # Force registration
 register_dm_blocks()

 # Get registered blocks
 registry <- blockr.core::available_blocks()

 # Check dm blocks are in the registry
 # Note: The registry uses constructor names with "new_" prefix stripped sometimes
 block_names <- names(registry)

 # Check that our blocks exist (may be with or without "new_" prefix)
 dm_found <- any(grepl("dm_block", block_names))
 expect_true(dm_found, info = paste("Available blocks:", paste(block_names, collapse = ", ")))
})
