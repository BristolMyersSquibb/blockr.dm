# The dm read/write blocks enforce the deployment file-access policy provided
# by blockr.io (blockr.verify_read_path / blockr.verify_write_path).
#
# Paths must live OUTSIDE tempdir(): the blocks exempt the app sandbox
# (tempdir / upload_path) so uploads and URL downloads keep working, so a
# policed path has to be elsewhere. dirname(tempdir()) (e.g. /tmp) is not under
# tempdir() (e.g. /tmp/RtmpXXXX).

test_that("dm read block honors blockr.verify_read_path", {

  base <- file.path(dirname(tempdir()), "blockr_dm_fp_ok")
  dir_ok <- file.path(base, "study")
  dir.create(dir_ok, recursive = TRUE)
  rds <- file.path(dir_ok, "tables.rds")
  saveRDS(list(a = data.frame(x = 1:2), b = data.frame(y = 3:4)), rds)
  on.exit(unlink(base, recursive = TRUE))

  # Allowed: path inside the permitted root reads the dm.
  withr::local_options(blockr.verify_read_path = blockr.io::within_dirs(dir_ok))
  block <- new_dm_read_block(path = rds, selected_tables = c("a", "b"))
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "dm")
      expect_setequal(names(dm::dm_get_tables(result)), c("a", "b"))
    },
    args = list(x = block, data = list())
  )
})

test_that("dm read block blocks a path outside the allowed root", {

  base <- file.path(dirname(tempdir()), "blockr_dm_fp_block")
  dir_ok <- file.path(base, "study")
  dir.create(dir_ok, recursive = TRUE)
  rds <- file.path(dir_ok, "tables.rds")
  saveRDS(list(a = data.frame(x = 1:2)), rds)
  on.exit(unlink(base, recursive = TRUE))

  # Verifier allows only an unrelated dir -> restored path is rejected, no read.
  withr::local_options(
    blockr.verify_read_path = blockr.io::within_dirs(file.path(base, "elsewhere"))
  )
  block <- new_dm_read_block(path = rds, selected_tables = "a")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_null(session$returned$result())
    },
    args = list(x = block, data = list())
  )
})
