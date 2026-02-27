upload_blockr_json <- function(board, data, name, ...) {

  tmp <- withr::local_tempfile(
    fileext = ".json",
    .local_envir = parent.frame()
  )

  jsonlite::write_json(data, tmp, null = "null")

  pins::pin_upload(board, tmp, name, ...)
}

blockr_test_session <- list(
  blocks = list(
    a = list(type = "dataset_block"),
    b = list(type = "filter_block")
  ),
  links = list(),
  format = "test"
)
