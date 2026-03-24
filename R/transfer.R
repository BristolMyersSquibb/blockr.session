transfer_ok <- function(path, content_type, filename) {
  list(ok = TRUE, path = path, content_type = content_type,
       filename = filename, error = NULL)
}

transfer_error <- function(msg) {
  list(ok = FALSE, path = NULL, content_type = NULL,
       filename = NULL, error = msg)
}

#' Prepare workflow files for download
#'
#' Fetches pinned workflow JSON(s) from the backend and returns a structured
#' result pointing to the file (single workflow) or a ZIP archive (multiple).
#'
#' @param sel Normalized selection list — each element a
#'   `list(name = ..., user = ...)`.
#' @param backend A pins board.
#'
#' @return A `transfer_result` list with `$ok`, `$path`, `$content_type`,
#'   `$filename`, `$error`.
#'
#' @keywords internal
prepare_download <- function(sel, backend) {
  if (length(sel) == 1L) {
    id <- rack_id_from_input(sel[[1]], backend)
    paths <- pins::pin_download(backend, pin_name(id))
    json_path <- paths[grepl("\\.json$", paths)][1]

    if (is.na(json_path)) {
      return(transfer_error("No JSON file found for workflow"))
    }

    fname <- paste0(sel[[1]]$name, ".json")
    return(transfer_ok(json_path, "application/json", fname))
  }

  tmp_dir <- tempfile("wf_download_")
  dir.create(tmp_dir)

  for (s in sel) {
    id <- tryCatch(
      rack_id_from_input(s, backend),
      error = function(e) NULL
    )
    if (is.null(id)) next

    paths <- tryCatch(
      pins::pin_download(backend, pin_name(id)),
      error = function(e) NULL
    )
    if (is.null(paths)) next

    json_path <- paths[grepl("\\.json$", paths)][1]
    if (!is.na(json_path)) {
      file.copy(json_path, file.path(tmp_dir, paste0(s$name, ".json")))
    }
  }

  zip_path <- tempfile(fileext = ".zip")
  zip::zip(zip_path, files = list.files(tmp_dir, full.names = TRUE),
           mode = "cherry-pick")

  transfer_ok(zip_path, "application/zip", "workflows.zip")
}
