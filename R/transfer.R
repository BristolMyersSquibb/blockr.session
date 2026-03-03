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

#' Upload workflow files to the backend
#'
#' Processes normalized file entries (from the browser file reader) and pins
#' each workflow JSON to the backend.
#'
#' @param files Normalized file list — each element a
#'   `list(name = ..., datapath = ...)`.
#' @param backend A pins board.
#'
#' @return `list(ok, uploaded, errors)` where `ok` is logical, `uploaded` is
#'   the count of successfully uploaded workflows, and `errors` is a character
#'   vector of per-file error messages.
#'
#' @keywords internal
upload_workflows <- function(files, backend) {
  uploaded <- 0L
  errors <- character()

  for (f in files) {
    fname <- f$name

    if (grepl("\\.zip$", fname, ignore.case = TRUE)) {
      res <- upload_zip(f, backend)
      uploaded <- uploaded + res$uploaded
      errors <- c(errors, res$errors)
    } else if (grepl("\\.json$", fname, ignore.case = TRUE)) {
      res <- upload_json(f, backend)
      uploaded <- uploaded + res$uploaded
      errors <- c(errors, res$errors)
    } else {
      errors <- c(errors, paste("Skipped unsupported file:", fname))
    }
  }

  list(ok = uploaded > 0L, uploaded = uploaded, errors = errors)
}

upload_zip <- function(f, backend) {
  uploaded <- 0L
  errors <- character()

  raw_data <- sub("^data:[^;]*;base64,", "", f$datapath)
  zip_bytes <- base64enc::base64decode(raw_data)
  zip_path <- tempfile(fileext = ".zip")
  writeBin(zip_bytes, zip_path)
  tmp_dir <- tempfile("wf_upload_")
  dir.create(tmp_dir)
  zip::unzip(zip_path, exdir = tmp_dir)

  json_files <- list.files(tmp_dir, pattern = "\\.json$",
                           full.names = TRUE, recursive = TRUE)

  for (jf in json_files) {
    wf_name <- tools::file_path_sans_ext(basename(jf))
    tryCatch({
      pins::pin_upload(backend, paths = jf, name = wf_name,
                       versioned = TRUE, metadata = list(format = "v1"),
                       tags = blockr_session_tags())
      uploaded <- uploaded + 1L
    }, error = function(e) {
      errors <<- c(errors, paste("Failed to upload:", wf_name))
    })
  }

  list(uploaded = uploaded, errors = errors)
}

upload_json <- function(f, backend) {
  uploaded <- 0L
  errors <- character()

  wf_name <- tools::file_path_sans_ext(f$name)
  tmp_path <- tempfile(fileext = ".json")
  writeLines(f$datapath, tmp_path)

  tryCatch({
    pins::pin_upload(backend, paths = tmp_path, name = wf_name,
                     versioned = TRUE, metadata = list(format = "v1"),
                     tags = blockr_session_tags())
    uploaded <- 1L
  }, error = function(e) {
    errors <- c(errors, paste("Failed to upload:", wf_name))
  })

  list(uploaded = uploaded, errors = errors)
}
