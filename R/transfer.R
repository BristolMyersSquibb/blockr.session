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
#' Processes files from [shiny::fileInput()] and pins each workflow JSON
#' to the backend.  Accepts `.json` files directly and `.zip` archives
#' that contain only `.json` files.
#'
#' @param file_info Data frame returned by [shiny::fileInput()] with
#'   columns `name`, `size`, `type`, `datapath`.
#' @param backend A pins board.
#'
#' @return `list(ok, uploaded, errors)`.
#'
#' @keywords internal
upload_workflows <- function(file_info, backend) {
  uploaded <- 0L
  errors <- character()

  for (i in seq_len(nrow(file_info))) {
    fname <- file_info$name[i]
    fpath <- file_info$datapath[i]

    if (grepl("\\.zip$", fname, ignore.case = TRUE)) {
      res <- upload_zip(fpath, backend)
      uploaded <- uploaded + res$uploaded
      errors <- c(errors, res$errors)
    } else if (grepl("\\.json$", fname, ignore.case = TRUE)) {
      wf_name <- tools::file_path_sans_ext(fname)
      res <- upload_single_json(fpath, wf_name, backend)
      uploaded <- uploaded + res$uploaded
      errors <- c(errors, res$errors)
    } else {
      errors <- c(errors, paste("Skipped unsupported file:", fname))
    }
  }

  list(ok = uploaded > 0L, uploaded = uploaded, errors = errors)
}

upload_single_json <- function(path, name, backend) {
  tryCatch(
    {
      name <- sanitize_pin_name(name)
      pins::pin_upload(
        backend, path, name,
        versioned = TRUE,
        metadata = list(format = "v1"),
        tags = blockr_session_tags()
      )
      list(uploaded = 1L, errors = character())
    },
    error = function(e) {
      list(
        uploaded = 0L,
        errors = paste("Failed to upload", name, "-",
                       conditionMessage(e))
      )
    }
  )
}

upload_zip <- function(path, backend) {
  uploaded <- 0L
  errors <- character()

  tmp_dir <- tempfile("wf_upload_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  zip::unzip(path, exdir = tmp_dir)

  all_files <- list.files(tmp_dir, recursive = TRUE)
  non_json <- all_files[!grepl("\\.json$", all_files, ignore.case = TRUE)]

  if (length(non_json) > 0L) {
    return(list(
      uploaded = 0L,
      errors = paste(
        "ZIP contains non-JSON files:",
        paste(non_json, collapse = ", ")
      )
    ))
  }

  json_files <- list.files(tmp_dir, pattern = "\\.json$",
                           full.names = TRUE, recursive = TRUE)

  for (jf in json_files) {
    wf_name <- tools::file_path_sans_ext(basename(jf))
    res <- upload_single_json(jf, wf_name, backend)
    uploaded <- uploaded + res$uploaded
    errors <- c(errors, res$errors)
  }

  list(uploaded = uploaded, errors = errors)
}
