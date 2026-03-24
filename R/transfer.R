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
    data <- rack_load(id, backend)
    json_path <- tempfile(fileext = ".json")
    jsonlite::write_json(data, json_path, null = "null")

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

    data <- tryCatch(
      rack_load(id, backend),
      error = function(e) NULL
    )
    if (is.null(data)) next

    jsonlite::write_json(
      data,
      file.path(tmp_dir, paste0(s$name, ".json")),
      null = "null"
    )
  }

  zip_path <- tempfile(fileext = ".zip")
  zip::zip(zip_path, files = list.files(tmp_dir, full.names = TRUE),
           mode = "cherry-pick")

  transfer_ok(zip_path, "application/zip", "workflows.zip")
}

#' Upload workflow JSON files to the backend
#'
#' Processes files from [shiny::fileInput()] and pins each workflow JSON
#' to the backend.
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

    if (!grepl("\\.json$", fname, ignore.case = TRUE)) {
      errors <- c(errors, paste("Skipped non-JSON file:", fname))
      next
    }

    wf_name <- sub("\\.[Jj][Ss][Oo][Nn]$", "", fname)

    tryCatch(
      {
        data <- jsonlite::fromJSON(
          fpath,
          simplifyDataFrame = FALSE,
          simplifyMatrix = FALSE
        )
        rack_save(backend, data, name = wf_name)
        uploaded <- uploaded + 1L
      },
      error = function(e) {
        errors <<- c(errors, paste("Failed to upload", wf_name,
                                   "-", conditionMessage(e)))
      }
    )
  }

  list(ok = uploaded > 0L, uploaded = uploaded, errors = errors)
}
