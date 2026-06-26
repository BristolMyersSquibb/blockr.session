prepare_download <- function(sel, backend) {
  if (length(sel) == 1L) {
    id <- rack_id_from_input(backend, sel[[1]])
    return(rack_download(id, backend))
  }

  tmp_dir <- tempfile("wf_download_")
  dir.create(tmp_dir)

  for (s in sel) {
    id <- tryCatch(
      rack_id_from_input(backend, s),
      error = function(e) NULL
    )
    if (is.null(id)) next

    path <- tryCatch(
      rack_download(id, backend),
      error = function(e) NULL
    )
    if (is.null(path)) next

    file.copy(path, file.path(tmp_dir, paste0(s$name, ".json")))
  }

  zip_path <- tempfile(fileext = ".zip")
  zip::zip(zip_path, files = list.files(tmp_dir, full.names = TRUE),
           mode = "cherry-pick")

  zip_path
}

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

    data <- try(jsonlite::fromJSON(fpath, simplifyDataFrame = FALSE,
                                   simplifyMatrix = FALSE), silent = TRUE)
    board <- try(blockr_deser(data), silent = TRUE)

    if (!inherits(board, "board")) {
      errors <- c(errors, paste("Skipped", fname, "- not a valid board"))
      next
    }

    if (!is_string(data$id) || !nzchar(data$id)) {
      errors <- c(errors, paste("Skipped", fname, "- no board id in payload"))
      next
    }

    tryCatch(
      {
        rid <- rack_id_from_input(backend, list(id = data$id))
        rack_upload(backend, fpath, rid, name = wf_name)
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
