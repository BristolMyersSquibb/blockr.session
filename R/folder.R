# board_folder ---------------------------------------------------------------

#' Create a folder-based storage backend
#'
#' @param path Path to the root directory for workflow storage.
#'
#' @export
board_folder <- function(path) {

  path <- normalizePath(path, mustWork = FALSE)

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  structure(
    list(path = path),
    class = "rack_board_folder"
  )
}

# rack_id_folder -------------------------------------------------------------

new_rack_id_folder <- function(name, version = NULL) {

  if (not_null(version) && (!is_string(version) || !nzchar(version))) {
    blockr_abort(
      "rack_id_folder version must be a non-empty string.",
      class = "rack_id_folder_invalid_version"
    )
  }

  new_rack_id(name, version = version, class = "rack_id_folder")
}

# pin_name -------------------------------------------------------------------

#' @export
pin_name.rack_id_folder <- function(id, ...) id$name

# display_name ---------------------------------------------------------------

#' @export
display_name.rack_id_folder <- function(id, ...) id$name

# format ---------------------------------------------------------------------

#' @export
format.rack_id_folder <- function(x, ...) {
  v <- if (not_null(x$version)) paste0("@", x$version) else ""
  paste0("<rack_id_folder: ", x$name, v, ">")
}

# helpers --------------------------------------------------------------------

folder_workflow_dir <- function(backend, name) {
  file.path(backend$path, name)
}

folder_version_files <- function(backend, name) {

  wf_dir <- folder_workflow_dir(backend, name)

  if (!dir.exists(wf_dir)) {
    return(character())
  }

  list.files(wf_dir, pattern = "\\.json$", full.names = TRUE)
}

folder_version_from_file <- function(path) {
  tools::file_path_sans_ext(basename(path))
}

folder_timestamp <- function() {
  format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
}

# last_saved -----------------------------------------------------------------

#' @export
last_saved.rack_id_folder <- function(id, backend, ...) {
  files <- folder_version_files(backend, id$name)

  if (length(files) == 0L) {
    return(NULL)
  }

  latest <- files[length(files)]
  file.info(latest)$mtime
}

# rack_list ------------------------------------------------------------------

#' @export
rack_list.rack_board_folder <- function(backend, ...) {

  dirs <- list.dirs(backend$path, full.names = TRUE, recursive = FALSE)

  if (length(dirs) == 0L) {
    return(list())
  }

  # For each subdirectory, find latest version file
  info <- lapply(dirs, function(d) {
    files <- list.files(d, pattern = "\\.json$", full.names = TRUE)
    if (length(files) == 0L) return(NULL)
    latest <- files[length(files)]
    list(
      name = basename(d),
      mtime = file.info(latest)$mtime
    )
  })

  info <- Filter(Negate(is.null), info)

  if (length(info) == 0L) {
    return(list())
  }

  # Sort by recency (most recent first)
  mtimes <- vapply(info, function(x) as.numeric(x$mtime), numeric(1L))
  info <- info[order(mtimes, decreasing = TRUE)]

  lapply(info, function(x) new_rack_id_folder(x$name))
}

# rack_info ------------------------------------------------------------------

#' @export
rack_info.rack_id_folder <- function(id, backend, ...) {

  files <- folder_version_files(backend, id$name)

  if (length(files) == 0L) {
    return(
      data.frame(
        version = character(),
        created = as.POSIXct(character()),
        hash = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  versions <- folder_version_from_file(files)
  created <- file.info(files)$mtime
  hashes <- unname(tools::md5sum(files))

  df <- data.frame(
    version = versions,
    created = created,
    hash = hashes,
    stringsAsFactors = FALSE
  )

  # Most recent first (timestamps are lexicographically sortable)
  df[order(df$version, decreasing = TRUE), , drop = FALSE]
}

# rack_load ------------------------------------------------------------------

#' @export
rack_load.rack_id_folder <- function(id, backend, ...) {

  version <- id$version

  if (is.null(version)) {
    info <- rack_info(id, backend)

    if (nrow(info) == 0L) {
      blockr_abort(
        "No versions found for workflow {id$name}.",
        class = "rack_load_no_versions"
      )
    }

    version <- info$version[1L]
  }

  wf_dir <- folder_workflow_dir(backend, id$name)
  json_path <- file.path(wf_dir, paste0(version, ".json"))

  if (!file.exists(json_path)) {
    blockr_abort(
      "Version {version} not found for workflow {id$name}.",
      class = "rack_load_version_not_found"
    )
  }

  jsonlite::fromJSON(
    json_path,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
}

# rack_save ------------------------------------------------------------------

#' @export
rack_save.rack_board_folder <- function(backend, data, ..., name) {

  wf_dir <- folder_workflow_dir(backend, name)

  if (!dir.exists(wf_dir)) {
    dir.create(wf_dir, recursive = TRUE)
  }

  version <- folder_timestamp()
  json_path <- file.path(wf_dir, paste0(version, ".json"))

  jsonlite::write_json(data, json_path, null = "null")

  new_rack_id_folder(name = name, version = version)
}

# rack_delete ----------------------------------------------------------------

#' @export
rack_delete.rack_id_folder <- function(id, backend, ...) {

  version <- id$version

  if (is.null(version)) {
    info <- rack_info(id, backend)

    if (nrow(info) == 0L) {
      blockr_abort(
        "No versions found for workflow {id$name}.",
        class = "rack_delete_no_versions"
      )
    }

    version <- info$version[1L]
  }

  wf_dir <- folder_workflow_dir(backend, id$name)
  json_path <- file.path(wf_dir, paste0(version, ".json"))

  if (file.exists(json_path)) {
    unlink(json_path)
  }

  invisible(TRUE)
}

# rack_purge -----------------------------------------------------------------

#' @export
rack_purge.rack_id_folder <- function(id, backend, ...) {
  wf_dir <- folder_workflow_dir(backend, id$name)
  unlink(wf_dir, recursive = TRUE)
  invisible(TRUE)
}

# rack_file ------------------------------------------------------------------

#' Get the local file path for a workflow version
#'
#' @param id A rack ID.
#' @param backend A storage backend.
#' @param ... Additional arguments.
#'
#' @keywords internal
rack_file <- function(id, backend, ...) UseMethod("rack_file")

#' @export
rack_file.rack_id_folder <- function(id, backend, ...) {

  version <- id$version

  if (is.null(version)) {
    info <- rack_info(id, backend)

    if (nrow(info) == 0L) {
      blockr_abort(
        "No versions found for workflow {id$name}.",
        class = "rack_file_no_versions"
      )
    }

    version <- info$version[1L]
  }

  wf_dir <- folder_workflow_dir(backend, id$name)
  file.path(wf_dir, paste0(version, ".json"))
}

#' @export
rack_file.rack_id_pins <- function(id, backend, ...) {
  pins::pin_download(backend, pin_name(id))
}
