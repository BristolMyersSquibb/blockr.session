# new_rack_id / new_rack_id_pins ------------------------------------------------

new_rack_id <- function(name, ..., class = character()) {

  if (!is_string(name) || !nzchar(name)) {
    blockr_abort(
      "rack id name must be a non-empty string, got {class(name)[[1L]]}.",
      class = "rack_id_invalid_name"
    )
  }

  structure(list(name = name, ...), class = c(class, "rack_id"))
}

new_rack_id_pins <- function(name, version = NULL) {

  if (not_null(version) && (!is_string(version) || !nzchar(version))) {
    blockr_abort(
      "rack_id_pins version must be a non-empty string.",
      class = "rack_id_pins_invalid_version"
    )
  }

  new_rack_id(name, version = version, class = "rack_id_pins")
}

# Accessor generics ------------------------------------------------------------

display_name <- function(id, ...) UseMethod("display_name")

#' @export
display_name.rack_id <- function(id, ...) id$name

last_saved <- function(id, ...) UseMethod("last_saved")

#' @export
last_saved.rack_id_pins <- function(id, backend, ...) {
  info <- rack_info(id, backend)
  if (nrow(info) == 0L) return(NULL)
  info$created[1L]
}

#' @export
format.rack_id_pins <- function(x, ...) {
  v <- if (not_null(x$version)) paste0("@", x$version) else ""
  paste0("<rack_id_pins: ", x$name, v, ">")
}

#' @export
format.rack_id <- function(x, ...) {
  paste0("<rack_id: ", x$name, ">")
}

#' @export
print.rack_id <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

# rack_list --------------------------------------------------------------------

rack_list <- function(backend, ...) UseMethod("rack_list")

#' @export
rack_list.pins_board <- function(backend, ...) {

  df <- pins::pin_search(backend)

  if (nrow(df) == 0L) {
    return(list())
  }

  keep <- lgl_ply(df$meta, has_tags)
  df <- df[keep, , drop = FALSE]

  if (nrow(df) == 0L) {
    return(list())
  }

  df <- df[order(df$created, decreasing = TRUE, na.last = TRUE), ]

  lapply(df$name, new_rack_id_pins)
}

# rack_info --------------------------------------------------------------------

rack_info <- function(id, backend, ...) UseMethod("rack_info")

#' @export
rack_info.rack_id_pins <- function(id, backend, ...) {

  versions <- tryCatch(
    pins::pin_versions(backend, id$name),
    error = function(e) {
      blockr_warn(
        "Could not retrieve versions for {id$name}: {conditionMessage(e)}",
        class = "rack_info_failed"
      )
      NULL
    }
  )

  if (is.null(versions) || nrow(versions) == 0L) {
    return(data.frame(
      version = character(),
      created = as.POSIXct(character()),
      hash = character(),
      stringsAsFactors = FALSE
    ))
  }

  versions <- versions[order(versions$created, decreasing = TRUE), ]

  if (!"hash" %in% colnames(versions)) {
    versions$hash <- NA_character_
  }

  rownames(versions) <- NULL
  versions[, c("version", "created", "hash"), drop = FALSE]
}

# rack_load --------------------------------------------------------------------

rack_load <- function(id, backend, ...) UseMethod("rack_load")

#' @export
rack_load.rack_id_pins <- function(id, backend, ...) {

  version <- id$version

  if (is.null(version)) {
    info <- rack_info(id, backend)

    if (nrow(info) == 0L) {
      blockr_abort(
        "No versions found for pin {id$name}.",
        class = "rack_load_no_versions"
      )
    }

    version <- info$version[1L]
  }

  meta <- pins::pin_meta(backend, id$name, version)

  if (!has_tags(meta)) {
    blockr_abort(
      "Pin {id$name} is not compatible with blockr (missing session tags).",
      class = "rack_load_invalid_tags"
    )
  }

  dat <- pins::pin_download(backend, id$name, version, meta$pin_hash)

  switch(
    meta$user$format,
    v1 = jsonlite::fromJSON(
      dat,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    ),
    blockr_abort(
      "Unrecognized file format {meta$user$format}.",
      class = "unknown_file_format"
    )
  )
}

# rack_save --------------------------------------------------------------------

rack_save <- function(backend, data, ...) UseMethod("rack_save")

#' @export
rack_save.pins_board <- function(backend, data, ..., name) {

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))

  jsonlite::write_json(data, tmp, null = "null")

  pins::pin_upload(
    backend,
    tmp,
    name,
    versioned = TRUE,
    metadata = list(format = "v1"),
    tags = blockr_session_tags()
  )

  info <- rack_info(new_rack_id_pins(name), backend)

  new_rack_id_pins(
    name = name,
    version = info$version[1L]
  )
}

# rack_delete ------------------------------------------------------------------

rack_delete <- function(id, backend, ...) UseMethod("rack_delete")

#' @export
rack_delete.rack_id_pins <- function(id, backend, ...) {

  version <- id$version

  if (is.null(version)) {
    info <- rack_info(id, backend)

    if (nrow(info) == 0L) {
      blockr_abort(
        "No versions found for pin {id$name}.",
        class = "rack_delete_no_versions"
      )
    }

    version <- info$version[1L]
  }

  pins::pin_version_delete(backend, id$name, version)
  invisible(TRUE)
}

rack_purge <- function(id, backend, ...) UseMethod("rack_purge")

#' @export
rack_purge.rack_id_pins <- function(id, backend, ...) {
  pins::pin_delete(backend, id$name)
  invisible(TRUE)
}
