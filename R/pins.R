# Constructors --------------------------------------------------------------

new_rack_id_pins <- function(name, version = NULL) {

  if (not_null(version) && (!is_string(version) || !nzchar(version))) {
    blockr_abort(
      "rack_id_pins version must be a non-empty string.",
      class = "rack_id_pins_invalid_version"
    )
  }

  new_rack_id(name, version = version, class = "rack_id_pins")
}

new_rack_id_pins_connect <- function(user, name, version = NULL) {

  if (!is_string(user) || !nzchar(user)) {
    blockr_abort(
      "rack_id_pins_connect user must be a non-empty string.",
      class = "rack_id_pins_connect_invalid_user"
    )
  }

  if (not_null(version) && (!is_string(version) || !nzchar(version))) {
    blockr_abort(
      "rack_id_pins_connect version must be a non-empty string.",
      class = "rack_id_pins_connect_invalid_version"
    )
  }

  new_rack_id(
    name,
    user = user,
    version = version,
    class = c("rack_id_pins_connect", "rack_id_pins")
  )
}

rack_id_from_input <- function(x) {

  version <- x$version

  if (not_null(version) && (!nzchar(version) || version == "null")) {
    version <- NULL
  }

  if (not_null(x$user) && nzchar(x$user)) {
    new_rack_id_pins_connect(x$user, x$name, version)
  } else {
    new_rack_id_pins(x$name, version)
  }
}

rack_id_for_board <- function(name, backend) {
  if (inherits(backend, "pins_board_connect")) {
    new_rack_id_pins_connect(backend$account, name)
  } else {
    new_rack_id_pins(name)
  }
}

# pin_name ------------------------------------------------------------------

#' @export
pin_name.rack_id_pins <- function(id, ...) id$name

#' @export
pin_name.rack_id_pins_connect <- function(id, ...) {
  paste0(id$user, "/", id$name)
}

# display_name --------------------------------------------------------------

#' @export
display_name.rack_id_pins_connect <- function(id, ...) id$name

# format --------------------------------------------------------------------

#' @export
format.rack_id_pins <- function(x, ...) {
  v <- if (not_null(x$version)) paste0("@", x$version) else ""
  paste0("<rack_id_pins: ", x$name, v, ">")
}

#' @export
format.rack_id_pins_connect <- function(x, ...) {
  v <- if (not_null(x$version)) paste0("@", x$version) else ""
  paste0("<rack_id_pins_connect: ", x$user, "/", x$name, v, ">")
}

# last_saved ----------------------------------------------------------------

#' @export
last_saved.rack_id_pins <- function(id, backend, ...) {
  info <- rack_info(id, backend)
  if (nrow(info) == 0L) return(NULL)
  info$created[1L]
}

# rack_list -----------------------------------------------------------------

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

#' @export
rack_list.pins_board_connect <- function(backend, ...) {

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

  lapply(df$name, function(qualified) {
    parts <- strsplit(qualified, "/", fixed = TRUE)[[1L]]
    new_rack_id_pins_connect(user = parts[1L], name = parts[2L])
  })
}

# rack_info -----------------------------------------------------------------

#' @export
rack_info.rack_id_pins <- function(id, backend, ...) {

  versions <- tryCatch(
    pins::pin_versions(backend, pin_name(id)),
    error = function(e) {
      blockr_warn(
        "Could not retrieve versions for {pin_name(id)}: {conditionMessage(e)}",
        class = "rack_info_failed"
      )
      NULL
    }
  )

  if (is.null(versions) || nrow(versions) == 0L) {
    return(
      data.frame(
        version = character(),
        created = as.POSIXct(character()),
        hash = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  versions <- versions[order(versions$created, decreasing = TRUE), ]

  if (!"hash" %in% colnames(versions)) {
    versions$hash <- NA_character_
  }

  rownames(versions) <- NULL
  versions[, c("version", "created", "hash"), drop = FALSE]
}

# rack_load -----------------------------------------------------------------

#' @export
rack_load.rack_id_pins <- function(id, backend, ...) {

  version <- id$version

  if (is.null(version)) {
    info <- rack_info(id, backend)

    if (nrow(info) == 0L) {
      blockr_abort(
        "No versions found for pin {pin_name(id)}.",
        class = "rack_load_no_versions"
      )
    }

    version <- info$version[1L]
  }

  meta <- pins::pin_meta(backend, pin_name(id), version)

  if (!has_tags(meta)) {
    blockr_abort(
      "Pin {pin_name(id)} is not compatible with blockr (missing session tags).",
      class = "rack_load_invalid_tags"
    )
  }

  dat <- pins::pin_download(backend, pin_name(id), version, meta$pin_hash)

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

# rack_save -----------------------------------------------------------------

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

#' @export
rack_save.pins_board_connect <- function(backend, data, ..., name) {

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

  qualified <- paste0(backend$account, "/", name)
  info <- rack_info(new_rack_id_pins(qualified), backend)

  new_rack_id_pins_connect(
    user = backend$account,
    name = name,
    version = info$version[1L]
  )
}

# rack_delete ---------------------------------------------------------------

#' @export
rack_delete.rack_id_pins <- function(id, backend, ...) {

  version <- id$version

  if (is.null(version)) {
    info <- rack_info(id, backend)

    if (nrow(info) == 0L) {
      blockr_abort(
        "No versions found for pin {pin_name(id)}.",
        class = "rack_delete_no_versions"
      )
    }

    version <- info$version[1L]
  }

  pins::pin_version_delete(backend, pin_name(id), version)
  invisible(TRUE)
}

#' @export
rack_purge.rack_id_pins <- function(id, backend, ...) {
  pins::pin_delete(backend, pin_name(id))
  invisible(TRUE)
}
