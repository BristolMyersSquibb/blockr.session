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

rack_id_from_input <- function(x, backend = NULL) {

  version <- x$version

  if (not_null(version) && (!nzchar(version) || version == "null")) {
    version <- NULL
  }

  if (not_null(x$user) && nzchar(x$user)) {
    new_rack_id_pins_connect(x$user, x$name, version)
  } else if (inherits(backend, "pins_board_connect")) {
    new_rack_id_pins_connect(backend$account, x$name, version)
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
rack_list.pins_board <- function(backend, tags = NULL, ...) {

  df <- pins::pin_search(backend)

  if (nrow(df) == 0L) {
    return(list())
  }

  keep <- lgl_ply(df$meta, has_tags)
  df <- df[keep, , drop = FALSE]

  if (not_null(tags)) {
    keep <- lgl_ply(df$meta, has_tags, tags = tags)
    df <- df[keep, , drop = FALSE]
  }

  if (nrow(df) == 0L) {
    return(list())
  }

  df <- df[order(df$created, decreasing = TRUE, na.last = TRUE), ]

  lapply(df$name, new_rack_id_pins)
}

#' @export
rack_list.pins_board_connect <- function(backend, tags = NULL, ...) {

  df <- pins::pin_search(backend)

  if (nrow(df) == 0L) {
    return(list())
  }

  keep <- lgl_ply(df$meta, has_tags)
  df <- df[keep, , drop = FALSE]

  if (not_null(tags)) {
    keep <- lgl_ply(df$meta, has_tags, tags = tags)
    df <- df[keep, , drop = FALSE]
  }

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

      if (!grepl("Can't find pin", conditionMessage(e), fixed = TRUE)) {
        blockr_warn(
          "Could not retrieve versions for {pin_name(id)}: ",
          "{conditionMessage(e)}",
          class = "rack_info_failed"
        )
      }

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
      "Pin {pin_name(id)} is not compatible with blockr ",
      "(missing session tags).",
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

  info <- rack_info(new_rack_id_pins_connect(backend$account, name), backend)

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

# rack_capabilities --------------------------------------------------------

#' @export
rack_capabilities.pins_board <- function(backend, ...) {
  list(
    versioning = TRUE,
    tags = TRUE,
    metadata = TRUE,
    sharing = FALSE,
    visibility = FALSE,
    user_discovery = FALSE
  )
}

#' @export
rack_capabilities.pins_board_connect <- function(backend, ...) {
  list(
    versioning = TRUE,
    tags = TRUE,
    metadata = TRUE,
    sharing = FALSE,
    visibility = FALSE,
    user_discovery = FALSE
  )
}

# rack_tags ----------------------------------------------------------------

#' @export
rack_tags.rack_id_pins <- function(id, backend, ...) {
  meta <- pins::pin_meta(backend, pin_name(id), id$version)
  setdiff(meta$tags, blockr_session_tags())
}

#' @export
rack_set_tags.rack_id_pins <- function(id, backend, tags, ...) {

  name <- pin_name(id)
  version <- id$version

  if (is.null(version)) {
    info <- rack_info(id, backend)

    if (nrow(info) == 0L) {
      blockr_abort(
        "No versions found for pin {name}.",
        class = "rack_set_tags_no_versions"
      )
    }

    version <- info$version[1L]
  }

  meta <- pins::pin_meta(backend, name, version)
  path <- pins::pin_download(backend, name, version, meta$pin_hash)

  all_tags <- unique(c(tags, blockr_session_tags()))

  pins::pin_upload(
    backend,
    path,
    name,
    versioned = TRUE,
    metadata = meta$user,
    tags = all_tags
  )

  invisible(id)
}

# rack_acl -----------------------------------------------------------------

#' @export
rack_acl.rack_id_pins <- function(id, backend, ...) {
  NULL
}

#' @export
rack_acl.rack_id_pins_connect <- function(id, backend, ...) {
  content <- connect_content_find(backend, id$name)
  content$access_type
}

#' @export
rack_set_acl.rack_id_pins <- function(id, backend, acl_type, ...) {
  blockr_abort(
    "Setting ACL is not supported by this backend.",
    class = "rack_not_supported"
  )
}

#' @export
rack_set_acl.rack_id_pins_connect <- function(id, backend, acl_type, ...) {
  content <- connect_content_find(backend, id$name)
  connect_api(
    backend, "PATCH",
    paste0("/content/", content$guid),
    body = list(access_type = acl_type)
  )
  invisible(id)
}

# rack_share ---------------------------------------------------------------

#' @export
rack_share.rack_id_pins <- function(id, backend, with_sub, ...) {
  blockr_abort(
    "Sharing is not supported by this backend.",
    class = "rack_not_supported"
  )
}

#' @export
rack_share.rack_id_pins_connect <- function(id, backend, with_sub, ...) {
  content <- connect_content_find(backend, id$name)
  connect_api(
    backend, "POST",
    paste0("/content/", content$guid, "/permissions"),
    body = list(
      principal_guid = with_sub,
      principal_type = "user",
      role = "viewer"
    )
  )
  invisible(id)
}

#' @export
rack_unshare.rack_id_pins <- function(id, backend, with_sub, ...) {
  blockr_abort(
    "Unsharing is not supported by this backend.",
    class = "rack_not_supported"
  )
}

#' @export
rack_unshare.rack_id_pins_connect <- function(id, backend, with_sub, ...) {
  content <- connect_content_find(backend, id$name)
  perms <- connect_api(
    backend, "GET",
    paste0("/content/", content$guid, "/permissions")
  )

  match <- Filter(function(p) p$principal_guid == with_sub, perms)

  if (length(match) == 0L) {
    blockr_abort(
      "No permission found for principal {with_sub}.",
      class = "rack_permission_not_found"
    )
  }

  connect_api(
    backend, "DELETE",
    paste0("/content/", content$guid, "/permissions/", match[[1L]]$id)
  )
  invisible(id)
}

#' @export
rack_shares.rack_id_pins <- function(id, backend, ...) {
  blockr_abort(
    "Listing shares is not supported by this backend.",
    class = "rack_not_supported"
  )
}

#' @export
rack_shares.rack_id_pins_connect <- function(id, backend, ...) {
  content <- connect_content_find(backend, id$name)
  connect_api(
    backend, "GET",
    paste0("/content/", content$guid, "/permissions")
  )
}

# rack_find_users ----------------------------------------------------------

#' @export
rack_find_users.pins_board <- function(backend, query, ...) {
  blockr_abort(
    "User discovery is not supported by this backend.",
    class = "rack_not_supported"
  )
}

#' @export
rack_find_users.pins_board_connect <- function(backend, query, ...) {
  result <- connect_api(
    backend, "GET",
    paste0("/users?prefix=", utils::URLencode(query, reserved = TRUE))
  )
  result$results
}
