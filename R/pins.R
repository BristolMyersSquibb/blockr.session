# Constructors --------------------------------------------------------------

new_rack_id_pins <- function(id, version = NULL) {

  if (not_null(version) && (!is_string(version) || !nzchar(version))) {
    blockr_abort(
      "rack_id_pins version must be a non-empty string.",
      class = "rack_id_pins_invalid_version"
    )
  }

  new_rack_id(id, version = version, class = "rack_id_pins")
}

new_rack_id_pins_connect <- function(user, id, version = NULL) {

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
    id,
    version = version,
    user = user,
    class = c("rack_id_pins_connect", "rack_id_pins")
  )
}

rack_id_from_input <- function(x, backend = NULL) {

  id <- coal(x$id, x$name, fail_all = FALSE)

  version <- x$version

  if (not_null(version) && (!nzchar(version) || version == "null")) {
    version <- NULL
  }

  if (not_null(x$user) && nzchar(x$user)) {
    new_rack_id_pins_connect(x$user, id, version)
  } else if (inherits(backend, "pins_board_connect")) {
    new_rack_id_pins_connect(backend$account, id, version)
  } else {
    new_rack_id_pins(id, version)
  }
}

# pin_name ------------------------------------------------------------------

#' @export
pin_name.rack_id_pins <- function(id, ...) id$id

#' @export
pin_name.rack_id_pins_connect <- function(id, ...) {
  paste0(id$user, "/", id$id)
}

# format --------------------------------------------------------------------

#' @export
format.rack_id_pins <- function(x, ...) {
  v <- if (not_null(x$version)) paste0("@", x$version) else ""
  paste0("<rack_id_pins: ", x$id, v, ">")
}

#' @export
format.rack_id_pins_connect <- function(x, ...) {
  v <- if (not_null(x$version)) paste0("@", x$version) else ""
  paste0("<rack_id_pins_connect: ", x$user, "/", x$id, v, ">")
}

# last_saved ----------------------------------------------------------------

#' @export
last_saved.rack_id_pins <- function(id, backend, ...) {
  info <- rack_info(id, backend)
  if (nrow(info) == 0L) return(NULL)
  info$created[1L]
}

# rack_name / rack_rename ---------------------------------------------------

pin_stored_name <- function(backend, id) {

  info <- rack_info(id, backend)

  if (nrow(info) == 0L) {
    return(NULL)
  }

  meta <- pins::pin_meta(backend, pin_name(id), info$version[1L])
  meta$user$name
}

rack_content_hash <- function(id, backend) {

  info <- rack_info(id, backend)

  if (nrow(info) == 0L) {
    return(NULL)
  }

  meta <- pins::pin_meta(backend, pin_name(id), info$version[1L])
  meta$user$content_hash
}

#' @export
rack_name.rack_id_pins <- function(id, backend, ...) {
  coal(pin_stored_name(backend, id), id$id, fail_all = FALSE)
}

#' @export
rack_name.rack_id_pins_connect <- function(id, backend, ...) {

  content <- tryCatch(
    connect_content_find(backend, id$id),
    error = function(e) NULL
  )

  if (is.null(content) || is.null(content$title) || !nzchar(content$title)) {
    return(id$id)
  }

  content$title
}

#' @export
rack_rename.rack_id_pins <- function(id, backend, name, ...) {

  info <- rack_info(id, backend)

  if (nrow(info) == 0L) {
    blockr_abort(
      "No versions found for pin {pin_name(id)}.",
      class = "rack_rename_no_versions"
    )
  }

  version <- info$version[1L]
  meta <- pins::pin_meta(backend, pin_name(id), version)
  path <- pins::pin_download(backend, pin_name(id), version, meta$pin_hash)

  user_meta <- coal(meta$user, list(), fail_all = FALSE)
  user_meta$name <- name

  # pins has no in-place metadata edit, so renaming re-uploads the latest
  # content and mints a version. This only hits file boards (local dev / the
  # no-token fallback); Connect renames cleanly via a content title PATCH.
  pins::pin_upload(
    backend, path, pin_name(id),
    versioned = TRUE,
    metadata = user_meta,
    tags = unique(c(meta$tags, blockr_session_tags()))
  )

  new_rack_id_pins(id$id, version = rack_info(id, backend)$version[1L])
}

#' @export
rack_rename.rack_id_pins_connect <- function(id, backend, name, ...) {

  content <- connect_content_find(backend, id$id) # nolint: object_usage.

  connect_api(
    backend, "PATCH /content/{content$guid}",
    body = list(title = name)
  )

  new_rack_id_pins_connect(
    coal(id$user, backend$account, fail_all = FALSE),
    id$id,
    version = id$version
  )
}

# rack_exists ---------------------------------------------------------------

#' @export
rack_exists.rack_id_pins <- function(id, backend, ...) {
  pins::pin_exists(backend, pin_name(id))
}

# rack_record ---------------------------------------------------------------

#' @export
rack_record.rack_id_pins <- function(id, backend, ...) {

  info <- rack_info(id, backend)
  created <- if (nrow(info) > 0L) info$created[1L] else NULL

  new_rack_record(
    id = id$id,
    name = rack_name(id, backend),
    created = created,
    n_versions = nrow(info),
    user = id$user
  )
}

# rack_list -----------------------------------------------------------------

#' @export
rack_list.pins_board <- function(backend, tags = NULL, ...) {

  df <- pins_session_search(backend, tags)

  if (nrow(df) == 0L) {
    return(list())
  }

  lapply(
    seq_len(nrow(df)),
    function(i) {
      new_rack_record(
        id = df$name[i],
        name = coal(df$meta[[i]]$user$name, df$name[i], fail_all = FALSE),
        created = df$created[i]
      )
    }
  )
}

#' @export
rack_list.pins_board_connect <- function(backend, tags = NULL, ...) {

  df <- pins_session_search(backend, tags)

  if (nrow(df) == 0L) {
    return(list())
  }

  titles <- tryCatch(
    connect_content_titles(backend),
    error = function(e) list()
  )

  lapply(
    seq_len(nrow(df)),
    function(i) {
      parts <- strsplit(df$name[i], "/", fixed = TRUE)[[1L]]
      slug <- parts[2L]
      new_rack_record(
        id = slug,
        name = coal(titles[[slug]], slug, fail_all = FALSE),
        created = df$created[i],
        user = parts[1L]
      )
    }
  )
}

pins_session_search <- function(backend, tags) {

  df <- pins::pin_search(backend)

  if (nrow(df) == 0L) {
    return(df)
  }

  keep <- lgl_ply(df$meta, has_tags)
  df <- df[keep, , drop = FALSE]

  if (not_null(tags)) {
    keep <- lgl_ply(df$meta, has_tags, tags = tags)
    df <- df[keep, , drop = FALSE]
  }

  if (nrow(df) == 0L) {
    return(df)
  }

  df <- df[order(df$created, decreasing = TRUE, na.last = TRUE), ]

  log_debug("rack_list matched {nrow(df)} pin(s)")

  df
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

# rack_download -------------------------------------------------------------

#' @export
rack_download.rack_id_pins <- function(id, backend, ...) {

  name <- pin_name(id)
  version <- id$version

  if (is.null(version)) {
    info <- rack_info(id, backend)

    if (nrow(info) == 0L) {
      blockr_abort(
        "No versions found for pin {name}.",
        class = "rack_load_no_versions"
      )
    }

    version <- info$version[1L]
  }

  log_debug("Pin download target {name} (version {version})")

  meta <- pins::pin_meta(backend, name, version)

  if (!has_tags(meta)) {
    blockr_abort(
      "Pin {name} is not compatible with blockr ",
      "(missing session tags).",
      class = "rack_load_invalid_tags"
    )
  }

  if (!identical(meta$user$format, "v1")) {
    blockr_abort(
      "Unrecognized file format {meta$user$format}.",
      class = "unknown_file_format"
    )
  }

  pins::pin_download(backend, name, version, meta$pin_hash)
}

# rack_upload ---------------------------------------------------------------

#' @export
rack_upload.pins_board <- function(backend, path, id, name = NULL,
                                   content_hash = NULL, ...) {

  slug <- pin_name(id)

  display <- coal(name, pin_stored_name(backend, id), fail_all = FALSE)

  metadata <- list(format = "v1")

  if (not_null(display)) {
    metadata[["name"]] <- display
  }

  if (not_null(content_hash)) {
    metadata[["content_hash"]] <- content_hash
  }

  log_debug("Pin upload target {slug}")

  pins::pin_upload(
    backend,
    path,
    slug,
    versioned = TRUE,
    metadata = metadata,
    tags = blockr_session_tags()
  )

  new_rack_id_pins(id$id, version = rack_info(id, backend)$version[1L])
}

#' @export
rack_upload.pins_board_connect <- function(backend, path, id, name = NULL,
                                           content_hash = NULL, ...) {

  slug <- id$id
  qualified <- paste0(backend$account, "/", slug)

  metadata <- list(format = "v1")

  if (not_null(content_hash)) {
    metadata[["content_hash"]] <- content_hash
  }

  log_debug("Connect pin upload target {qualified}")

  pins::pin_upload(
    backend,
    path,
    qualified,
    versioned = TRUE,
    metadata = metadata,
    tags = blockr_session_tags()
  )

  base <- new_rack_id_pins_connect(backend$account, slug)

  rid <- new_rack_id_pins_connect(
    user = backend$account,
    id = slug,
    version = rack_info(base, backend)$version[1L]
  )

  if (not_null(name)) {
    rack_rename(rid, backend, name)
  }

  rid
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

  session <- shiny::getDefaultReactiveDomain()
  has_api <- is.null(session) ||
    !is.null(session$request$HTTP_POSIT_CONNECT_USER_SESSION_TOKEN)

  list(
    versioning = TRUE,
    tags = TRUE,
    metadata = TRUE,
    sharing = has_api,
    visibility = has_api,
    user_discovery = has_api
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
  "public"
}

#' @export
rack_acl.rack_id_pins_connect <- function(id, backend, ...) {
  content <- connect_content_find(backend, id$id)
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
  content <- connect_content_find(backend, id$id) # nolint: object_usage.
  connect_api(
    backend, "PATCH /content/{content$guid}",
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
  content <- connect_content_find(backend, id$id) # nolint: object_usage.
  connect_api(
    backend, "POST /content/{content$guid}/permissions",
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
  content <- connect_content_find(backend, id$id) # nolint: object_usage.
  perms <- connect_api(backend, "GET /content/{content$guid}/permissions")

  match <- Filter(function(p) p$principal_guid == with_sub, perms)

  if (length(match) == 0L) {
    blockr_abort(
      "No permission found for principal {with_sub}.",
      class = "rack_permission_not_found"
    )
  }

  connect_api(
    backend,
    "DELETE /content/{content$guid}/permissions/{match[[1L]]$id}"
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

  content <- connect_content_find(backend, id$id) # nolint: object_usage.
  perms <- connect_api(backend, "GET /content/{content$guid}/permissions")

  lapply(perms, function(p) {
    guid <- p$principal_guid # nolint: object_usage.
    user <- tryCatch(
      connect_api(backend, "GET /users/{guid}"),
      error = function(e) NULL
    )

    p$display_name <- if (not_null(user)) {
      name <- paste(coal(user$first_name, ""), coal(user$last_name, ""))
      if (nzchar(trimws(name))) trimws(name) else coal(user$username, guid)
    } else {
      guid
    }

    p
  })
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
  result <- connect_api(backend, "GET /users", query = list(prefix = query))
  result$results
}
