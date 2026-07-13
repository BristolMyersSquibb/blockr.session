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

#' @export
rack_id_from_input.pins_board <- function(backend, x, ...) {
  new_rack_id_pins(input_id(x), input_version(x))
}

input_id <- function(x) coal(x$id, x$name, fail_all = FALSE)

input_version <- function(x) {

  version <- x$version

  if (not_null(version) && (!nzchar(version) || version == "null")) {
    return(NULL)
  }

  version
}

blockr_session_tags <- function() "blockr-session"

has_tags <- function(x, tags = blockr_session_tags()) {
  all(tags %in% x[["tags"]])
}

# pin_name ------------------------------------------------------------------

pin_name <- function(id, ...) UseMethod("pin_name")

#' @export
pin_name.rack_id_pins <- function(id, ...) id$id

# format --------------------------------------------------------------------

#' @export
format.rack_id_pins <- function(x, ...) {
  v <- if (not_null(x$version)) paste0("@", x$version) else ""
  paste0("<rack_id_pins: ", x$id, v, ">")
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

#' @export
rack_content_hash.rack_id_pins <- function(id, backend, ...) {

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

# rack_exists ---------------------------------------------------------------

#' @export
rack_exists.rack_id_pins <- function(id, backend, ...) {
  pins::pin_exists(backend, pin_name(id))
}

# rack_list -----------------------------------------------------------------

#' @export
rack_list.pins_board <- function(backend, tags = NULL, ...) {
  as_rack_records(filter_workflows(pins::pin_search(backend), tags))
}

#' @export
rack_list.pins_board_folder <- function(backend, tags = NULL, ...) {
  as_rack_records(filter_workflows(local_pin_cache$fetch(backend), tags))
}

filter_workflows <- function(df, tags) {

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

as_rack_records <- function(x, ...) UseMethod("as_rack_records")

#' @export
as_rack_records.data.frame <- function(x, ...) {
  lapply(
    seq_len(nrow(x)),
    function(i) {
      new_rack_record(
        id = x$name[i],
        name = coal(x$meta[[i]]$user$name, x$name[i], fail_all = FALSE),
        saved = x$created[i]
      )
    }
  )
}

# A file board lists by walking each pin's version directories and reading the
# latest data.txt. A pin's directory mtime changes exactly when a version is
# added or removed -- every save, rename, tag edit or delete, since pins
# versions are immutable and each mints or drops a version. So key each pin's
# metadata on that mtime and re-walk only the pins that moved. The cache is
# process-wide and keyed by board path; pin metadata is intrinsic to the pin on
# disk (not session state), so sessions multiplexed on one process share it
# safely and pick up each other's writes via the same mtime. Shiny runs one
# session at a time on the R thread, so no locking is needed.
local_pin_cache <- local({

  cache <- new.env(parent = emptyenv())

  reset <- function() {
    rm(list = ls(cache, all.names = TRUE), envir = cache)
  }

  fetch <- function(backend) {

    root <- normalizePath(backend[["path"]], mustWork = FALSE)
    nms <- pins::pin_list(backend)

    prev <- coal(cache[[root]], list(), fail_all = FALSE)
    cur <- set_names(vector("list", length(nms)), nms)

    for (nm in nms) {

      mtime <- file.info(file.path(root, nm))[["mtime"]]
      hit <- prev[[nm]]

      if (not_null(hit) && !is.na(mtime) && identical(hit[["mtime"]], mtime)) {
        cur[[nm]] <- hit
        next
      }

      meta <- tryCatch(pins::pin_meta(backend, nm), error = function(e) NULL)

      if (not_null(meta)) {
        cur[[nm]] <- list(mtime = mtime, meta = meta)
      }
    }

    cur <- cur[lengths(cur) > 0L]

    cache[[root]] <- cur

    pin_search_df(cur)
  }

  list(fetch = fetch, reset = reset)
})

pin_search_df <- function(cur) {

  if (!length(cur)) {
    return(empty_pin_search())
  }

  metas <- lst_xtr(cur, "meta")

  out <- data.frame(name = names(cur), stringsAsFactors = FALSE)
  out[["created"]] <- do.call(c, unname(lst_xtr(metas, "created")))
  out[["meta"]] <- unname(metas)

  out
}

empty_pin_search <- function() {

  out <- data.frame(name = character(), stringsAsFactors = FALSE)
  out[["created"]] <- .POSIXct(numeric())
  out[["meta"]] <- list()

  out
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
rack_set_acl.rack_id_pins <- function(id, backend, acl_type, ...) {
  blockr_abort(
    "Setting ACL is not supported by this backend.",
    class = "rack_not_supported"
  )
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
rack_unshare.rack_id_pins <- function(id, backend, with_sub, ...) {
  blockr_abort(
    "Unsharing is not supported by this backend.",
    class = "rack_not_supported"
  )
}

#' @export
rack_shares.rack_id_pins <- function(id, backend, ...) {
  blockr_abort(
    "Listing shares is not supported by this backend.",
    class = "rack_not_supported"
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
