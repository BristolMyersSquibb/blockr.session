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
as_rack_id.pins_board <- function(x, backend, ...) {
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

# A file board's listing costs one readdir and nothing else: the pin directory
# names ARE the ids, and a pin directory's mtime is its last save, because pins
# versions are immutable and every save, delete or tag edit mints or drops a
# version directory in it.
#
# Reading pin metadata instead -- which is what this did -- costs a dir_ls, a
# read and a YAML parse PER PIN, and it bought exactly one field the ids do not
# already carry: the display name. Since the id became the name (the Save-as
# dialog writes the typed string as both, and rack_rename is no longer reachable
# from the UI), that field is a copy of the id for every workflow this package
# creates, and paying a per-pin round trip for it is the listing's whole cost.
# On a network share, where each of those calls is a round trip, it is the
# difference between a listing that scales with the store and one that does not:
# 200 pins with 8 versions each, cold, measured 0.379s the old way and 0.001s
# this way on a local disk.
#
# What it gives up, and why each is acceptable here:
#
# - A pin whose stored name DIFFERS from its id lists under its id. Three ways
#   to get one: saved before the id became the name, renamed through the API, or
#   uploaded from a file whose name is not the payload's id (`upload_workflows`
#   keys on the id in the JSON and takes the name from the filename). The round
#   trip does not produce one, since a download is named after the record.
#   `rack_name()` still reads the stored name, so a board opens under its real
#   title either way -- the divergence is visible in the listing only.
# - Non-blockr pins in the directory now list. The tag check moves to load time,
#   where `rack_load()` already refuses them (`rack_load_invalid_tags`), which is
#   what the Connect backend has always done -- so the two backends stop
#   disagreeing about what a listing means.
#
# Neither is worth a per-pin read. A store that needs BOTH the speed and names
# that diverge from ids needs an index written on save, not a cheaper walk.
#' @export
rack_list.pins_board_folder <- function(backend, tags = NULL, ...) {

  # Filtering by tag is the one question ids cannot answer, so asking it opts
  # into the metadata walk (`rack_list.pins_board`). The app never asks: the
  # listing is unfiltered, and this is the path it takes.
  if (not_null(tags)) {
    return(NextMethod())
  }

  ids <- pins::pin_list(backend)

  if (!length(ids)) {
    return(list())
  }

  mtime <- file.info(file.path(backend[["path"]], ids))[["mtime"]]

  ord <- order(mtime, decreasing = TRUE, na.last = TRUE)

  log_debug("rack_list found {length(ids)} pin(s)")

  lapply(
    ord,
    function(i) new_rack_record(id = ids[i], name = ids[i], saved = mtime[i])
  )
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

# rack_info -----------------------------------------------------------------

version_table <- function(id, backend) {

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
    return(NULL)
  }

  versions <- versions[order(versions$created, decreasing = TRUE), ]
  rownames(versions) <- NULL

  versions
}

empty_version_info <- function() {
  data.frame(
    version = character(),
    created = as.POSIXct(character()),
    ref = character(),
    stringsAsFactors = FALSE
  )
}

#' @export
rack_info.rack_id_pins <- function(id, backend, ...) {

  versions <- version_table(id, backend)

  if (is.null(versions)) {
    return(empty_version_info())
  }

  data.frame(
    version = versions$version,
    created = versions$created,
    ref = versions$hash,
    stringsAsFactors = FALSE
  )
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
