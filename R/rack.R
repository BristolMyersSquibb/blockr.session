# new_rack_id ---------------------------------------------------------------

new_rack_id <- function(id, version = NULL, user = NULL, ...,
                        class = character()) {

  if (!is_string(id) || !nzchar(id)) {
    blockr_abort(
      "rack id must be a non-empty string, got {class(id)[[1L]]}.",
      class = "rack_id_invalid_id"
    )
  }

  structure(
    list(id = id, version = version, user = user, ...),
    class = c(class, "rack_id")
  )
}

# new_rack_record -----------------------------------------------------------

new_rack_record <- function(id, name, created = NULL, n_versions = NULL, ...,
                            class = character()) {

  if (!is_string(id) || !nzchar(id)) {
    blockr_abort(
      "rack record id must be a non-empty string, got {class(id)[[1L]]}.",
      class = "rack_record_invalid_id"
    )
  }

  if (!is_string(name)) {
    blockr_abort(
      "rack record name must be a string, got {class(name)[[1L]]}.",
      class = "rack_record_invalid_name"
    )
  }

  structure(
    list(id = id, name = name, created = created, n_versions = n_versions, ...),
    class = c(class, "rack_record")
  )
}

# Accessor generics ---------------------------------------------------------

last_saved <- function(id, ...) UseMethod("last_saved")

pin_name <- function(id, ...) UseMethod("pin_name")

rack_name <- function(id, backend, ...) UseMethod("rack_name")

rack_rename <- function(id, backend, name, ...) UseMethod("rack_rename")

rack_exists <- function(id, backend, ...) UseMethod("rack_exists")

rack_record <- function(id, backend, ...) UseMethod("rack_record")

# Format / print ------------------------------------------------------------

#' @export
format.rack_id <- function(x, ...) {
  paste0("<rack_id: ", x$id, ">")
}

#' @export
print.rack_id <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

#' @export
format.rack_record <- function(x, ...) {
  paste0("<rack_record: ", x$id, " (", x$name, ")>")
}

#' @export
print.rack_record <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

# rack_list -----------------------------------------------------------------

rack_list <- function(backend, tags = NULL, ...) UseMethod("rack_list")

# rack_info -----------------------------------------------------------------

rack_info <- function(id, backend, ...) UseMethod("rack_info")

# rack_download -------------------------------------------------------------

#' Download a session file from a rack backend
#'
#' Retrieves the raw file path(s) for a stored session identified by `id` from
#' the given `backend`. This is a low-level generic; most callers should use
#' [rack_load()] instead.
#'
#' @param id A `rack_id` object identifying the session to download.
#' @param backend A rack backend object (e.g. a `pins_board`).
#' @param ... Additional arguments passed to the method.
#'
#' @return A character vector of local file paths to the downloaded content.
#'
#' @seealso [rack_load()] for the high-level wrapper that parses the result,
#'   [rack_upload()] for the complementary upload generic.
#'
#' @export
rack_download <- function(id, backend, ...) UseMethod("rack_download")

# rack_upload ---------------------------------------------------------------

#' Upload a session file to a rack backend
#'
#' Stores the file at `path` under the record identified by `id`, adding a new
#' version. The storage handle is taken purely from `id` (its stable, name-
#' independent slug); the display `name`, when supplied, is written to the
#' backend's native name field (Connect content title, or pin metadata for file
#' boards) and never affects the storage key. This is a low-level generic; most
#' callers should use [rack_create()] or [rack_update()].
#'
#' @param backend A rack backend object (e.g. a `pins_board`).
#' @param path Character scalar. Path to the local file to upload.
#' @param ... Additional arguments passed to the method, including `id` (the
#'   `rack_id` whose slug keys the record) and `name` (the display name to set;
#'   `NULL` preserves the existing name).
#'
#' @return A `rack_id` object identifying the newly created version.
#'
#' @seealso [rack_create()] and [rack_update()] for the high-level wrappers that
#'   serialise R data before uploading, [rack_download()] for the complementary
#'   download generic.
#'
#' @export
rack_upload <- function(backend, path, ...) UseMethod("rack_upload")

# rack_load -----------------------------------------------------------------

#' Load a session from a rack backend
#'
#' Downloads and parses a stored session identified by `id` from the given
#' `backend`. This is a convenience wrapper around [rack_download()] that
#' additionally deserialises the JSON payload into an R object.
#'
#' @param id A `rack_id` object identifying the session to load.
#' @param backend A rack backend object (e.g. a `pins_board`).
#' @param ... Additional arguments forwarded to [rack_download()].
#'
#' @return The deserialised session data as an R object (typically a named
#'   list).
#'
#' @seealso [rack_create()] and [rack_update()] for the complementary save
#'   functions, [rack_download()] for the underlying download generic.
#'
#' @export
rack_load <- function(id, backend, ...) {
  path <- rack_download(id, backend, ...)
  jsonlite::fromJSON(path, simplifyDataFrame = FALSE,
                     simplifyMatrix = FALSE)
}

# rack_create / rack_update -------------------------------------------------

#' Create or update a session record on a rack backend
#'
#' `rack_create()` serialises `data` to JSON and stores it as a **new** record
#' under a stable, name-independent id. `id` is the storage handle to mint the
#' record under (typically the board id); when `NULL` a fresh id is minted. A
#' create never overwrites: if a non-`NULL` `id` already names a record it
#' errors (class `rack_create_exists`) rather than appending a version -- use
#' `rack_update()` to add a version to an existing record. `name` is written to
#' the backend's native display field. `rack_update()` adds a **new version** to
#' the existing record identified by `id` and never touches its name. Together
#' they replace the former `rack_save()`, separating create from update. To
#' change a record's name, use `rack_rename()`.
#'
#' @param backend A rack backend object (e.g. a `pins_board`).
#' @param data An R object to serialise and store (typically the session list
#'   returned by the blockr session machinery).
#' @param id For `rack_create()`, the storage id to mint the record under (a
#'   fresh id is minted when `NULL`); errors if a non-`NULL` `id` already names
#'   a record. For `rack_update()`, the `rack_id` of the record to add a
#'   version to.
#' @param name Character scalar. The display name for the new record.
#' @param ... Additional arguments forwarded to [rack_upload()].
#'
#' @return A `rack_id` object identifying the newly created version.
#'
#' @seealso [rack_load()] for the complementary load function, `rack_rename()`
#'   to change a record's name, [rack_upload()] for the underlying generic.
#'
#' @export
rack_create <- function(backend, data, id = NULL, name, ...) {

  if (is.null(id)) {
    repeat {
      rid <- rack_id_from_input(list(id = rand_names()), backend)
      if (!rack_exists(rid, backend)) {
        break
      }
    }
  } else {
    rid <- rack_id_from_input(list(id = id), backend)
    if (rack_exists(rid, backend)) {
      blockr_abort(
        "A rack record with id {id} already exists; use rack_update().",
        class = "rack_create_exists"
      )
    }
  }

  upload_serialized(backend, rid, data, name = name, ...)
}

#' @rdname rack_create
#' @export
rack_update <- function(id, backend, data, ...) {
  upload_serialized(backend, id, data, ...)
}

upload_serialized <- function(backend, id, data, ...) {

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))

  jsonlite::write_json(data, tmp, null = "null")

  rack_upload(backend, tmp, id, ...)
}

# rack_delete ---------------------------------------------------------------

rack_delete <- function(id, backend, ...) UseMethod("rack_delete")

rack_purge <- function(id, backend, ...) UseMethod("rack_purge")

# rack_capabilities --------------------------------------------------------

rack_capabilities <- function(backend, ...) UseMethod("rack_capabilities")

# rack_tags ----------------------------------------------------------------

rack_tags <- function(id, backend, ...) UseMethod("rack_tags")

rack_set_tags <- function(id, backend, tags, ...) UseMethod("rack_set_tags")

# rack_acl -----------------------------------------------------------------

rack_acl <- function(id, backend, ...) UseMethod("rack_acl")

rack_set_acl <- function(id, backend, acl_type, ...) UseMethod("rack_set_acl")

# rack_share ---------------------------------------------------------------

rack_share <- function(id, backend, with_sub, ...) UseMethod("rack_share")

rack_unshare <- function(id, backend, with_sub, ...) UseMethod("rack_unshare")

rack_shares <- function(id, backend, ...) UseMethod("rack_shares")

# rack_find_users ----------------------------------------------------------

rack_find_users <- function(backend, query, ...) UseMethod("rack_find_users")
