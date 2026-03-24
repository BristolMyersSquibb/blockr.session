# new_rack_id ---------------------------------------------------------------

new_rack_id <- function(name, ..., class = character()) {

  if (!is_string(name) || !nzchar(name)) {
    blockr_abort(
      "rack id name must be a non-empty string, got {class(name)[[1L]]}.",
      class = "rack_id_invalid_name"
    )
  }

  structure(list(name = name, ...), class = c(class, "rack_id"))
}

# Accessor generics ---------------------------------------------------------

display_name <- function(id, ...) UseMethod("display_name")

#' @export
display_name.rack_id <- function(id, ...) id$name

last_saved <- function(id, ...) UseMethod("last_saved")

pin_name <- function(id, ...) UseMethod("pin_name")

# Format / print ------------------------------------------------------------

#' @export
format.rack_id <- function(x, ...) {
  paste0("<rack_id: ", x$name, ">")
}

#' @export
print.rack_id <- function(x, ...) {
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
#' Stores the file at `path` under the given `backend`, creating a new versioned
#' entry. This is a low-level generic; most callers should use [rack_save()]
#' instead.
#'
#' @param backend A rack backend object (e.g. a `pins_board`).
#' @param path Character scalar. Path to the local file to upload.
#' @param ... Additional arguments passed to the method.
#' @param name Character scalar. The name under which the session will be
#'   stored.
#'
#' @return A `rack_id` object identifying the newly created version.
#'
#' @seealso [rack_save()] for the high-level wrapper that serialises R data
#'   before uploading, [rack_download()] for the complementary download generic.
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
#' @seealso [rack_save()] for the complementary save function,
#'   [rack_download()] for the underlying download generic.
#'
#' @export
rack_load <- function(id, backend, ...) {
  path <- rack_download(id, backend, ...)
  jsonlite::fromJSON(path, simplifyDataFrame = FALSE,
                     simplifyMatrix = FALSE)
}

# rack_save -----------------------------------------------------------------

#' Save a session to a rack backend
#'
#' Serialises `data` to a temporary JSON file and uploads it to `backend` under
#' `name`. This is a convenience wrapper around [rack_upload()] that handles
#' JSON serialisation automatically.
#'
#' @param backend A rack backend object (e.g. a `pins_board`).
#' @param data An R object to serialise and store (typically the session list
#'   returned by the blockr session machinery).
#' @param ... Additional arguments forwarded to [rack_upload()].
#' @param name Character scalar. The name under which the session will be
#'   stored.
#'
#' @return A `rack_id` object identifying the newly created version, returned
#'   invisibly by the underlying [rack_upload()] call.
#'
#' @seealso [rack_load()] for the complementary load function,
#'   [rack_upload()] for the underlying upload generic.
#'
#' @export
rack_save <- function(backend, data, ..., name) {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  jsonlite::write_json(data, tmp, null = "null")
  rack_upload(backend, tmp, ..., name = name)
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
