#' Rack storage backend contract
#'
#' The rack layer is the storage abstraction behind blockr session
#' persistence: it turns a save into a stored, versioned record and a load back
#' into board state. The shipped backend dispatches on \pkg{pins} boards (see
#' [user_pins_board()]), but the contract is storage-agnostic -- a database or
#' document store is supported by implementing the generics documented here and
#' carrying the `rack_backend` class, which is what wiring a non-pins backend
#' into the `session_mgmt_backend` [blockr.core::blockr_option()] requires. The
#' high-level [rack_create()],
#' [rack_append()] and [rack_load()] entry points that application code calls
#' are built on these generics.
#'
#' Two kinds of object flow through the contract. A *backend* is the board or
#' store itself, the dispatch target for `as_rack_id()`, `rack_capabilities()`,
#' `rack_list()`, `rack_upload()` and `rack_find_users()`. A `rack_id` is a
#' handle to one stored record, the dispatch target for the remaining generics.
#' [as_rack_id()] coerces a raw identifier -- a component list or a
#' `rack_record` listing row -- into the backend's own `rack_id` subclass (via
#' `new_rack_id(..., class = )`) from the components (`id`, `version`, `user`)
#' carried in a session URL, and returns an existing `rack_id` unchanged.
#' `rack_list()` returns [new_rack_record()] summaries, the listing rows the
#' workflow menu renders.
#'
#' @section Capabilities:
#' `rack_capabilities()` is the feature switch: it returns a named list of
#' logical flags declaring which optional features the backend supports, and
#' the management UI gates the matching generics on it.
#'
#' - `versioning`: a version history is kept per record: `rack_info()` lists it
#'   and `rack_download()` / `rack_delete()` accept a version.
#' - `tags`: `rack_tags()` and `rack_set_tags()` are implemented.
#' - `metadata`: a display name (`rack_name()`, `rack_rename()`) and a content
#'   hash (`rack_content_hash()`) are stored alongside the payload.
#' - `sharing`: `rack_share()`, `rack_unshare()` and `rack_shares()` are
#'   implemented.
#' - `visibility`: `rack_acl()` and `rack_set_acl()` are implemented.
#' - `user_discovery`: `rack_find_users()` is implemented.
#'
#' A minimal backend implements only the core create / load / list set --
#' `as_rack_id()`, `rack_upload()`, `rack_download()`, `rack_exists()`,
#' `rack_list()`, `rack_info()` and `rack_name()` -- and returns `FALSE` for
#' every optional capability; the generics behind a `FALSE` flag are then never
#' reached and need no method.
#'
#' @param backend A rack backend: the storage object dispatched on. A
#'   \pkg{pins} board is recognized directly; any other backend carries the
#'   `rack_backend` class so the `session_mgmt_backend` option accepts it.
#' @param id A `rack_id` identifying a stored record, for the accessor and
#'   mutator generics. For `new_rack_id()` and `new_rack_record()`, the
#'   record's storage id: a non-empty string, stable and independent of the
#'   display name.
#' @param x The object to coerce with `as_rack_id()`: an existing `rack_id`
#'   (returned unchanged), or a component bag -- a list of identifier fields
#'   (`id`, and optionally `version` and `user`) or a `rack_record` -- turned
#'   into the backend's own `rack_id` subclass.
#' @param path Path to the local file to upload as a new version.
#' @param name Display name for the record, written to the backend's native
#'   name field and never used as the storage key; `NULL` keeps the current
#'   name.
#' @param tags Character vector of tags: the filter applied by `rack_list()`
#'   (`NULL` for none), or the tag set written by `rack_set_tags()`.
#' @param acl_type Access-control level to set, backend-defined (e.g.
#'   `"private"`, `"acl"`, `"all"`).
#' @param with_sub Backend identifier of the principal to share a record with
#'   or revoke.
#' @param query Search prefix for `rack_find_users()`.
#' @param version For `new_rack_id()`, an optional version string pinning the
#'   `rack_id` to one stored version; `NULL` tracks the latest.
#' @param user For `new_rack_id()`, the owning account, used to reach records
#'   stored under another user's namespace.
#' @param class Character vector prepended to the object's class, so a backend
#'   carries its own `rack_id` / `rack_record` subclass.
#' @param ... Passed on to methods; for the constructors, extra named fields
#'   stored on the object.
#'
#' @return
#' `new_rack_id()` and `new_rack_record()` return a `rack_id` and `rack_record`
#' respectively. `rack_capabilities()` returns the named list of flags above.
#' `rack_list()` returns a list of `rack_record`s and `rack_info()` a data
#' frame of versions (columns `version`, `created`, `ref`, newest first).
#' `as_rack_id()`, `rack_upload()` and `rack_rename()` return a `rack_id`;
#' `rack_download()` a local file path; `rack_name()` a string; `rack_exists()`
#' a scalar logical; `rack_content_hash()` the stored payload hash;
#' `rack_tags()`, `rack_acl()` and `rack_shares()` the stored tags, access
#' level and shares. The mutators (`rack_set_tags()`, `rack_set_acl()`,
#' `rack_share()`, `rack_unshare()`, `rack_delete()`, `rack_purge()`) return
#' invisibly.
#'
#' @seealso [rack_create()], [rack_append()] and [rack_load()] for the
#'   high-level save / load API built on the contract, and [user_pins_board()]
#'   for the default \pkg{pins} backend.
#'
#' @name rack-backend
NULL

# new_rack_id ---------------------------------------------------------------

#' @rdname rack-backend
#' @export
new_rack_id <- function(id, version = NULL, user = NULL, ...,
                        class = character()) {

  if (!is_string(id) || !nzchar(id)) {
    blockr_abort(
      "rack id must be a non-empty string, got {class(id)[[1L]]}.",
      class = "rack_id_invalid_id"
    )
  }

  if (anyNA(version)) {
    blockr_abort(
      "rack id version must not be NA.",
      class = "rack_id_invalid_version"
    )
  }

  structure(
    list(id = id, version = version, user = user, ...),
    class = c(class, "rack_id")
  )
}

# new_rack_record -----------------------------------------------------------

#' @rdname rack-backend
#' @export
new_rack_record <- function(id, name, ..., class = character()) {

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
    list(id = id, name = name, ...),
    class = c(class, "rack_record")
  )
}

# Accessor generics ---------------------------------------------------------

#' @rdname rack-backend
#' @export
rack_content_hash <- function(id, backend, ...) UseMethod("rack_content_hash")

#' @rdname rack-backend
#' @export
rack_name <- function(id, backend, ...) UseMethod("rack_name")

#' @rdname rack-backend
#' @export
rack_rename <- function(id, backend, name, ...) UseMethod("rack_rename")

#' @rdname rack-backend
#' @export
rack_exists <- function(id, backend, ...) UseMethod("rack_exists")

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

#' @rdname rack-backend
#' @export
rack_list <- function(backend, tags = NULL, ...) UseMethod("rack_list")

# as_rack_id ----------------------------------------------------------------

#' @rdname rack-backend
#' @export
as_rack_id <- function(x, backend, ...) {

  if (inherits(x, "rack_id")) {
    return(x)
  }

  # dispatch on the backend, not x
  UseMethod("as_rack_id", backend)
}

# rack_info -----------------------------------------------------------------

#' @rdname rack-backend
#' @export
rack_info <- function(id, backend, ...) UseMethod("rack_info")

# rack_download -------------------------------------------------------------

#' @rdname rack-backend
#' @export
rack_download <- function(id, backend, ...) UseMethod("rack_download")

# rack_upload ---------------------------------------------------------------

#' @rdname rack-backend
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
#' @seealso [rack_create()] and [rack_append()] for the complementary save
#'   functions, [rack_download()] for the underlying download generic.
#'
#' @export
rack_load <- function(id, backend, ...) {
  path <- rack_download(id, backend, ...)
  jsonlite::fromJSON(path, simplifyDataFrame = FALSE,
                     simplifyMatrix = FALSE)
}

# rack_create / rack_append -------------------------------------------------

#' Create or append to a session record on a rack backend
#'
#' `rack_create()` serialises `data` to JSON and stores it as a **new** record
#' keyed on `id` -- the board's own stable id, so the record id and the board id
#' match. It is a strict insert: it errors (class `rack_create_exists`) if `id`
#' already names a record rather than appending a version. `name` is written to
#' the backend's native display field. `rack_append()` adds a **new version** to
#' the existing record identified by `id`, erroring (class
#' `rack_append_missing`) if there is none, and never touches the name. Together
#' they replace the former `rack_save()`, separating insert from append. To
#' change a record's name, use `rack_rename()`. `rack_records()` lists what a
#' backend holds, of one kind at a time.
#'
#' @section Drafts:
#' A *draft* is unsaved work parked for crash recovery. It is an ordinary rack
#' record in every respect a backend can see -- same store, same tags, read
#' with [rack_load()] and dropped with `rack_purge()` -- and all that sets it
#' apart is a reserved id, minted by `rack_create(draft = )` and matched by
#' `rack_records(draft = )`. Nothing below this API knows drafts exist, so a
#' backend needs no draft support of its own.
#'
#' Passing `draft` also relaxes what `rack_create()` does, because a draft is a
#' single slot rather than a history: the id-already-taken check is skipped, so
#' a repeated write overwrites, and the write is unversioned. With
#' `draft = FALSE` the reserved namespace is refused instead, so a user cannot
#' save a record into it.
#'
#' A draft is of one *kind*. A `record` draft follows a saved workflow and is
#' keyed on it, so its id is the same in every session: there is one slot per
#' workflow by construction, it is overwritten as the work changes, and the
#' save it was shadowing drops it. Nothing about it expires, since a workflow
#' bounds it. A `session` draft belongs to a board with no record yet, keyed on
#' an opaque per-session id, and those do accumulate -- so one carries the time
#' it was opened and is swept on a retention read straight off the id. That
#' needs neither the optional `metadata` capability nor any field beyond those
#' `new_rack_record()` guarantees. The key is hashed into the id rather than
#' embedded, since a workflow id may hold characters a backend rejects in a
#' record name.
#'
#' The id a draft write returns carries no version: a draft keeps one, and the
#' next write to that slot deletes it.
#'
#' @param backend A rack backend object (e.g. a `pins_board`).
#' @param data An R object to serialise and store (typically the session list
#'   returned by the blockr session machinery).
#' @param id For `rack_create()`, the storage id to key the new record on
#'   (typically the board id); errors if it already names a record. For
#'   `rack_append()`, the `rack_id` of the record to add a version to.
#' @param name Character scalar. The display name for the new record.
#' @param draft Which records this concerns: `FALSE` for one the user saved,
#'   or a draft kind, `"session"` or `"record"`. A `rack_records()` call also
#'   accepts `TRUE`, matching a draft of either kind.
#' @param ... Additional arguments forwarded to [rack_upload()] or, for
#'   `rack_records()`, to [rack_list()].
#'
#' @return `rack_create()` and `rack_append()` return a `rack_id` object
#'   identifying the newly created version, `rack_records()` a list of
#'   `rack_record`s.
#'
#' @seealso [rack_load()] for the complementary load function, `rack_rename()`
#'   to change a record's name, [rack_upload()] and [rack_list()] for the
#'   underlying generics.
#'
#' @export
rack_create <- function(backend, data, id, name, draft = FALSE, ...) {

  rid <- as_rack_id(list(id = draft_record_id(id, draft)), backend)

  if (isFALSE(draft) && rack_exists(rid, backend)) {
    blockr_abort(
      "A rack record with id {id} already exists; use rack_append().",
      class = "rack_create_exists"
    )
  }

  res <- upload_serialized(
    backend, rid, data, name = name, versioned = isFALSE(draft), ...
  )

  if (isFALSE(draft)) {
    return(res)
  }

  # A draft keeps one version and the next write deletes it, so pinning the
  # returned handle to that version would break the very next read
  as_rack_id(list(id = res$id, user = res$user), backend)
}

#' @rdname rack_create
#' @export
rack_append <- function(id, backend, data, ...) {

  if (!rack_exists(id, backend)) {
    blockr_abort(
      "No rack record with id {id$id}; use rack_create().",
      class = "rack_append_missing"
    )
  }

  upload_serialized(backend, id, data, versioned = TRUE, ...)
}

#' @rdname rack_create
#' @export
rack_records <- function(backend, draft = FALSE, ...) {

  records <- rack_list(backend, ...)

  records[is_draft_record(chr_xtr(records, "id"), draft)]
}

# Drafts are ordinary rack records; all that separates them from what a user
# saved is a reserved id, minted by `draft_record_id()` and read back by
# `is_draft_record()`. Nothing below this plain API knows about them, so a
# backend stores and lists a draft exactly as it does anything else.
#
# The id carries everything retention needs -- which kind, whether the owning
# session closed cleanly, and when -- because it is the one field the contract
# guarantees on a listing. Metadata is an optional capability and `saved` is
# not part of `new_rack_record()`, so a sweep resting on either would quietly
# do nothing on a conforming backend that supplies neither.
#
#   blockr-draft-<state>-<epoch>-<hash>
#
# The key is hashed rather than embedded: Posit Connect rejects a content name
# outside `[-_A-Za-z0-9]`, which a workflow id is free to violate on a file
# board, and hashing also keeps the id a fixed width whatever the id it is
# derived from.
draft_kinds <- c("session", "record")

# Only a session draft carries a mint time, because only a session draft
# expires. A record draft is anchored to a workflow, so there is at most one
# per workflow however long anyone edits, and it is dropped when the save it
# was shadowing lands. That leaves its id nothing to say beyond which workflow
# it belongs to, and makes it the same id in every session -- one slot per
# record by construction rather than by tidying up after.
draft_prefix <- "blockr-draft-"

draft_key_hash <- function(key) {
  substr(rlang::hash(key), 1L, 8L)
}

is_reserved_id <- function(id) {
  startsWith(id, draft_prefix)
}

draft_record_id <- function(id, draft, time = Sys.time()) {

  if (isFALSE(draft)) {

    if (any(is_reserved_id(id))) {
      blockr_abort(
        "The rack record id {id} is reserved for drafts.",
        class = "rack_id_reserved"
      )
    }

    return(id)
  }

  kind <- match.arg(draft, draft_kinds)

  # idempotent: an id already minted for this kind is a slot being overwritten,
  # and its epoch is when the slot was opened, not now
  if (is_draft_record(id, kind)) {
    return(id)
  }

  if (identical(kind, "record")) {
    return(paste0(draft_prefix, kind, "-", draft_key_hash(id)))
  }

  paste0(draft_prefix, kind, "-", as.integer(time), "-", draft_key_hash(id))
}

# `draft` matches either a whole state or just the kind, so a caller can ask
# for every session draft without naming both of its states.
is_draft_record <- function(id, draft) {

  if (isFALSE(draft)) {
    return(!is_reserved_id(id))
  }

  if (isTRUE(draft)) {
    return(is_reserved_id(id))
  }

  startsWith(id, paste0(draft_prefix, match.arg(draft, draft_kinds)))
}

draft_record_parts <- function(id) {
  strsplit(sub(paste0("^", draft_prefix), "", id), "-", fixed = TRUE)[[1L]]
}

draft_record_kind <- function(id) {
  draft_record_parts(id)[[1L]]
}

draft_record_hash <- function(id) {

  parts <- draft_record_parts(id)

  parts[[length(parts)]]
}

draft_record_epoch <- function(id) {
  as.numeric(draft_record_parts(id)[[2L]])
}

upload_serialized <- function(backend, id, data, ...) {

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))

  jsonlite::write_json(data, tmp, null = "null")

  rack_upload(backend, tmp, id, content_hash = content_hash(data), ...)
}

# a stable digest of the serialized payload, stored per version (via
# rack_content_hash) so a save can tell whether anything actually changed
content_hash <- function(data) {
  rlang::hash(data)
}

rack_content_changed <- function(id, backend, data) {
  !identical(content_hash(data), rack_content_hash(id, backend))
}

# rack_delete ---------------------------------------------------------------

#' @rdname rack-backend
#' @export
rack_delete <- function(id, backend, ...) UseMethod("rack_delete")

#' @rdname rack-backend
#' @export
rack_purge <- function(id, backend, ...) UseMethod("rack_purge")

# rack_capabilities --------------------------------------------------------

#' @rdname rack-backend
#' @export
rack_capabilities <- function(backend, ...) UseMethod("rack_capabilities")

# rack_tags ----------------------------------------------------------------

#' @rdname rack-backend
#' @export
rack_tags <- function(id, backend, ...) UseMethod("rack_tags")

#' @rdname rack-backend
#' @export
rack_set_tags <- function(id, backend, tags, ...) UseMethod("rack_set_tags")

# rack_acl -----------------------------------------------------------------

#' @rdname rack-backend
#' @export
rack_acl <- function(id, backend, ...) UseMethod("rack_acl")

#' @rdname rack-backend
#' @export
rack_set_acl <- function(id, backend, acl_type, ...) UseMethod("rack_set_acl")

# rack_share ---------------------------------------------------------------

#' @rdname rack-backend
#' @export
rack_share <- function(id, backend, with_sub, ...) UseMethod("rack_share")

#' @rdname rack-backend
#' @export
rack_unshare <- function(id, backend, with_sub, ...) UseMethod("rack_unshare")

#' @rdname rack-backend
#' @export
rack_shares <- function(id, backend, ...) UseMethod("rack_shares")

# rack_find_users ----------------------------------------------------------

#' @rdname rack-backend
#' @export
rack_find_users <- function(backend, query, ...) UseMethod("rack_find_users")
