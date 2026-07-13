# Constructor --------------------------------------------------------------

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

#' @export
rack_id_from_input.pins_board_connect <- function(backend, x, ...) {

  user <- if (not_null(x$user) && nzchar(x$user)) x$user else backend$account

  new_rack_id_pins_connect(user, input_id(x), input_version(x))
}

# pin_name / format ---------------------------------------------------------

#' @export
pin_name.rack_id_pins_connect <- function(id, ...) {
  paste0(id$user, "/", id$id)
}

#' @export
format.rack_id_pins_connect <- function(x, ...) {
  v <- if (not_null(x$version)) paste0("@", x$version) else ""
  paste0("<rack_id_pins_connect: ", x$user, "/", x$id, v, ">")
}

# rack_name / rack_rename ---------------------------------------------------

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

# rack_list -----------------------------------------------------------------

#' @export
rack_list.pins_board_connect <- function(backend, tags = NULL, ...) {

  if (not_null(tags)) {
    return(connect_list_tagged(backend, tags))
  }

  items <- tryCatch(
    connect_api(backend, "GET /content"),
    error = function(e) NULL
  )

  if (!length(items)) {
    return(list())
  }

  records <- list()

  for (item in items) {

    if (!identical(item$content_category, "pin")) {
      next
    }

    membership <- connect_membership$lookup(backend, item)

    if (isTRUE(membership$member)) {
      records[[length(records) + 1L]] <- new_rack_record(
        id = item$name,
        name = connect_item_title(item),
        user = membership$user,
        saved = connect_item_saved(item)
      )
    }
  }

  records
}

connect_list_tagged <- function(backend, tags) {

  df <- filter_workflows(pins::pin_search(backend), tags)

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
        user = parts[1L],
        saved = df$created[i]
      )
    }
  )
}

connect_item_title <- function(item) {
  if (not_null(item$title) && nzchar(item$title)) item$title else item$name
}

connect_item_saved <- function(item) {

  stamp <- item$last_deployed_time

  if (is.null(stamp) || !nzchar(stamp)) {
    return(NULL)
  }

  connect_parse_time(stamp)
}

# Whether a pin is a blockr workflow -- and who owns it -- is intrinsic to the
# pin, identical for every viewer, so the check memoizes safely in one
# process-wide map keyed by slug: a pin is inspected once, the first time a
# listing meets it, and the result is reused thereafter.
connect_membership <- local({

  members <- list()

  reset <- function() {
    members <<- list()
  }

  lookup <- function(backend, item) {

    slug <- item$name

    if (is.null(members[[slug]])) {
      members[[slug]] <<- connect_probe_membership(backend, item)
    }

    members[[slug]]
  }

  list(lookup = lookup, reset = reset)
})

connect_probe_membership <- function(backend, item) {

  owner <- tryCatch(
    connect_api(backend, "GET /users/{item$owner_guid}")$username,
    error = function(e) NULL
  )

  owner <- coal(owner, backend$account, fail_all = FALSE)

  meta <- tryCatch(
    pins::pin_meta(backend, paste0(owner, "/", item$name)),
    error = function(e) NULL
  )

  list(member = not_null(meta) && has_tags(meta), user = owner)
}

# rack_upload ---------------------------------------------------------------

#' @export
rack_upload.pins_board_connect <- function(backend, path, id, name = NULL,
                                           content_hash = NULL, ...) {

  slug <- id$id
  qualified <- paste0(backend$account, "/", slug)

  # pins defaults a missing title to boilerplate and PATCHes the content title
  # on every write, so an append with no title silently resets it. Pass the
  # intended title through -- the new name on create, the current stored title
  # on append -- so pins re-asserts the right one instead of the boilerplate.
  title <- if (not_null(name)) name else rack_name(id, backend)

  metadata <- list(format = "v1")

  if (not_null(content_hash)) {
    metadata[["content_hash"]] <- content_hash
  }

  log_debug("Connect pin upload target {qualified}")

  pins::pin_upload(
    backend,
    path,
    qualified,
    title = title,
    versioned = TRUE,
    metadata = metadata,
    tags = blockr_session_tags()
  )

  base <- new_rack_id_pins_connect(backend$account, slug)

  new_rack_id_pins_connect(
    user = backend$account,
    id = slug,
    version = rack_info(base, backend)$version[1L]
  )
}

# rack_capabilities --------------------------------------------------------

#' @export
rack_capabilities.pins_board_connect <- function(backend, ...) {

  session <- get_session()
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

# rack_acl -----------------------------------------------------------------

#' @export
rack_acl.rack_id_pins_connect <- function(id, backend, ...) {
  content <- connect_content_find(backend, id$id)
  content$access_type
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
rack_find_users.pins_board_connect <- function(backend, query, ...) {
  result <- connect_api(backend, "GET /users", query = list(prefix = query))
  result$results
}

# Connect API helpers (internal) -------------------------------------------

connect_api <- function(board, route, ..., body = NULL, query = NULL,
                        env = parent.frame()) {

  url <- paste0(board$url, "/__api__/v1")

  req <- httr2::request(url)
  req <- httr2::req_template(req, glue::glue(route, .envir = env))
  req <- httr2::req_headers(req, Authorization = paste("Key", board$auth))

  if (length(query)) {
    req <- do.call(httr2::req_url_query, c(list(req), query))
  }

  if (is.list(body)) {
    req <- httr2::req_body_json(req, body)
  }

  req <- httr2::req_error(req,
    body = function(resp) httr2::resp_body_string(resp)
  )

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

connect_content_find <- function(board, name) {

  results <- connect_api(board, "GET /content", query = list(name = name))

  if (length(results) == 0L) {
    blockr_abort(
      "No content found with name {name}.",
      class = "rack_content_not_found"
    )
  }

  results[[1L]]
}

connect_content_titles <- function(board) {

  results <- connect_api(board, "GET /content")

  out <- list()

  for (item in results) {

    slug <- item$name

    if (is.null(slug) || !nzchar(slug)) {
      next
    }

    out[[slug]] <- if (not_null(item$title) && nzchar(item$title)) {
      item$title
    } else {
      slug
    }
  }

  out
}

connect_parse_time <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
}
