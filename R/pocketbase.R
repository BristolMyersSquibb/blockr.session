# Constructor & auth -----------------------------------------------------------

new_rack_backend_pb <- function(url, token, user_id,
                                reauth = NULL) {

  stopifnot(is_string(url), nzchar(url))
  stopifnot(is_string(token), nzchar(token))
  stopifnot(is_string(user_id), nzchar(user_id))

  auth <- new.env(parent = emptyenv())
  auth$token <- token

  structure(
    list(
      url = url,
      auth = auth,
      user_id = user_id,
      reauth = reauth
    ),
    class = "rack_backend_pb"
  )
}

new_rack_id_pb <- function(name, record_id = NULL,
                           version_id = NULL) {

  if (not_null(record_id) &&
        (!is_string(record_id) || !nzchar(record_id))) {
    blockr_abort(
      "rack_id_pb record_id must be a non-empty string.",
      class = "rack_id_pb_invalid_record_id"
    )
  }

  if (not_null(version_id) &&
        (!is_string(version_id) || !nzchar(version_id))) {
    blockr_abort(
      "rack_id_pb version_id must be a non-empty string.",
      class = "rack_id_pb_invalid_version_id"
    )
  }

  new_rack_id(
    name,
    record_id = record_id,
    version_id = version_id,
    class = "rack_id_pb"
  )
}

pb_authenticate <- function(vault_addr, vault_token,
                            vault_path =
                              "secret/data/pocketbase") {

  vault_request(
    vault_addr, vault_token, "GET",
    "/v1/auth/token/lookup-self"
  )

  creds <- vault_request(
    vault_addr, vault_token, "GET",
    paste0("/v1/", vault_path)
  )

  pb_url <- creds$data$data$url
  pb_email <- creds$data$data$email
  pb_password <- creds$data$data$password

  result <- pb_auth_password(
    pb_url, pb_email, pb_password
  )

  reauth <- function() {
    pb_auth_password(
      pb_url, pb_email, pb_password
    )$token
  }

  new_rack_backend_pb(
    url = pb_url,
    token = result$token,
    user_id = result$record$id,
    reauth = reauth
  )
}

# Internal helpers ------------------------------------------------------------

vault_request <- function(addr, token, method, path) {

  req <- httr2::request(paste0(addr, path))
  req <- httr2::req_method(req, method)
  req <- httr2::req_headers(
    req, `X-Vault-Token` = token
  )
  req <- httr2::req_error(
    req,
    body = function(resp) {
      httr2::resp_body_string(resp)
    }
  )

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

pb_auth_password <- function(url, identity, password) {

  path <- "/api/collections/users/auth-with-password"

  req <- httr2::request(paste0(url, path))
  req <- httr2::req_body_json(
    req,
    list(identity = identity, password = password),
    auto_unbox = TRUE
  )
  req <- httr2::req_error(
    req,
    body = function(resp) {
      httr2::resp_body_string(resp)
    }
  )

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

pb_request <- function(backend, method, path,
                       body = NULL, query = NULL) {

  perform <- function(token) {

    req <- httr2::request(paste0(backend$url, path))
    req <- httr2::req_method(req, method)
    req <- httr2::req_headers(
      req,
      Authorization = paste("Bearer", token)
    )

    if (length(query)) {
      req <- do.call(
        httr2::req_url_query, c(list(req), query)
      )
    }

    if (is.list(body)) {
      req <- httr2::req_body_json(
        req, body, auto_unbox = TRUE
      )
    }

    req <- httr2::req_error(
      req, is_error = function(resp) FALSE
    )

    httr2::req_perform(req)
  }

  resp <- perform(backend$auth$token)

  if (httr2::resp_status(resp) == 401L &&
        !is.null(backend$reauth)) {
    backend$auth$token <- backend$reauth()
    resp <- perform(backend$auth$token)
  }

  if (httr2::resp_is_error(resp)) {
    status <- httr2::resp_status(resp) # nolint: object_usage.
    msg <- tryCatch( # nolint: object_usage.
      httr2::resp_body_string(resp),
      error = function(e) {
        httr2::resp_status_desc(resp)
      }
    )
    blockr_abort(
      paste0(
        "PocketBase request failed ({status}): {msg}"
      ),
      class = "pb_request_error"
    )
  }

  if (method == "DELETE") {
    return(invisible(NULL))
  }

  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

pb_parse_time <- function(x) {
  as.POSIXct(
    sub("\\.\\d+Z$", "", x),
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )
}

pb_find_board <- function(backend, name) {

  filter <- paste0(
    "name=\"", name, "\"",
    " && owner=\"", backend$user_id, "\""
  )

  result <- pb_request(
    backend, "GET",
    "/api/collections/boards/records",
    query = list(filter = filter, perPage = 1L)
  )

  if (length(result$items) == 0L) {
    return(NULL)
  }

  result$items[[1L]]
}

pb_resolve_board <- function(id, backend) {

  if (not_null(id$record_id)) {
    return(
      pb_request(
        backend, "GET",
        paste0(
          "/api/collections/boards/records/",
          id$record_id
        )
      )
    )
  }

  board <- pb_find_board(backend, id$name)

  if (is.null(board)) {
    blockr_abort(
      "No board found with name {id$name}.",
      class = "pb_board_not_found"
    )
  }

  board
}

pb_upsert_board <- function(backend, name) {

  existing <- pb_find_board(backend, name)

  if (not_null(existing)) {
    return(existing)
  }

  pb_request(
    backend, "POST",
    "/api/collections/boards/records",
    body = list(
      name = name,
      owner = backend$user_id,
      tags = list(blockr_session_tags()),
      acl_type = "acl"
    )
  )
}

# Accessors -------------------------------------------------------------------

#' @export
pin_name.rack_id_pb <- function(id, ...) id$name

#' @export
format.rack_id_pb <- function(x, ...) {
  v <- if (not_null(x$version_id)) {
    paste0("@", x$version_id)
  } else {
    ""
  }
  paste0("<rack_id_pb: ", x$name, v, ">")
}

#' @export
last_saved.rack_id_pb <- function(id, backend, ...) {
  info <- rack_info(id, backend)
  if (nrow(info) == 0L) return(NULL)
  info$created[1L]
}

# rack_list -------------------------------------------------------------------

#' @export
rack_list.rack_backend_pb <- function(backend,
                                      tags = NULL, ...) {

  filter <- paste0("owner=\"", backend$user_id, "\"")

  result <- pb_request(
    backend, "GET",
    "/api/collections/boards/records",
    query = list(
      filter = filter,
      sort = "-created",
      perPage = 200L
    )
  )

  items <- result$items
  if (length(items) == 0L) return(list())

  keep <- vapply(items, function(item) {
    has_tags(list(tags = item$tags))
  }, logical(1))
  items <- items[keep]

  if (not_null(tags)) {
    keep <- vapply(items, function(item) {
      has_tags(list(tags = item$tags), tags = tags)
    }, logical(1))
    items <- items[keep]
  }

  if (length(items) == 0L) return(list())

  lapply(items, function(item) {
    new_rack_id_pb(
      name = item$name,
      record_id = item$id
    )
  })
}

# rack_info -------------------------------------------------------------------

#' @export
rack_info.rack_id_pb <- function(id, backend, ...) {

  empty_info <- data.frame(
    version = character(),
    created = as.POSIXct(character()),
    hash = character(),
    stringsAsFactors = FALSE
  )

  record_id <- id$record_id

  if (is.null(record_id)) {
    board <- pb_find_board(backend, id$name)
    if (is.null(board)) return(empty_info)
    record_id <- board$id
  }

  filter <- paste0("board=\"", record_id, "\"")

  result <- pb_request(
    backend, "GET",
    "/api/collections/board_versions/records",
    query = list(
      filter = filter,
      sort = "-created",
      perPage = 200L
    )
  )

  items <- result$items
  if (length(items) == 0L) return(empty_info)

  data.frame(
    version = vapply(
      items, function(v) v$id, character(1)
    ),
    created = pb_parse_time(
      vapply(
        items, function(v) v$created, character(1)
      )
    ),
    hash = NA_character_,
    stringsAsFactors = FALSE
  )
}

# rack_load -------------------------------------------------------------------

#' @export
rack_load.rack_id_pb <- function(id, backend, ...) {

  version_id <- id$version_id

  if (is.null(version_id)) {
    info <- rack_info(id, backend)

    if (nrow(info) == 0L) {
      blockr_abort(
        "No versions found for board {id$name}.",
        class = "rack_load_no_versions"
      )
    }

    version_id <- info$version[1L]
  }

  record <- pb_request(
    backend, "GET",
    paste0(
      "/api/collections/board_versions/records/",
      version_id
    )
  )

  switch(
    record$format,
    v1 = jsonlite::fromJSON(
      record$data,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    ),
    blockr_abort(
      "Unrecognized file format {record$format}.",
      class = "unknown_file_format"
    )
  )
}

# rack_save -------------------------------------------------------------------

#' @export
rack_save.rack_backend_pb <- function(backend, data, ...,
                                      name) {

  name <- sanitize_pin_name(name)

  board_record <- pb_upsert_board(backend, name)

  data_json <- as.character(
    jsonlite::toJSON(
      data, null = "null", auto_unbox = TRUE
    )
  )

  version_record <- pb_request(
    backend, "POST",
    "/api/collections/board_versions/records",
    body = list(
      board = board_record$id,
      data = data_json,
      format = "v1"
    )
  )

  new_rack_id_pb(
    name = name,
    record_id = board_record$id,
    version_id = version_record$id
  )
}

# rack_delete -----------------------------------------------------------------

#' @export
rack_delete.rack_id_pb <- function(id, backend, ...) {

  version_id <- id$version_id

  if (is.null(version_id)) {
    info <- rack_info(id, backend)

    if (nrow(info) == 0L) {
      blockr_abort(
        "No versions found for board {id$name}.",
        class = "rack_delete_no_versions"
      )
    }

    version_id <- info$version[1L]
  }

  pb_request(
    backend, "DELETE",
    paste0(
      "/api/collections/board_versions/records/",
      version_id
    )
  )

  invisible(TRUE)
}

#' @export
rack_purge.rack_id_pb <- function(id, backend, ...) {

  record_id <- id$record_id

  if (is.null(record_id)) {
    board <- pb_find_board(backend, id$name)

    if (is.null(board)) {
      blockr_abort(
        "No board found with name {id$name}.",
        class = "rack_purge_not_found"
      )
    }

    record_id <- board$id
  }

  pb_request(
    backend, "DELETE",
    paste0(
      "/api/collections/boards/records/",
      record_id
    )
  )

  invisible(TRUE)
}

# rack_capabilities -----------------------------------------------------------

#' @export
rack_capabilities.rack_backend_pb <- function(backend,
                                              ...) {
  list(
    versioning = TRUE,
    tags = TRUE,
    metadata = TRUE,
    sharing = TRUE,
    visibility = TRUE,
    user_discovery = TRUE
  )
}

# rack_tags -------------------------------------------------------------------

#' @export
rack_tags.rack_id_pb <- function(id, backend, ...) {
  board <- pb_resolve_board(id, backend)
  setdiff(unlist(board$tags), blockr_session_tags())
}

#' @export
rack_set_tags.rack_id_pb <- function(id, backend,
                                     tags, ...) {
  board <- pb_resolve_board(id, backend)
  all_tags <- unique(c(tags, blockr_session_tags()))

  pb_request(
    backend, "PATCH",
    paste0(
      "/api/collections/boards/records/", board$id
    ),
    body = list(tags = as.list(all_tags))
  )

  invisible(id)
}

# rack_acl --------------------------------------------------------------------

#' @export
rack_acl.rack_id_pb <- function(id, backend, ...) {
  board <- pb_resolve_board(id, backend)
  board$acl_type
}

#' @export
rack_set_acl.rack_id_pb <- function(id, backend,
                                    acl_type, ...) {
  board <- pb_resolve_board(id, backend)

  pb_request(
    backend, "PATCH",
    paste0(
      "/api/collections/boards/records/", board$id
    ),
    body = list(acl_type = acl_type)
  )

  invisible(id)
}

# rack_share ------------------------------------------------------------------

#' @export
rack_share.rack_id_pb <- function(id, backend,
                                  with_sub, ...) {
  board <- pb_resolve_board(id, backend)
  body <- list()
  body[["shared_with+"]] <- with_sub

  pb_request(
    backend, "PATCH",
    paste0(
      "/api/collections/boards/records/", board$id
    ),
    body = body
  )

  invisible(id)
}

#' @export
rack_unshare.rack_id_pb <- function(id, backend,
                                    with_sub, ...) {
  board <- pb_resolve_board(id, backend)
  body <- list()
  body[["shared_with-"]] <- with_sub

  pb_request(
    backend, "PATCH",
    paste0(
      "/api/collections/boards/records/", board$id
    ),
    body = body
  )

  invisible(id)
}

#' @export
rack_shares.rack_id_pb <- function(id, backend, ...) {

  board <- pb_resolve_board(id, backend)

  result <- pb_request(
    backend, "GET",
    paste0(
      "/api/collections/boards/records/", board$id
    ),
    query = list(expand = "shared_with")
  )

  expand <- result$expand
  if (is.null(expand) ||
        is.null(expand$shared_with)) {
    return(list())
  }

  lapply(expand$shared_with, function(u) {
    display <- trimws(paste(
      coal(u$name, ""), coal(u$email, "")
    ))
    if (!nzchar(display)) {
      display <- coal(u$username, u$id)
    }

    list(
      principal_guid = u$id,
      principal_type = "user",
      role = "viewer",
      display_name = display
    )
  })
}

# rack_find_users -------------------------------------------------------------

#' @export
rack_find_users.rack_backend_pb <- function(backend,
                                            query,
                                            ...) {

  safe_q <- gsub("\"", "\\\\\"", query)
  filter <- paste0(
    "(name~\"", safe_q,
    "\" || email~\"", safe_q, "\")"
  )

  result <- pb_request(
    backend, "GET",
    "/api/collections/users/records",
    query = list(filter = filter, perPage = 20L)
  )

  lapply(result$items, function(u) {
    parts <- strsplit(
      coal(u$name, ""), "\\s+", perl = TRUE
    )[[1L]]

    list(
      guid = u$id,
      username = coal(u$username, u$email),
      first_name = if (length(parts) > 0L) {
        parts[1L]
      } else {
        ""
      },
      last_name = if (length(parts) > 1L) {
        paste(parts[-1L], collapse = " ")
      } else {
        ""
      },
      email = coal(u$email, "")
    )
  })
}
