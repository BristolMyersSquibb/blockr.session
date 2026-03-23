mock_backend_pb <- function(
    url = "https://pb.example.com",
    token = "fake-token-a",
    user_id = "user001") {
  new_rack_backend_pb(url, token, user_id) # nolint: object_usage.
}

# Build a sequence-based mock for pb_request. Each call
# returns the next response from the list.
mock_pb_responses <- function(...) {
  responses <- list(...)
  env <- new.env(parent = emptyenv())
  env$idx <- 0L

  function(backend, method, path,
           body = NULL, query = NULL) {
    env$idx <- env$idx + 1L
    if (env$idx > length(responses)) {
      stop(
        "Unexpected pb_request call #", env$idx,
        " (", method, " ", path, ")",
        call. = FALSE
      )
    }
    resp <- responses[[env$idx]]
    if (is.function(resp)) {
      resp(method, path, body, query)
    } else {
      resp
    }
  }
}

# Helpers for building PB API responses

pb_board_record <- function(
    id = "board001",
    name = "test-board",
    owner = "user001",
    tags = list("blockr-session"),
    acl_type = "acl",
    shared_with = list()) {
  list(
    id = id,
    name = name,
    owner = owner,
    tags = tags,
    acl_type = acl_type,
    shared_with = shared_with,
    created = "2024-01-01 00:00:00.000Z",
    updated = "2024-01-01 00:00:00.000Z"
  )
}

pb_version_record <- function(
    id = "ver001",
    board = "board001",
    data = NULL,
    format = "v1") {
  if (is.null(data)) {
    data <- paste0(
      '{"blocks":{"a":{"type":"dataset_block"}},',
      '"links":[],"format":"test"}'
    )
  }
  list(
    id = id,
    board = board,
    data = data,
    format = format,
    created = "2024-01-01 00:00:00.000Z"
  )
}

pb_empty_list <- function() {
  list(
    page = 1L,
    perPage = 200L,
    totalItems = 0L,
    items = list()
  )
}

pb_list_response <- function(...) {
  items <- list(...)
  list(
    page = 1L,
    perPage = 200L,
    totalItems = length(items),
    items = items
  )
}

pb_user_record <- function(
    id = "usr001",
    username = "jsmith",
    name = "John Smith",
    email = "john@example.com") {
  list(
    id = id,
    username = username,
    name = name,
    email = email,
    created = "2024-01-01 00:00:00.000Z"
  )
}
