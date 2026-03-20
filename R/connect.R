# Connect API helpers (internal) -------------------------------------------

connect_api <- function(board, method, path, body = NULL) {

  req <- httr2::request(paste0(board$url, "/__api__/v1", path))
  req <- httr2::req_headers(req, Authorization = paste("Key", board$auth))
  req <- httr2::req_method(req, method)

  if (!is.null(body)) {
    req <- httr2::req_body_json(req, body)
  }

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

connect_content_find <- function(board, name) {

  results <- connect_api(
    board, "GET",
    paste0("/content?name=", utils::URLencode(name, reserved = TRUE))
  )

  if (length(results) == 0L) {
    blockr_abort(
      "No content found with name {name}.",
      class = "rack_content_not_found"
    )
  }

  results[[1L]]
}
