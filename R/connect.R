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
