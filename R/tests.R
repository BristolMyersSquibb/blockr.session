mock_board_connect <- function(account = "user_a",
                               url = "https://connect.example.com",
                               key = "fake-key-a") {

  structure(
    list(
      board = "pins_board_connect",
      api = c(0, 1),
      cache = tempdir(),
      name = "posit-connect",
      url = url,
      account = account,
      server_name = gsub("^https?://", "", url),
      auth = key,
      versioned = TRUE,
      use_cache_on_failure = FALSE
    ),
    class = c("pins_board_connect", "pins_board")
  )
}

connect_fixture <- function(name, record = NULL, cleanup = NULL) {

  fixtures_dir <- testthat::test_path("fixtures", "connect")
  json_name <- paste0(name, ".json")
  json_path <- file.path(fixtures_dir, json_name)

  if (connect_recording()) {

    if (is.null(record)) {
      blockr_abort(
        "connect_fixture({name}) needs a record function in recording mode.",
        class = "connect_fixture_no_recorder"
      )
    }

    if (!dir.exists(fixtures_dir)) {
      dir.create(fixtures_dir, recursive = TRUE)
    }

    val <- record()

    json_text <- jsonlite::serializeJSON(val, pretty = TRUE)

    subs <- getOption("blockr.connect_fixture_subs")

    if (not_null(subs)) {
      for (i in seq_along(subs)) {
        json_text <- gsub(names(subs)[i], subs[i], json_text, fixed = TRUE)
      }
    }

    writeLines(json_text, json_path)

    if (!is.null(cleanup)) {
      withr::defer(cleanup(), envir = parent.frame())
    }
  }

  if (!file.exists(json_path)) {
    msg <- paste0(
      "Connect fixture '", name, "' not found"
    )
    testthat::skip(msg)
  }

  jsonlite::unserializeJSON(readLines(json_path, warn = FALSE))
}

connect_recording <- function() {
  isTRUE(getOption("blockr.connect_recording"))
}
