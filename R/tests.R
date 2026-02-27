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

utils::globalVariables(
  c(
    "connect_test_recording",
    "connect_test_substitutions"
  )
)

makeActiveBinding(
  "connect_test_recording",
  local(
    {
      state <- FALSE

      function(val) {

        if (missing(val)) {
          return(state)
        }

        stopifnot(is_bool(val))
        state <<- val
      }
    }
  ),
  environment()
)

makeActiveBinding(
  "connect_test_substitutions",
  local(
    {
      state <- character()

      function(val) {

        if (missing(val)) {
          return(state)
        }

        stopifnot(
          is.character(val),
          length(unique(names(val))) == length(val)
        )

        state <<- val
      }
    }
  ),
  environment()
)

connect_fixture <- function(name, record = NULL, cleanup = NULL,
                            envir = parent.frame()) {

  fixtures_dir <- testthat::test_path("_fixtures", "connect")
  json_name <- paste0(name, ".json")
  json_path <- file.path(fixtures_dir, json_name)

  if (connect_test_recording) {

    if (is.null(record)) {
      stop(
        "connect_fixture(\"", name, "\") needs a `record` function in ",
        "recording mode.",
        call. = FALSE
      )
    }

    if (!dir.exists(fixtures_dir)) {
      dir.create(fixtures_dir, recursive = TRUE)
    }

    val <- record()

    json_text <- jsonlite::serializeJSON(val, pretty = TRUE)

    for (i in seq_along(connect_test_substitutions)) {
      json_text <- gsub(
        names(connect_test_substitutions)[i],
        connect_test_substitutions[i],
        json_text,
        fixed = TRUE
      )
    }

    writeLines(json_text, json_path)

    if (!is.null(cleanup)) {
      withr::defer(cleanup(), envir = envir)
    }
  }

  if (!file.exists(json_path)) {
    testthat::skip(
      paste0("Connect fixture '", name, "' not found")
    )
  }

  jsonlite::unserializeJSON(
    readLines(json_path, warn = FALSE)
  )
}
