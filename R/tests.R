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

.connect <- local({

  recording <- FALSE
  subs <- NULL

  env <- new.env(parent = emptyenv())

  makeActiveBinding("recording", function(val) {
    if (missing(val)) recording
    else recording <<- val
  }, env)

  makeActiveBinding("subs", function(val) {
    if (missing(val)) subs
    else subs <<- val
  }, env)

  env
})

connect_fixture <- function(name, record = NULL, cleanup = NULL) {

  fixtures_dir <- testthat::test_path("_fixtures", "connect")
  json_name <- paste0(name, ".json")
  json_path <- file.path(fixtures_dir, json_name)

  if (.connect$recording) {

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

    subs <- .connect$subs

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
    testthat::skip(
      paste0("Connect fixture '", name, "' not found")
    )
  }

  jsonlite::unserializeJSON(
    readLines(json_path, warn = FALSE)
  )
}

local_connect_options <- function(subs, .local_envir = parent.frame()) {

  old_recording <- .connect$recording
  old_subs <- .connect$subs

  .connect$recording <- TRUE
  .connect$subs <- subs

  withr::defer(
    {
      .connect$recording <- old_recording
      .connect$subs <- old_subs
    },
    envir = .local_envir
  )
}
