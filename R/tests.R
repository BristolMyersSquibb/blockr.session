#' Connect testing utilities
#'
#' Utilities for testing code that interacts with Posit Connect via pins.
#'
#' `fake_board_connect()` creates a mock board object with the correct S3
#' class hierarchy for dispatch without requiring a real server.
#'
#' `connect_fixture()` records or replays a Connect return value as a JSON
#' fixture. When `connect_recording()` is `TRUE`, it evaluates `record()`
#' and saves the result; otherwise it loads from disk or skips.
#'
#' `fixture_df()` converts a list of row-lists (as returned by
#' `connect_fixture()` for data.frame fixtures) into a proper data.frame,
#' preserving complex columns as list-columns.
#'
#' `connect_recording()` returns `TRUE` when the `CONNECT_SERVER`
#' environment variable is set, indicating a real Connect server is
#' available for fixture recording.
#'
#' @param account Connect username for the mock board.
#' @param url Connect server URL for the mock board.
#' @param key API key for the mock board.
#'
#' @return `fake_board_connect()` returns an S3 object of class
#'   `c("pins_board_connect", "pins_board")`.
#'
#' @rdname connect-testing
#' @export
fake_board_connect <- function(account = "user_a",
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

#' @param name Unique fixture name (e.g. `"pin_search"`). Determines the
#'   JSON filename under `tests/testthat/fixtures/connect/`.
#' @param record A zero-argument function that produces the value to
#'   record. Only called when [connect_recording()] is `TRUE`. Ignored
#'   during playback.
#'
#' @return `connect_fixture()` returns the recorded or replayed R object
#'   as a list (parsed JSON with no simplification).
#'
#' @rdname connect-testing
#' @export
connect_fixture <- function(name, record = NULL) {

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

    json_text <- jsonlite::toJSON(
      val,
      auto_unbox = TRUE,
      null = "null",
      POSIXt = "ISO8601",
      pretty = TRUE,
      force = TRUE
    )

    subs <- getOption("blockr.connect_fixture_subs")

    if (not_null(subs)) {
      for (i in seq_along(subs)) {
        json_text <- gsub(names(subs)[i], subs[i], json_text, fixed = TRUE)
      }
    }

    writeLines(json_text, json_path)
  }

  if (!file.exists(json_path)) {
    msg <- paste0(
      "Connect fixture '", name, "' not recorded (set CONNECT_SERVER)"
    )
    testthat::skip(msg)
  }

  jsonlite::read_json(json_path)
}

#' @param rows A list of row-lists as returned by [connect_fixture()]
#'   for a data.frame fixture. Each element represents one row.
#'
#' @return `fixture_df()` returns a data.frame. Scalar columns become
#'   character vectors; complex columns become list-columns.
#'
#' @rdname connect-testing
#' @export
fixture_df <- function(rows) {

  nms <- names(rows[[1L]])
  n <- length(rows)
  result <- vector("list", length(nms))
  names(result) <- nms

  for (nm in nms) {

    vals <- lapply(rows, `[[`, nm)

    is_scalar <- all(
      vapply(
        vals,
        function(v) is.atomic(v) && length(v) <= 1L,
        logical(1L)
      )
    )

    if (is_scalar) {
      result[[nm]] <- vapply(
        vals,
        function(v) if (is.null(v)) NA_character_ else as.character(v),
        character(1L)
      )
    } else {
      result[[nm]] <- vals
    }
  }

  structure(
    result,
    class = "data.frame",
    row.names = seq_len(n)
  )
}

#' @return `connect_recording()` returns a scalar logical.
#'
#' @rdname connect-testing
#' @export
connect_recording <- function() {
  nzchar(Sys.getenv("CONNECT_SERVER"))
}
