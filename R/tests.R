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

    norm <- fixture_normalize_obj(val)
    json_text <- jsonlite::serializeJSON(norm$val, pretty = TRUE)

    subs <- connect_test_substitutions
    ord <- order(nchar(names(subs)), decreasing = TRUE)

    for (i in ord) {
      json_text <- gsub(names(subs)[i], subs[i], json_text, fixed = TRUE)
    }

    json_text <- fixture_normalize_json(json_text)

    writeLines(json_text, json_path)

    if (!is.null(cleanup)) {
      withr::defer(cleanup(), envir = envir)
    }

    # Return identity-substituted but not pattern-normalized value so
    # subsequent record() calls can use real server IDs (versions, etc.)
    ret_json <- jsonlite::serializeJSON(val, pretty = TRUE)
    for (i in ord) {
      ret_json <- gsub(names(subs)[i], subs[i], ret_json, fixed = TRUE)
    }
    return(jsonlite::unserializeJSON(ret_json))
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

fixture_normalize_obj <- function(x) {
  timestamps <- fixture_collect_ts(x)
  if (length(timestamps) == 0L) {
    return(list(val = x, ts_offset = 0))
  }

  anchor <- 1577836800 # 2020-01-01 00:00:00 UTC
  ts_offset <- anchor - min(timestamps)

  list(val = fixture_shift_ts(x, ts_offset), ts_offset = ts_offset)
}

fixture_collect_ts <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(as.numeric(x))
  }
  if (is.character(x)) {
    iso_re <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$"
    iso <- grep(iso_re, x, value = TRUE, perl = TRUE)
    if (length(iso)) {
      return(vapply(iso, function(s) {
        as.numeric(as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
      }, numeric(1), USE.NAMES = FALSE))
    }
  }
  if (is.list(x)) {
    return(unlist(lapply(x, fixture_collect_ts), use.names = FALSE))
  }
  numeric(0L)
}

fixture_shift_ts <- function(x, offset) {
  if (inherits(x, "POSIXct")) {
    tz <- attr(x, "tzone")
    if (is.null(tz)) tz <- ""
    return(.POSIXct(as.numeric(x) + offset, tz = tz))
  }
  if (is.character(x)) {
    iso_re <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$"
    iso_idx <- grep(iso_re, x, perl = TRUE)
    for (i in iso_idx) {
      ts <- as.numeric(as.POSIXct(x[i], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
      x[i] <- format(
        .POSIXct(ts + offset, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
      )
    }
    return(x)
  }
  if (is.list(x)) {
    x[] <- lapply(x, fixture_shift_ts, offset = offset)
  }
  x
}

fixture_normalize_json <- function(json_text) {
  # UUIDs not already normalized (skip 00000000-... from identity subs)
  uuid_re <- paste0(
    "(?!00000000)[0-9a-f]{8}-[0-9a-f]{4}-",
    "[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
  )
  m <- gregexpr(uuid_re, json_text, perl = TRUE)
  uuid_vals <- unique(regmatches(json_text, m)[[1]])
  for (i in seq_along(uuid_vals)) {
    json_text <- gsub(
      uuid_vals[i],
      sprintf("00000000-0000-4000-b000-%012d", i),
      json_text, fixed = TRUE
    )
  }

  # Board cache hash (connect-<md5>)
  json_text <- gsub(
    "connect-[0-9a-f]{32}",
    "connect-00000000000000000000000000000000",
    json_text, perl = TRUE
  )

  # Tempfile names (file<hex>.json)
  tmp_re <- "file[0-9a-f]{6,}\\.json"
  m <- gregexpr(tmp_re, json_text, perl = TRUE)
  tmp_vals <- unique(regmatches(json_text, m)[[1]])
  if (length(tmp_vals)) {
    phs <- sprintf("__TMP_%04d__", seq_along(tmp_vals))
    for (i in seq_along(tmp_vals)) {
      json_text <- gsub(tmp_vals[i], phs[i], json_text, fixed = TRUE)
    }
    for (i in seq_along(tmp_vals)) {
      json_text <- gsub(
        phs[i], sprintf("file%04d.json", i), json_text, fixed = TRUE
      )
    }
  }

  # Numeric string IDs (version/bundle/permission) — two-pass via placeholders
  numid_re <- '"\\d+"'
  m <- gregexpr(numid_re, json_text, perl = TRUE)
  numid_matches <- unique(regmatches(json_text, m)[[1]])
  if (length(numid_matches)) {
    numid_vals <- gsub('"', "", numid_matches)
    ord <- order(nchar(numid_vals), decreasing = TRUE)
    numid_vals <- numid_vals[ord]

    phs <- sprintf("__NUM_%04d__", seq_along(numid_vals))
    for (i in seq_along(numid_vals)) {
      # Quoted value context: "798"
      json_text <- gsub(
        paste0('"', numid_vals[i], '"'),
        paste0('"', phs[i], '"'),
        json_text, fixed = TRUE
      )
      # URL version context: _rev798/
      json_text <- gsub(
        paste0("_rev", numid_vals[i], "/"),
        paste0("_rev", phs[i], "/"),
        json_text, fixed = TRUE
      )
      # Path version context: /798"
      json_text <- gsub(
        paste0("/", numid_vals[i], '"'),
        paste0("/", phs[i], '"'),
        json_text, fixed = TRUE
      )
    }
    for (i in seq_along(numid_vals)) {
      json_text <- gsub(
        phs[i], as.character(99L + i), json_text, fixed = TRUE
      )
    }
  }

  json_text
}
