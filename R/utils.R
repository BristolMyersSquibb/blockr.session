url_params_enabled <- function() {
  isTRUE(blockr_option("session_url_params", FALSE))
}

get_session_backend <- function() {

  val <- blockr_option("session_mgmt_backend", pins::board_local)

  if (is.function(val)) {
    val <- val()
  }

  if (!inherits(val, "pins_board")) {
    blockr_abort(
      paste(
        "The `session_mgmt_backend` option must be a pins board or a",
        "function that returns one, got {class(val)[[1L]]}."
      ),
      class = "invalid_session_backend"
    )
  }

  val
}

format_time_ago <- function(time) {

  if (is.character(time)) {
    time <- as.POSIXct(time)
  }

  diff_secs <- as.numeric(difftime(Sys.time(), time, units = "secs"))

  if (diff_secs < 60) {
    "Just now"
  } else if (diff_secs < 3600) {
    mins <- floor(diff_secs / 60)
    paste0(mins, " min", if (mins > 1) "s" else "", " ago")
  } else if (diff_secs < 86400) {
    hours <- floor(diff_secs / 3600)
    paste0(hours, " hour", if (hours > 1) "s" else "", " ago")
  } else if (diff_secs < 604800) {
    days <- floor(diff_secs / 86400)
    paste0(days, " day", if (days > 1) "s" else "", " ago")
  } else if (diff_secs < 2592000) {
    weeks <- floor(diff_secs / 604800)
    paste0(weeks, " week", if (weeks > 1) "s" else "", " ago")
  } else {
    format(time, "%b %d, %Y")
  }
}

get_initials <- function(username) {

  if (is.null(username) || username == "") {
    return("U")
  }

  parts <- strsplit(username, "[._@ -]")[[1]]
  parts <- parts[parts != ""]

  if (length(parts) >= 2) {
    paste0(
      toupper(substr(parts[1], 1, 1)),
      toupper(substr(parts[2], 1, 1))
    )
  } else {
    toupper(substr(username, 1, min(2, nchar(username))))
  }
}

has_tags <- function(x, tags = blockr_session_tags()) {
  all(tags %in% x[["tags"]])
}

cnd_to_notif <- function(return_val = NULL, type = "warning",
                         session = get_session()) {

  function(cnd) {
    msg <- conditionMessage(cnd)
    # Escape curly braces so cli/glue won't interpret them (e.g. JSON in
    # Connect API error bodies like {"code":5, ...} would crash glue::glue).
    msg <- gsub("{", "{{", msg, fixed = TRUE)
    msg <- gsub("}", "}}", msg, fixed = TRUE)

    notify(
      msg,
      type = type,
      session = session
    )

    return_val
  }
}

sanitize_pin_name <- function(name) {
  name <- gsub("[^[:alnum:]._-]", "_", name)
  name <- gsub("_+", "_", name)
  name <- gsub("^[_.-]+|[_.-]+$", "", name)
  if (nchar(name) < 3L) {
    name <- paste0(name, strrep("x", 3L - nchar(name)))
  }
  if (nchar(name) > 64L) {
    name <- substr(name, 1L, 64L)
  }
  name
}

blockr_session_tags <- function() "blockr-session"

# Normalize JS array-of-objects input from Shiny.setInputValue.
# Shiny may deliver as:
#   - list of lists (ideal) -> use as-is
#   - data.frame (multiple objects) -> split into list of rows
#   - named atomic vector (single object) -> wrap in list
#   - named atomic vector with repeated keys (multiple objects flattened)
#     e.g. c(name="a", user="", name="b", user="") -> split into list of objects
normalize_js_input <- function(x) {
  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, ])))
  }

  if (!is.atomic(x)) {
    return(x)
  }

  nms <- names(x)
  if (is.null(nms)) {
    return(list(as.list(x)))
  }

  unique_keys <- unique(nms)
  n_keys <- length(unique_keys)
  n_total <- length(x)

  if (n_keys > 0 && n_total > n_keys && n_total %% n_keys == 0) {
    n_objects <- n_total %/% n_keys
    return(lapply(seq_len(n_objects), function(i) {
      start <- (i - 1L) * n_keys + 1L
      end <- i * n_keys
      as.list(x[start:end])
    }))
  }

  list(as.list(x))
}

safe_restore_board <- function(board, board_ser, restore_result,
                               meta = list(), session) {
  tryCatch(
    {
      restore_board(
        board, board_ser, restore_result,
        meta = meta, session = session
      )
      TRUE
    },
    error = cnd_to_notif(return_val = FALSE, type = "error", session = session)
  )
}

reset_board_name <- function(board, name) {
  if ("board_name" %in% board_option_ids(board)) {
    board_options(board) <- combine_board_options(
      new_board_name_option(name),
      board_options(board)
    )
  }
  board
}

board_query_string <- function(id, backend) {

  params <- list(board_name = display_name(id))

  if (not_null(id$user) && !identical(id$user, backend$account)) {
    params$user <- id$user
  }

  if (not_null(id$version)) {
    params$version <- id$version
  }

  paste0(
    "?",
    paste(names(params), params, sep = "=", collapse = "&")
  )
}
