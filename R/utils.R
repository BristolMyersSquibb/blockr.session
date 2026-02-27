.blockr_reload_state <- new.env(parent = emptyenv())

get_and_clear_reload_url <- function() {
  url <- .blockr_reload_state$url
  .blockr_reload_state$url <- NULL
  url
}

set_reload_url <- function(url) {
  .blockr_reload_state$url <- url
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
    notify(
      HTML(cli::ansi_html(conditionMessage(cnd))),
      type = type,
      session = session
    )

    return_val
  }
}

blockr_session_tags <- function() "blockr-session"

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
