reval <- function(x) x()

reval_if <- function(x) if (is.function(x)) x() else x

#' Format time ago
#'
#' Converts a timestamp to a human-readable "time ago" string.
#'
#' @param time POSIXct or character timestamp
#' @return Character string like "2 hours ago", "3 days ago"
#' @keywords internal
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

#' List all workflows
#'
#' Returns a data frame of all saved workflows with metadata.
#'
#' @param backend A pins board
#' @return Data frame with columns: name, created, time_ago
#' @keywords internal
list_workflows <- function(backend) {
  df <- tryCatch(
    pins::pin_search(backend),
    error = function(e) data.frame()
  )

  if (nrow(df) == 0) {
    return(data.frame(
      name = character(),
      created = as.POSIXct(character()),
      time_ago = character(),
      stringsAsFactors = FALSE
    ))
  }

  df <- df[, c("name", "created"), drop = FALSE]
  df <- df[order(df$created, decreasing = TRUE, na.last = TRUE), ]

  df$time_ago <- vapply(df$created, function(t) {
    if (is.na(t)) "" else format_time_ago(t)
  }, character(1))

  rownames(df) <- NULL
  df
}

pin_versions <- function(name, board) {

  res <- tryCatch(
    pins::pin_versions(board, name),
    pins_pin_missing = function(e) NULL,
    error = cnd_to_notif()
  )

  if (is.null(res) || nrow(res) == 0L) {
    return(character())
  }

  res <- res[order(res$created, decreasing = TRUE), ]

  if ("hash" %in% colnames(res)) {
    names <- paste0(res$created, " (", res$hash, ")")
  } else {
    names <- res$created
  }

  set_names(res$version, names)
}

pin_list <- function(backend) {

  res <- tryCatch(
    pins::pin_list(backend),
    error = cnd_to_notif()
  )

  c(`Select board` = "", res)
}

has_tags <- function(x, tags = blockr_session_tags()) {
  all(tags %in% x[["tags"]])
}

cnd_to_notif <- function(return_val = NULL, type = "warning",
                         session = get_session()) {

  function(cnd) {
    showNotification(
      HTML(cli::ansi_html(conditionMessage(cnd))),
      type = type,
      session = session
    )

    return_val
  }
}

blockr_session_tags <- function() "blockr-session"

update_versions <- function(name, backend, session = get_session()) {
  if (nchar(name)) {
    updateSelectInput(
      session,
      "pin_version",
      choices = pin_versions(name, backend)
    )
  }
}

reset_versions <- function(session = get_session()) {
  updateSelectInput(
    session,
    "pin_version",
    choices = c(`Select board` = "")
  )
}

update_pins <- function(backend, session = get_session()) {
  updateSelectInput(
    session,
    "pin_name",
    choices = pin_list(backend)
  )
}
