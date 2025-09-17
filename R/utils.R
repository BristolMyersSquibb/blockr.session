reval <- function(x) x()

reval_if <- function(x) if (is.function(x)) x() else x

pin_versions <- function(name, board) {

  res <- tryCatch(
    pins::pin_versions(board, name),
    pins_pin_missing = function(e) NULL,
    error = cnd_to_notif()
  )

  if (is.null(res)) {
    return(character())
  }

  set_names(
    res$version,
    paste0(res$created, " (", res$hash, ")")
  )
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
