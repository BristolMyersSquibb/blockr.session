reval <- function(x) x()

reval_if <- function(x) if (is.function(x)) x() else x

pin_versions <- function(name, board) {

  res <- tryCatch(
    pins::pin_versions(board, name),
    pins_pin_missing = function(e) NULL,
    error = cnd_to_notif()
  )

  if (is.null(res) || nrow(res) == 0L) {
    return(character())
  }

  if ("hash" %in% colnames(res)) {
    names <- paste0(res$created, " (", res$hash, ")")
  } else {
    names <- res$created
  }

  set_names(res$version, names)
}

pin_list <- function(backend) {
  tryCatch(
    {
      res <- pins::pin_list(backend)

      if (inherits(backend, "pins_board_connect")) {
        res[lgl_ply(res, has_tags, backend)]
      }

      res
    },
    error = cnd_to_notif()
  )
}

has_tags <- function(x, backend, tags = blockr_session_tags()) {
  all(tags %in% pins::pin_meta(backend, x)[["tag"]])
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
