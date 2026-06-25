#' Posit Connect storage backend with graceful fallback
#'
#' Resolves the storage backend for the `session_mgmt_backend`
#' [blockr.core::blockr_option()] -- of which it is the default -- in three
#' tiers, degrading instead of erroring on a missing credential:
#'
#' 1. When the visitor carries a Posit Connect user session token (the Connect
#'    API Integration is enabled), it is exchanged via `connectapi::connect()`
#'    for a viewer-scoped API key, so each user reads and writes pins under
#'    their own namespace ([pins::board_connect()]). Requires the
#'    \pkg{connectapi} package.
#' 2. With no visitor token but application Connect credentials in the
#'    environment (`CONNECT_SERVER` and `CONNECT_API_KEY`), a board on the
#'    application's own account.
#' 3. Otherwise (e.g. local development, off Connect), [pins::board_local()].
#'
#' @param session Shiny session whose request carries the Connect user session
#'   token; defaults to the current reactive domain.
#'
#' @return A `pins_board`: a viewer- or application-scoped `board_connect`, or
#'   `board_local` when no Connect credentials are available.
#'
#' @export
user_pins_board <- function(session = get_session()) {

  token <- connect_session_token(session$request)

  if (not_null(token)) {
    board <- connect_board(token)
    log_info("Resolved a Connect pin board for account {board$account}.")
    return(board)
  }

  if (has_connect_app_creds()) {
    log_info("No visitor token; using the application Connect pin board.")
    return(app_pins_board())
  }

  log_info("No Connect credentials found; using a local pins board.")

  pins::board_local()
}

has_connect_app_creds <- function() {
  nzchar(Sys.getenv("CONNECT_SERVER")) && nzchar(Sys.getenv("CONNECT_API_KEY"))
}

app_pins_board <- function() {
  pins::board_connect(
    auth = "manual",
    server = Sys.getenv("CONNECT_SERVER"),
    key = Sys.getenv("CONNECT_API_KEY")
  )
}

connect_session_token <- function(request) {

  token <- request[["HTTP_POSIT_CONNECT_USER_SESSION_TOKEN"]]

  if (is.null(token) || !nzchar(token)) {
    return(NULL)
  }

  token
}

connect_board <- function(token) {

  if (!requireNamespace("connectapi", quietly = TRUE)) {
    blockr_abort(
      "Package \"connectapi\" is required for user-scoped Connect pins.",
      class = "connectapi_not_installed"
    )
  }

  con <- connectapi::connect(token = token)

  pins::board_connect(
    auth = "manual",
    server = Sys.getenv("CONNECT_SERVER"),
    key = con$api_key
  )
}
