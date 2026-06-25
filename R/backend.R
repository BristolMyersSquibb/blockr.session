#' User-scoped Posit Connect storage backend
#'
#' Constructs a [pins::board_connect()] scoped to the *viewer's* own Posit
#' Connect account, for use as the `session_mgmt_backend`
#' [blockr.core::blockr_option()]. When the content has the Connect API
#' Integration enabled, Connect injects a per-viewer session token into the
#' request, which this exchanges for a viewer-scoped API key via
#' `connectapi::connect()` so each user reads and writes pins under their own
#' namespace. With no token present (e.g. local development), it falls back to
#' [pins::board_local()].
#'
#' Requires the \pkg{connectapi} package and the `CONNECT_SERVER` environment
#' variable.
#'
#' @param session Shiny session whose request carries the Connect user session
#'   token; defaults to the current reactive domain.
#'
#' @return A `pins_board`: a viewer-scoped `board_connect`, or `board_local`
#'   when no Connect session token is available.
#'
#' @export
user_pins_board <- function(session = shiny::getDefaultReactiveDomain()) {

  token <- connect_session_token(session$request)

  if (is.null(token)) {
    log_info("No Connect user session token found; using a local pins board.")
    return(pins::board_local())
  }

  board <- connect_board(token)

  log_info("Resolved a Connect pin board for account {board$account}.")

  board
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
