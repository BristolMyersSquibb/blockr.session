#' Project management
#'
#' Enhanced session management with navbar-compatible UI. Provides a
#' [blockr.core::preserve_board()] plugin with full navbar layout including
#' workflows, version history, and editable title.
#'
#' @inheritParams blockr.core::preserve_board
#'
#' @return See [blockr.core::preserve_board()].
#'
#' @export
manage_project <- function(server = manage_project_server,
                           ui = manage_project_ui) {

  register_board_preload(session_preload)

  preserve_board(server, ui)
}

session_preload <- function(query, req) {

  if (is.null(query$board_name)) {
    return(NULL)
  }

  backend <- tryCatch(get_session_backend(), error = function(e) NULL)

  if (is.null(backend)) {
    return(NULL)
  }

  id <- rack_id_from_input(
    list(
      name = query$board_name,
      user = query$user,
      version = query$version
    ),
    backend
  )

  board_ser <- tryCatch(rack_load(id, backend), error = function(e) NULL)

  if (is.null(board_ser)) {
    return(NULL)
  }

  list(
    board = blockr_deser(board_ser),
    meta = list(url = board_query_string(id, backend))
  )
}
