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

  options("blockr.preload_board" = preload_board_from_query)

  preserve_board(server, ui)
}
