#' Session extension
#'
#' Adds workflow management UI to the blockr.dock navbar (save, load,
#' workflows list, history). Unlike dock extensions, this does not create
#' a dock panel - it only provides navbar content.
#'
#' @return A navbar provider object
#'
#' @examples
#' \dontrun{
#' library(blockr.dock)
#' library(blockr.session)
#'
#' serve(
#'   new_dock_board(
#'     extensions = list(
#'       blockr.dag::new_dag_extension(),
#'       new_session_extension()
#'     )
#'   )
#' )
#' }
#'
#' @export
new_session_extension <- function() {
  blockr.dock::new_navbar_provider(
    id = "session",
    navbar_left_ui = session_navbar_left_ui,
    navbar_left_server = session_navbar_left_server,
    navbar_right_ui = session_navbar_right_ui,
    navbar_right_server = session_navbar_right_server,
    class = "session_extension"
  )
}
