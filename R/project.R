#' Project management
#'
#' Enhanced session management with navbar-compatible UI. Provides a
#' [blockr.core::preserve_board()] plugin with full navbar layout including
#' workflows, version history, and editable title.
#'
#' @section Crash recovery:
#' Set the `session_autosave` [blockr.core::blockr_option()] to an interval in
#' seconds to have unsaved work parked for recovery. On each tick the board is
#' serialized and, when it differs from what was last parked, written to the
#' configured backend as a *draft* (see [rack_create()]): an ordinary record at
#' a reserved id, which overwrites in place rather than keeping a history and
#' is held out of the workflow listing. An idle board costs one local
#' serialization per tick and no backend traffic at all.
#'
#' Recovery is what a draft is for, so nothing is published by it: a save
#' remains the only way work reaches a record other people can load. A session
#' holding a loaded record parks under that record, one slot reused; a board
#' with no record yet parks under an opaque per-session id. Saving clears the
#' latter. A later session lists what is left and offers each entry for restore
#' or discard, leaving anything untouched available next time; the offer is
#' reached from the navbar or by loading the app with a bare `recover` query
#' parameter. Drafts are swept once they pass the `session_snapshot_ttl`
#' option's retention, in days, seven by default.
#'
#' @inheritParams blockr.core::preserve_board
#'
#' @return See [blockr.core::preserve_board()].
#'
#' @examples
#' plg <- manage_project()
#' blockr.core::is_plugin(plg)
#'
#' @export
manage_project <- function(server = manage_project_server,
                           ui = manage_project_ui) {

  preserve_board(server, ui)
}
