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
#' configured backend as a *snapshot*: a private, unversioned record that
#' overwrites in place, stays out of the workflow listing and never adds to a
#' workflow's version history. An idle board costs one local serialization per
#' tick and no backend traffic at all.
#'
#' Recovery is what a snapshot is for, so nothing is published by it: a save
#' remains the only way work reaches a record other people can load. A session
#' holding a loaded record parks under that record, one slot reused; a board
#' with no record yet parks under an opaque per-session key. Saving clears the
#' latter. A later session lists what is left and offers each entry for restore
#' or discard, leaving anything untouched available next time. Snapshots are
#' swept once they pass the `session_snapshot_ttl` option's retention (in days,
#' seven by default), or shortly after the session that owned one ended
#' cleanly. Backends that cannot keep one visitor's snapshots apart from
#' another's report `snapshot = FALSE` from [rack_capabilities()] and are never
#' written to; on Posit Connect that means autosave is on only where the
#' visitor's own credentials are available.
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
