# blockr.session 0.1.1

* Setting the `session_autosave` blockr option to an interval in seconds turns
  on crash recovery: a board with unsaved changes is parked as a private
  snapshot on the configured backend, and a later session offers to restore it.
  Snapshots never touch a workflow's version history and stay out of the
  workflow listing. Retention is governed by `session_snapshot_ttl`.

# blockr.session 0.1.0

* Initial CRAN release.

* `manage_project()` provides a blockr.core `preserve_board` plugin that
  saves, restores and manages boards from within a running app, with a
  navbar dropdown exposing a workflow listing, version history and an
  editable board title.

* Board storage is backed by the pins package. `user_pins_board()` is the
  default backend, resolved from the `session_mgmt_backend` blockr option:
  on Posit Connect with the Connect API Integration enabled, each visitor
  reads and writes pins under their own account, falling back to the
  application's Connect credentials and then to a local board off Connect.

* On Posit Connect, boards can be shared with named users and given a
  visibility level from the dropdown's Sharing tab.
