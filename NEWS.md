# blockr.session (development version)

* The navbar's left side is the workflow: its name is a menu that switches
  to another workflow, and "Save" beside it opens a save menu. Save (⌘S on a
  Mac, Ctrl+S elsewhere; "Save…" until the workflow has a name), then, once
  it is saved, "Save as new workflow…" (⌘⇧S / Ctrl+Shift+S) and Download,
  "Version history" with the recent versions in a menu beside it, and
  "Share…" at the foot. The hints come from `blockr.ui::shortcut()`, and the
  shortcuts work before the menu has opened. The menus use blockr.ui's menu
  look and chevrons; the views menu on the right comes from blockr.dock.

# blockr.session 0.1.1

* The workflow name in the navbar gives way on a narrow bar: it is cut short
  with an ellipsis, with the full name as its tooltip, while the controls
  beside it keep their width (#136).

* The avatar moves out of the `manage_project()` plugin's piece of the navbar
  into an item of its own, `avatar_navbar_item()`, which an app appends to the
  navbar of a blockr.dock board, after the dock's own controls, with
  `serve(board, navbar = blockr.dock::custom_navbar(avatar_navbar_item()))`.
  It shows only for a signed-in user, where the plugin fell back to the account
  the app runs under (#135).

* The workflow listing and the draft notice show their tooltips as blockr's
  light card rather than the browser's native box. The card comes from
  blockr.ui, which the package now imports. The listing's icon-only buttons
  and links gain an accessible name, and the links that open a workflow in a
  new tab gain a tooltip as well. The navbar's workflow name drops its
  "Workflow ID" tooltip.

* Downloading a single workflow or a single version from a row of the workflow
  listing works again: both buttons drive a hidden download link that Shiny had
  left unregistered while hidden, so clicking did nothing at all.

* Setting the `session_autosave` blockr option to an interval in seconds turns
  on crash recovery: a board with unsaved changes is parked on the configured
  backend as a *draft*, and a later session offers to restore it. A draft is an
  ordinary record at a reserved id, so it never touches a workflow's version
  history and stays out of the workflow listing. Retention is governed by
  `session_draft_ttl`.

* The `rack_create()` function gains a `draft` argument minting those reserved
  ids, and the new `rack_records()` lists records of one kind at a time. Draft
  writes are unversioned, via a new `versioned` argument on `rack_upload()`.

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
