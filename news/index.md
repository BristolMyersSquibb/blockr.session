# Changelog

## blockr.session 0.1.1

- Downloading a single workflow or a single version from a row of the
  workflow listing works again: both buttons drive a hidden download
  link that Shiny had left unregistered while hidden, so clicking did
  nothing at all.

- Setting the `session_autosave` blockr option to an interval in seconds
  turns on crash recovery: a board with unsaved changes is parked on the
  configured backend as a *draft*, and a later session offers to restore
  it. A draft is an ordinary record at a reserved id, so it never
  touches a workflow’s version history and stays out of the workflow
  listing. Retention is governed by `session_draft_ttl`.

- The
  [`rack_create()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_create.md)
  function gains a `draft` argument minting those reserved ids, and the
  new
  [`rack_records()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_create.md)
  lists records of one kind at a time. Draft writes are unversioned, via
  a new `versioned` argument on
  [`rack_upload()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md).

## blockr.session 0.1.0

CRAN release: 2026-08-04

- Initial CRAN release.

- [`manage_project()`](https://bristolmyerssquibb.github.io/blockr.session/reference/manage_project.md)
  provides a blockr.core `preserve_board` plugin that saves, restores
  and manages boards from within a running app, with a navbar dropdown
  exposing a workflow listing, version history and an editable board
  title.

- Board storage is backed by the pins package.
  [`user_pins_board()`](https://bristolmyerssquibb.github.io/blockr.session/reference/user_pins_board.md)
  is the default backend, resolved from the `session_mgmt_backend`
  blockr option: on Posit Connect with the Connect API Integration
  enabled, each visitor reads and writes pins under their own account,
  falling back to the application’s Connect credentials and then to a
  local board off Connect.

- On Posit Connect, boards can be shared with named users and given a
  visibility level from the dropdown’s Sharing tab.
