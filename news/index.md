# Changelog

## blockr.session 0.1.0

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
