# Project management

Enhanced session management with navbar-compatible UI. Provides a
[`blockr.core::preserve_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/preserve_board.html)
plugin with full navbar layout including workflows, version history, and
editable title.

## Usage

``` r
manage_project(server = manage_project_server, ui = manage_project_ui)

manage_project_server(id, board, ...)

manage_project_ui(id, x)
```

## Arguments

- server, ui:

  Server/UI for the plugin module

- id:

  Namespace ID

- board:

  Reactive values object containing board state

- ...:

  Extra arguments (may include dock object)

- x:

  Board object

## Value

See
[`blockr.core::preserve_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/preserve_board.html).

## Examples

``` r
plg <- manage_project()
blockr.core::is_plugin(plg)
#> [1] TRUE
```
