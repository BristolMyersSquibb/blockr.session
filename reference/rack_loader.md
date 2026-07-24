# Rack-backed board loader

A
[`blockr.core::board_loader()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_loader.html)
for
[`blockr.core::serve()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.html)
that picks the board to build from the request URL. The board named by
the URL handle (`board_name` / `user` / `version`) loads from the
`session_mgmt_backend` backend; a `new` handle (minted when the user
hits New) yields an empty board under that fresh id, so saving it forks
a distinct record rather than overwriting the served board; a request
without any handle (a cold load) gets a cleared copy of the served
board. The backend is resolved with the loader's own request (at the
GET, before any session) or session (at the WS connect), so a
user-scoped backend such as
[`user_pins_board()`](https://bristolmyerssquibb.github.io/blockr.session/reference/user_pins_board.md)
resolves under the *visitor's* own Posit Connect credentials at both
phases; a board the visitor may not read does not resolve, whatever the
`user` / `board_name` in the URL. Pair it with
[`manage_project()`](https://bristolmyerssquibb.github.io/blockr.session/reference/manage_project.md)
when calling
[`blockr.core::serve()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.html):
`serve(board, plugins = c(.., manage_project()), loader = rack_loader())`.

## Usage

``` r
rack_loader()
```

## Value

A
[`blockr.core::board_loader()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_loader.html)
object.
