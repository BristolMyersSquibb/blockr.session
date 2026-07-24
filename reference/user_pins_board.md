# Posit Connect storage backend with graceful fallback

Resolves the storage backend for the `session_mgmt_backend`
[`blockr.core::blockr_option()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_option.html)
– of which it is the default – in three tiers, degrading instead of
erroring on a missing credential:

## Usage

``` r
user_pins_board(session = get_session())
```

## Arguments

- session:

  Shiny session whose request carries the Connect user session token;
  defaults to the current reactive domain.

## Value

A `pins_board`: a viewer- or application-scoped `board_connect`, or
`board_local` when no Connect credentials are available.

## Details

1.  When the visitor carries a Posit Connect user session token (the
    Connect API Integration is enabled), it is exchanged via
    [`connectapi::connect()`](https://posit-dev.github.io/connectapi/reference/connect.html)
    for a viewer-scoped API key, so each user reads and writes pins
    under their own namespace
    ([`pins::board_connect()`](https://pins.rstudio.com/reference/board_connect.html)).
    Requires the connectapi package.

2.  With no visitor token but application Connect credentials in the
    environment (`CONNECT_SERVER` and `CONNECT_API_KEY`), a board on the
    application's own account.

3.  Otherwise (e.g. local development, off Connect),
    [`pins::board_local()`](https://pins.rstudio.com/reference/board_folder.html).

Set the `session_connect_tag`
[`blockr.core::blockr_option()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_option.html)
to an existing Connect tag's name (or a `"Category/Name"` path) to have
the workflow listing filter server-side by that native tag; saves then
apply it. Unset, the listing returns every pin and checks blockr
membership only on load.
