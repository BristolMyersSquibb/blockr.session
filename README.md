
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blockr.session

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![status](https://github.com/BristolMyersSquibb/blockr.session/actions/workflows/ci.yaml/badge.svg)](https://github.com/BristolMyersSquibb/blockr.session/actions/workflows/ci.yaml)
[![coverage](https://codecov.io/gh/BristolMyersSquibb/blockr.session/graph/badge.svg?token=SA9116N8LN)](https://app.codecov.io/gh/BristolMyersSquibb/blockr.session)
<!-- badges: end -->

Project (i.e. board) management provided by blockr.core via the
`preserve_board` plugin is a simple file upload/download-based
mechanism. More user-friendly alternatives using the pins package are
available as `manage_project` plugin.

## Installation

You can install the development version of blockr.session from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.session")
```

## Example

We can start up an app that uses the `manage_session()` plugin by
swapping out the default `preserve_board()` plugin.

``` r
library(blockr.core)
library(blockr.dock)
library(blockr.session)

serve(
  new_dock_board(),
  plugins = custom_plugins(manage_project())
)
```

The default storage backend is \[pins::board_local()\] which can be
configured by setting the `session_mgmt_backend` \[blockr_option()\].

## Deploying to Posit Connect

When deploying to Posit Connect, use `pins::board_connect` as the
backend so that a fresh board is created per user session (picking up
the correct credentials at runtime rather than at deploy time):

``` r
options(blockr.session_mgmt_backend = pins::board_connect)
```

Note the absence of `()`: the value is the **function itself**, not its
result. `get_session_backend()` calls it once per session, which is what
allows per-user pin storage to work correctly.

### Per-user pins with the Connect API Integration

By default, pins are stored under the deploying user’s namespace. To
have each viewer’s pins saved under their own account, enable the
**Connect API Integration** for the content:

1.  In Connect, open the content’s **Settings → Access** tab.
2.  Under *API Integrations*, enable the **Posit Connect** integration.
3.  Set the maximum role to **Viewer** (sufficient for reading and
    writing pins).

With the integration enabled, Connect injects an ephemeral
`CONNECT_API_KEY` into each user’s session. Because
`pins::board_connect` is called fresh per session, it automatically
picks up that key via its default `auth = "auto"`. No additional
packages or configuration are needed — in particular, the
[`connectcreds`](https://github.com/posit-dev/connectcreds) package is
**not** required here; that package is for exchanging session
credentials for third-party OAuth tokens (e.g. GitHub, Salesforce),
which is a separate use case.
