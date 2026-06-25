
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

We can start up an app that uses the `manage_project()` plugin by
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

The default storage backend is \[user_pins_board()\]: on Posit Connect
it resolves per-visitor pin storage from the visitor’s own session token
(falling back to the application’s Connect credentials), and off Connect
it falls back to \[pins::board_local()\]. Override it by setting the
`session_mgmt_backend` \[blockr_option()\].

## Connect storage

On Posit Connect with the Connect API Integration enabled, each user
reads and writes pins under their own account out of the box, with no
backend option to set. The navbar dropdown gains a **Sharing** tab (next
to Workflows and History) that lets you set visibility and share with
other Connect users.

To override the default, set the option to a `pins` board or to a
function returning one, which `get_session_backend()` calls once per
session so credentials are picked up at runtime. For example, to use a
single shared namespace on the publisher’s account instead of
per-visitor storage:

``` r
options(blockr.session_mgmt_backend = pins::board_connect)
```

### Deploying to Posit Connect

When deploying to Connect no backend option is needed: the default
\[user_pins_board()\] picks up Connect credentials from the environment
automatically.

### Per-user pins with the Connect API Integration

Without the integration, every viewer’s pins are stored under the
application’s own Connect account, a single shared namespace. To have
each viewer’s pins saved under their *own* account instead, enable the
**Connect API Integration** for the content:

1.  In Connect, open the content’s **Settings → Access** tab.
2.  Under *API Integrations*, enable the **Posit Connect** integration.
3.  Set the maximum role to **Viewer** (sufficient for reading and
    writing pins).

With the integration enabled, Connect attaches a per-visitor session
token to each request, which \[user_pins_board()\] exchanges (via
`connectapi::connect()`) for a viewer-scoped API key, so the
[`connectapi`](https://github.com/posit-dev/connectapi) package is
required for this path.
