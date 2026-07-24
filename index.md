# blockr.session

blockr.session adds project (i.e. board) persistence to
[blockr](https://bristolmyerssquibb.github.io/blockr.core/) apps: save,
restore and manage boards from within a running app. It provides a
[`manage_project()`](https://bristolmyerssquibb.github.io/blockr.session/reference/manage_project.md)
plugin backed by the [pins](https://pins.rstudio.com/) package – a more
capable alternative to blockr.core’s built-in
[`preserve_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/preserve_board.html),
which offers only simple file upload and download.

## Installation

You can install the development version of blockr.session from
[GitHub](https://github.com/) with:

``` r

# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.session")
```

## Example

We can start up an app that uses the
[`manage_project()`](https://bristolmyerssquibb.github.io/blockr.session/reference/manage_project.md)
plugin by swapping out the default
[`preserve_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/preserve_board.html)
plugin.

``` r

library(blockr.core)
library(blockr.dock)
library(blockr.session)

serve(
  new_dock_board(),
  plugins = custom_plugins(manage_project())
)
```

The default storage backend is
[`user_pins_board()`](https://bristolmyerssquibb.github.io/blockr.session/reference/user_pins_board.md):
on Posit Connect it resolves per-visitor pin storage from the visitor’s
own session token (falling back to the application’s Connect
credentials), and off Connect it falls back to
[`pins::board_local()`](https://pins.rstudio.com/reference/board_folder.html).
Override it by setting the `session_mgmt_backend`
[`blockr.core::blockr_option()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_option.html).

## Custom storage backends

Storage is a pluggable S3 contract, not tied to pins.
[`user_pins_board()`](https://bristolmyerssquibb.github.io/blockr.session/reference/user_pins_board.md)
is the default backend, but you can implement your own (a database, an
object store, a REST service) by defining methods for the exported rack
generics. See
[`vignette("custom-storage-backend")`](https://bristolmyerssquibb.github.io/blockr.session/articles/custom-storage-backend.md)
for a worked, file-based example.

## Connect storage

On Posit Connect with the Connect API Integration enabled, each user
reads and writes pins under their own account out of the box, with no
backend option to set. The navbar dropdown gains a **Sharing** tab (next
to Workflows and History) that lets you set visibility and share with
other Connect users.

To override the default, set the option to a `pins` board or to a
function returning one, which blockr.session resolves once per session
so credentials are picked up at runtime. For example, to use a single
shared namespace on the publisher’s account instead of per-visitor
storage:

``` r

options(blockr.session_mgmt_backend = pins::board_connect)
```

### Deploying to Posit Connect

When deploying to Connect no backend option is needed: the default
[`user_pins_board()`](https://bristolmyerssquibb.github.io/blockr.session/reference/user_pins_board.md)
picks up Connect credentials from the environment automatically.

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
token to each request, which
[`user_pins_board()`](https://bristolmyerssquibb.github.io/blockr.session/reference/user_pins_board.md)
exchanges (via
[`connectapi::connect()`](https://posit-dev.github.io/connectapi/reference/connect.html))
for a viewer-scoped API key, so the
[`connectapi`](https://github.com/posit-dev/connectapi) package is
required for this path.
