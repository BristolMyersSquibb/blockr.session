
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
