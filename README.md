
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blockr.session

<!-- badges: start -->

<!-- badges: end -->

The goal of blockr.session is to …

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
library(blockr.session)

serve(
  new_board(),
  plugins = custom_plugins(manage_project())
)
```

The default storage backend is \[pins::board_local()\] which can be
configured by setting the `session_mgmt_backend` \[blockr_option()\].
