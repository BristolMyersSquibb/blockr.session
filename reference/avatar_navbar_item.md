# Navbar avatar

An item for the navbar of a blockr.dock board that shows the initials of
the user signed in to the app, from `session$user`. A server that signs
its visitors in, such as Posit Connect, sets it, and for a visitor who
is not signed in the item draws nothing. Appended with
[`blockr.dock::custom_navbar()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/navbar.html),
the avatar sits last on the bar.

## Usage

``` r
avatar_navbar_item()
```

## Value

A
[`blockr.dock::navbar_item()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/navbar.html)
with id `"avatar"`.

## Examples

``` r
blockr.dock::is_navbar_item(avatar_navbar_item())
#> [1] TRUE

if (interactive()) {
  blockr.core::serve(
    blockr.dock::new_dock_board(),
    plugins = blockr.core::custom_plugins(manage_project()),
    navbar = blockr.dock::custom_navbar(avatar_navbar_item())
  )
}
```
