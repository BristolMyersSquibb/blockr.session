# Load a session from a rack backend

Downloads and parses a stored session identified by `id` from the given
`backend`. This is a convenience wrapper around
[`rack_download()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
that additionally deserialises the JSON payload into an R object.

## Usage

``` r
rack_load(id, backend, ...)
```

## Arguments

- id:

  A `rack_id` object identifying the session to load.

- backend:

  A rack backend object (e.g. a `pins_board`).

- ...:

  Additional arguments forwarded to
  [`rack_download()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md).

## Value

The deserialised session data as an R object (typically a named list).

## See also

[`rack_create()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_create.md)
and
[`rack_append()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_create.md)
for the complementary save functions,
[`rack_download()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
for the underlying download generic.
