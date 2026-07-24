# Create or append to a session record on a rack backend

`rack_create()` serialises `data` to JSON and stores it as a **new**
record keyed on `id` – the board's own stable id, so the record id and
the board id match. It is a strict insert: it errors (class
`rack_create_exists`) if `id` already names a record rather than
appending a version. `name` is written to the backend's native display
field. `rack_append()` adds a **new version** to the existing record
identified by `id`, erroring (class `rack_append_missing`) if there is
none, and never touches the name. Together they replace the former
`rack_save()`, separating insert from append. To change a record's name,
use
[`rack_rename()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md).

## Usage

``` r
rack_create(backend, data, id, name, ...)

rack_append(id, backend, data, ...)
```

## Arguments

- backend:

  A rack backend object (e.g. a `pins_board`).

- data:

  An R object to serialise and store (typically the session list
  returned by the blockr session machinery).

- id:

  For `rack_create()`, the storage id to key the new record on
  (typically the board id); errors if it already names a record. For
  `rack_append()`, the `rack_id` of the record to add a version to.

- name:

  Character scalar. The display name for the new record.

- ...:

  Additional arguments forwarded to
  [`rack_upload()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md).

## Value

A `rack_id` object identifying the newly created version.

## See also

[`rack_load()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_load.md)
for the complementary load function,
[`rack_rename()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
to change a record's name,
[`rack_upload()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
for the underlying generic.
