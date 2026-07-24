# Rack storage backend contract

The rack layer is the storage abstraction behind blockr session
persistence: it turns a save into a stored, versioned record and a load
back into board state. The shipped backend dispatches on pins boards
(see
[`user_pins_board()`](https://bristolmyerssquibb.github.io/blockr.session/reference/user_pins_board.md)),
but the contract is storage-agnostic – a database or document store is
supported by implementing the generics documented here and carrying the
`rack_backend` class, which is what wiring a non-pins backend into the
`session_mgmt_backend`
[`blockr.core::blockr_option()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_option.html)
requires. The high-level
[`rack_create()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_create.md),
[`rack_append()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_create.md)
and
[`rack_load()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_load.md)
entry points that application code calls are built on these generics.

## Usage

``` r
new_rack_id(id, version = NULL, user = NULL, ..., class = character())

new_rack_record(id, name, ..., class = character())

rack_content_hash(id, backend, ...)

rack_name(id, backend, ...)

rack_rename(id, backend, name, ...)

rack_exists(id, backend, ...)

rack_list(backend, tags = NULL, ...)

as_rack_id(x, backend, ...)

rack_info(id, backend, ...)

rack_download(id, backend, ...)

rack_upload(backend, path, ...)

rack_delete(id, backend, ...)

rack_purge(id, backend, ...)

rack_capabilities(backend, ...)

rack_tags(id, backend, ...)

rack_set_tags(id, backend, tags, ...)

rack_acl(id, backend, ...)

rack_set_acl(id, backend, acl_type, ...)

rack_share(id, backend, with_sub, ...)

rack_unshare(id, backend, with_sub, ...)

rack_shares(id, backend, ...)

rack_find_users(backend, query, ...)
```

## Arguments

- id:

  A `rack_id` identifying a stored record, for the accessor and mutator
  generics. For `new_rack_id()` and `new_rack_record()`, the record's
  storage id: a non-empty string, stable and independent of the display
  name.

- version:

  For `new_rack_id()`, an optional version string pinning the `rack_id`
  to one stored version; `NULL` tracks the latest.

- user:

  For `new_rack_id()`, the owning account, used to reach records stored
  under another user's namespace.

- ...:

  Passed on to methods; for the constructors, extra named fields stored
  on the object.

- class:

  Character vector prepended to the object's class, so a backend carries
  its own `rack_id` / `rack_record` subclass.

- name:

  Display name for the record, written to the backend's native name
  field and never used as the storage key; `NULL` keeps the current
  name.

- backend:

  A rack backend: the storage object dispatched on. A pins board is
  recognized directly; any other backend carries the `rack_backend`
  class so the `session_mgmt_backend` option accepts it.

- tags:

  Character vector of tags: the filter applied by `rack_list()` (`NULL`
  for none), or the tag set written by `rack_set_tags()`.

- x:

  The object to coerce with `as_rack_id()`: an existing `rack_id`
  (returned unchanged), or a component bag – a list of identifier fields
  (`id`, and optionally `version` and `user`) or a `rack_record` –
  turned into the backend's own `rack_id` subclass.

- path:

  Path to the local file to upload as a new version.

- acl_type:

  Access-control level to set, backend-defined (e.g. `"private"`,
  `"acl"`, `"all"`).

- with_sub:

  Backend identifier of the principal to share a record with or revoke.

- query:

  Search prefix for `rack_find_users()`.

## Value

`new_rack_id()` and `new_rack_record()` return a `rack_id` and
`rack_record` respectively. `rack_capabilities()` returns the named list
of flags above. `rack_list()` returns a list of `rack_record`s and
`rack_info()` a data frame of versions (columns `version`, `created`,
`ref`, newest first). `as_rack_id()`, `rack_upload()` and
`rack_rename()` return a `rack_id`; `rack_download()` a local file path;
`rack_name()` a string; `rack_exists()` a scalar logical;
`rack_content_hash()` the stored payload hash; `rack_tags()`,
`rack_acl()` and `rack_shares()` the stored tags, access level and
shares. The mutators (`rack_set_tags()`, `rack_set_acl()`,
`rack_share()`, `rack_unshare()`, `rack_delete()`, `rack_purge()`)
return invisibly.

## Details

Two kinds of object flow through the contract. A *backend* is the board
or store itself, the dispatch target for `as_rack_id()`,
`rack_capabilities()`, `rack_list()`, `rack_upload()` and
`rack_find_users()`. A `rack_id` is a handle to one stored record, the
dispatch target for the remaining generics. `as_rack_id()` coerces a raw
identifier – a component list or a `rack_record` listing row – into the
backend's own `rack_id` subclass (via `new_rack_id(..., class = )`) from
the components (`id`, `version`, `user`) carried in a session URL, and
returns an existing `rack_id` unchanged. `rack_list()` returns
`new_rack_record()` summaries, the listing rows the workflow menu
renders.

## Capabilities

`rack_capabilities()` is the feature switch: it returns a named list of
logical flags declaring which optional features the backend supports,
and the management UI gates the matching generics on it.

- `versioning`: a version history is kept per record: `rack_info()`
  lists it and `rack_download()` / `rack_delete()` accept a version.

- `tags`: `rack_tags()` and `rack_set_tags()` are implemented.

- `metadata`: a display name (`rack_name()`, `rack_rename()`) and a
  content hash (`rack_content_hash()`) are stored alongside the payload.

- `sharing`: `rack_share()`, `rack_unshare()` and `rack_shares()` are
  implemented.

- `visibility`: `rack_acl()` and `rack_set_acl()` are implemented.

- `user_discovery`: `rack_find_users()` is implemented.

A minimal backend implements only the core create / load / list set –
`as_rack_id()`, `rack_upload()`, `rack_download()`, `rack_exists()`,
`rack_list()`, `rack_info()` and `rack_name()` – and returns `FALSE` for
every optional capability; the generics behind a `FALSE` flag are then
never reached and need no method.

## See also

[`rack_create()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_create.md),
[`rack_append()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_create.md)
and
[`rack_load()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_load.md)
for the high-level save / load API built on the contract, and
[`user_pins_board()`](https://bristolmyerssquibb.github.io/blockr.session/reference/user_pins_board.md)
for the default pins backend.
