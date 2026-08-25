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
`rack_records()` lists what a backend holds, of one kind at a time.

## Usage

``` r
rack_create(backend, data, id, name, draft = FALSE, ...)

rack_append(id, backend, data, ...)

rack_records(backend, draft = FALSE, ...)
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

- draft:

  Which records this concerns: `FALSE` for one the user saved, or a
  draft kind, `"session"` or `"record"`. A `rack_records()` call also
  accepts `TRUE`, matching a draft of either kind.

- ...:

  Additional arguments forwarded to
  [`rack_upload()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  or, for `rack_records()`, to
  [`rack_list()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md).

## Value

`rack_create()` and `rack_append()` return a `rack_id` object
identifying the newly created version, `rack_records()` a list of
`rack_record`s.

## Drafts

A *draft* is unsaved work parked for crash recovery. It is an ordinary
rack record in every respect a backend can see – same store, same tags,
read with
[`rack_load()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_load.md)
and dropped with
[`rack_purge()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
– and all that sets it apart is a reserved id, minted by
`rack_create(draft = )` and matched by `rack_records(draft = )`. Nothing
below this API knows drafts exist, so a backend needs no draft support
of its own.

Passing `draft` also relaxes what `rack_create()` does, because a draft
is a single slot rather than a history: the id-already-taken check is
skipped, so a repeated write overwrites, and the write is unversioned.
With `draft = FALSE` the reserved namespace is refused instead, so a
user cannot save a record into it.

A draft is of one *kind*. A `record` draft follows a saved workflow and
is keyed on it, so its id is the same in every session: there is one
slot per workflow by construction, it is overwritten as the work
changes, and the save it was shadowing drops it. Nothing about it
expires, since a workflow bounds it. A `session` draft belongs to a
board with no record yet, keyed on an opaque per-session id, and those
do accumulate – so one carries the time it was opened and is swept on a
retention read straight off the id. That needs neither the optional
`metadata` capability nor any field beyond those
[`new_rack_record()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
guarantees. The key is hashed into the id rather than embedded, since a
workflow id may hold characters a backend rejects in a record name.

The id a draft write returns carries no version: a draft keeps one, and
the next write to that slot deletes it.

## See also

[`rack_load()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_load.md)
for the complementary load function,
[`rack_rename()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
to change a record's name,
[`rack_upload()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
and
[`rack_list()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
for the underlying generics.
