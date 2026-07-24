# Implementing a custom storage backend

``` r

library(blockr.session)
```

The `blockr.session` package persists a board through a *rack*: a
storage abstraction that turns a save into a stored, versioned record
and a load back into board state. The shipped backend keeps records as
[pins](https://pins.rstudio.com), so on Posit Connect each user reads
and writes under their own account and off Connect everything lands in a
local pin folder. That default already covers plain on-disk storage:
point the `session_mgmt_backend` option at a
[`pins::board_folder()`](https://pins.rstudio.com/reference/board_folder.html)
or
[`pins::board_local()`](https://pins.rstudio.com/reference/board_folder.html)
and workflows are saved as files, no extra code.

This vignette is about the other case – storing session state somewhere
pins does not reach: a relational database, an object store, a REST
service. The rack contract is storage-agnostic, so any of these is a
matter of implementing a handful of S3 generics. We build a small
file-based backend as a runnable, dependency-free stand-in; the same
shape is what a Postgres or PocketBase backend fills in.

## The contract

Two kinds of object flow through the rack layer.

- A **backend** is the store itself – the board, database handle or
  client object. It is the dispatch target for the generics that operate
  on the store as a whole:
  [`as_rack_id()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md),
  [`rack_capabilities()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md),
  [`rack_list()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md),
  [`rack_upload()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  and
  [`rack_find_users()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md).
- A **`rack_id`** is a handle to one stored record. It is the dispatch
  target for the generics that read or mutate a single record:
  [`rack_download()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md),
  [`rack_exists()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md),
  [`rack_info()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md),
  [`rack_name()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md),
  [`rack_delete()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  and the rest.

Application code never calls the generics directly. It uses the
high-level entry points –
[`rack_create()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_create.md)
to insert a new record,
[`rack_append()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_create.md)
to add a version to an existing one,
[`rack_load()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_load.md)
to read one back – and those are built on the generics. For instance,
[`rack_create()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_create.md)
coerces the board id to a `rack_id` with
[`as_rack_id()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md),
checks
[`rack_exists()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md),
and stores the payload with
[`rack_upload()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md).
Implement the generics and the whole high-level API comes along for
free.

The two objects are minted with exported constructors,
[`new_rack_id()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
and
[`new_rack_record()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md),
each taking a `class` argument so your backend carries its own subclass.
That subclass is what the per-record generics dispatch on. See
[`?"rack-backend"`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
for the full generic-by-generic reference; the sections below are a
worked path through it.

## Capabilities

Not every store supports every feature. The
[`rack_capabilities()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
generic is the switch: it returns a named list of logical flags, and the
management UI reads them to decide which optional features to offer.

| Flag | Turns on |
|----|----|
| `versioning` | a per-record version history ([`rack_info()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md), versioned [`rack_download()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md) / [`rack_delete()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)) |
| `metadata` | a display name ([`rack_name()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md), [`rack_rename()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)) and a content hash ([`rack_content_hash()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)) |
| `tags` | tag read/write ([`rack_tags()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md), [`rack_set_tags()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)) |
| `sharing` | sharing a record with other users ([`rack_share()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md), [`rack_unshare()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md), [`rack_shares()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)) |
| `visibility` | access-control levels ([`rack_acl()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md), [`rack_set_acl()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)) |
| `user_discovery` | looking up users to share with ([`rack_find_users()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)) |

Return `FALSE` for a flag and you can leave the generics behind it
unimplemented – the UI does not surface that feature. A backend that
returns `FALSE` for `sharing`, `visibility` and `user_discovery` – as
ours will – needs none of the sharing, ACL or user-lookup methods, and
the navbar simply omits the Sharing tab.

## A file-based backend

The store keeps one directory per record under a root. Each record
directory holds the version payloads (`1.json`, `2.json`, …) plus a
`meta.json` sidecar carrying the display name and, per version, a
timestamp and content hash.

``` r

demo_store <- function(dir = tempfile("demo_store")) {

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  structure(list(dir = dir), class = c("demo_store", "rack_backend"))
}
```

The `rack_backend` class in second position is what matters: it is the
ticket past the `session_mgmt_backend` option’s validation, which
accepts a pins board or anything inheriting `rack_backend`. The first
class, `demo_store`, is what the backend generics dispatch on.

A few storage helpers keep the methods short. They are plain file I/O –
the part a real backend swaps for SQL or HTTP.

``` r

record_dir <- function(backend, id) {
  file.path(backend$dir, id)
}

read_meta <- function(backend, id) {

  path <- file.path(record_dir(backend, id), "meta.json")

  if (!file.exists(path)) {
    return(list(name = id, versions = list()))
  }

  jsonlite::read_json(path)
}

write_meta <- function(backend, id, meta) {
  jsonlite::write_json(
    meta,
    file.path(record_dir(backend, id), "meta.json"),
    auto_unbox = TRUE,
    null = "null"
  )
}
```

### Store-level generics

The
[`as_rack_id()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
generic coerces a raw identifier into the backend’s own `rack_id`
subclass. The raw identifier is either a component list (`id`, and
optionally `version` and `user`, as carried in a session URL) or a
`rack_record` listing row; both expose the fields through `$`, so one
line covers them. It dispatches on the *backend*, not on `x`.

``` r

as_rack_id.demo_store <- function(x, backend, ...) {
  new_rack_id(x$id, version = x$version, class = "rack_id_demo")
}
```

The
[`rack_upload()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
generic writes a local file as a new version. Both
[`rack_create()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_create.md)
and
[`rack_append()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_create.md)
funnel through it, handing over the serialized payload as a temp file
plus the record’s `id`, its `content_hash`, and – on a create – a
display `name`. We assign the next integer version, copy the payload in,
and record the version in the sidecar. The return value is a `rack_id`
pinned to the version just written.

``` r

rack_upload.demo_store <- function(backend, path, id, name = NULL,
                                   content_hash = NULL, ...) {

  dir <- record_dir(backend, id$id)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  meta <- read_meta(backend, id$id)
  version <- as.character(length(meta$versions) + 1L)

  file.copy(path, file.path(dir, paste0(version, ".json")), overwrite = TRUE)

  if (!is.null(name)) {
    meta$name <- name
  }

  meta$versions <- c(
    meta$versions,
    list(
      list(version = version, created = format(Sys.time()), hash = content_hash)
    )
  )

  write_meta(backend, id$id, meta)

  new_rack_id(id$id, version = version, class = "rack_id_demo")
}
```

The
[`rack_list()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
generic returns the listing rows the workflow menu renders, one
`rack_record()` per record. The `saved` field feeds the “saved N minutes
ago” label; a `tags` argument (unused here) would filter the listing.

``` r

rack_list.demo_store <- function(backend, tags = NULL, ...) {

  ids <- list.dirs(backend$dir, full.names = FALSE, recursive = FALSE)

  lapply(
    ids,
    function(rid) {
      meta <- read_meta(backend, rid)
      latest <- meta$versions[[length(meta$versions)]]
      new_rack_record(id = rid, name = meta$name, saved = latest$created)
    }
  )
}
```

### Record-level generics

These dispatch on the `rack_id_demo` subclass:
[`rack_exists()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
decides insert versus append;
[`rack_download()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
returns a local path to the payload, resolving the latest version when
the `rack_id` pins none;
[`rack_info()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
lists the versions newest-first as a data frame with `version`,
`created` and `ref` columns.

``` r

rack_exists.rack_id_demo <- function(id, backend, ...) {
  file.exists(file.path(record_dir(backend, id$id), "meta.json"))
}

rack_download.rack_id_demo <- function(id, backend, ...) {

  info <- rack_info(id, backend)

  if (nrow(info) == 0L) {
    stop("No versions stored for record ", id$id)
  }

  version <- if (is.null(id$version)) info$version[1L] else id$version

  file.path(record_dir(backend, id$id), paste0(version, ".json"))
}

rack_info.rack_id_demo <- function(id, backend, ...) {

  versions <- read_meta(backend, id$id)$versions

  if (length(versions) == 0L) {
    return(
      data.frame(
        version = character(),
        created = as.POSIXct(character()),
        ref = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  version <- vapply(versions, `[[`, character(1L), "version")
  created <- vapply(versions, `[[`, character(1L), "created")
  newest_first <- rev(seq_along(version))

  data.frame(
    version = version[newest_first],
    created = as.POSIXct(created[newest_first]),
    ref = version[newest_first],
    stringsAsFactors = FALSE
  )
}
```

The display name lives in the sidecar, so
[`rack_name()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
reads it and
[`rack_rename()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
rewrites it. The
[`rack_content_hash()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
generic returns the stored hash of the latest version; the app compares
it against the current board to skip a save that would change nothing.

``` r

rack_name.rack_id_demo <- function(id, backend, ...) {
  read_meta(backend, id$id)$name
}

rack_rename.rack_id_demo <- function(id, backend, name, ...) {

  meta <- read_meta(backend, id$id)
  meta$name <- name
  write_meta(backend, id$id, meta)

  new_rack_id(id$id, version = id$version, class = "rack_id_demo")
}

rack_content_hash.rack_id_demo <- function(id, backend, ...) {

  versions <- read_meta(backend, id$id)$versions

  if (length(versions) == 0L) {
    return(NULL)
  }

  versions[[length(versions)]]$hash
}
```

Deletion comes in two grains:
[`rack_delete()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
drops a single version (and the whole record once its last version
goes), while
[`rack_purge()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
removes the record outright – the grain the workflow menu’s delete uses.

``` r

rack_delete.rack_id_demo <- function(id, backend, ...) {

  meta <- read_meta(backend, id$id)

  version <- if (is.null(id$version)) {
    meta$versions[[length(meta$versions)]]$version
  } else {
    id$version
  }

  unlink(file.path(record_dir(backend, id$id), paste0(version, ".json")))

  meta$versions <- Filter(
    function(v) !identical(v$version, version),
    meta$versions
  )

  if (length(meta$versions) == 0L) {
    unlink(record_dir(backend, id$id), recursive = TRUE)
  } else {
    write_meta(backend, id$id, meta)
  }

  invisible(TRUE)
}

rack_purge.rack_id_demo <- function(id, backend, ...) {
  unlink(record_dir(backend, id$id), recursive = TRUE)
  invisible(TRUE)
}
```

### Declaring capabilities

This store keeps versions and a name-and-hash sidecar, so it advertises
`versioning` and `metadata`. It has no notion of users, sharing or
access control, so those flags are `FALSE` – which is why none of
[`rack_share()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md),
[`rack_acl()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
or
[`rack_find_users()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
appears above.

``` r

rack_capabilities.demo_store <- function(backend, ...) {
  list(
    versioning = TRUE,
    metadata = TRUE,
    tags = FALSE,
    sharing = FALSE,
    visibility = FALSE,
    user_discovery = FALSE
  )
}
```

## The round-trip

That is the whole backend. Because the storage layer needs no Shiny, we
can exercise it directly – create, list, load, append, inspect, rename,
delete.

``` r

store <- demo_store()

board <- list(
  blocks = list(list(id = "a", type = "dataset")),
  links = list()
)

id <- rack_create(store, board, id = "sales-report", name = "Sales report")
id
#> <rack_id: sales-report>
```

With the record saved,
[`rack_list()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
reports one entry and
[`rack_load()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_load.md)
reads back exactly what we stored.

``` r

rack_list(store)
#> [[1]]
#> <rack_record: sales-report (Sales report)>

identical(rack_load(id, store), board)
#> [1] TRUE
```

Saving again appends a version rather than replacing the record, and
[`rack_info()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
shows both, newest first.

``` r

board$blocks <- c(board$blocks, list(list(id = "b", type = "filter")))

rack_append(id, store, board)
#> <rack_id: sales-report>

rack_info(id, store)
#>   version             created ref
#> 1       2 2026-07-24 19:55:46   2
#> 2       1 2026-07-24 19:55:46   1
```

The name is stored independently of the payload, so a rename sticks
across a reload, and the content hash is what lets the app tell an
unchanged board from a modified one.

``` r

rack_rename(id, store, "Quarterly sales")
#> <rack_id: sales-report>
rack_name(as_rack_id(list(id = "sales-report"), store), store)
#> [1] "Quarterly sales"

rack_content_hash(id, store)
#> [1] "53abb582de74d031ce033ff3ee295976"
```

Deleting the record leaves the store empty.

``` r

rack_purge(id, store)
rack_list(store)
#> list()
```

## Trimming to the minimum

The capability switch decides how much you must implement. Our store
turns on `versioning` and `metadata`; a backend that has neither – say a
key-value store that keeps only the latest state – returns `FALSE` for
every optional flag and drops the matching methods, leaving just the
create / load / list core:
[`as_rack_id()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md),
[`rack_upload()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md),
[`rack_download()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md),
[`rack_exists()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md),
[`rack_list()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
and
[`rack_name()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md).
Even then
[`rack_content_hash()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
is worth keeping, since the app calls it to skip no-op saves.

Add capabilities back one at a time as the store grows: flip
`versioning` on and implement
[`rack_info()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md);
flip `sharing` and `visibility` on and implement the
[`rack_share()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
/
[`rack_acl()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
family. Each flag is independent, so a backend can support exactly the
features its store can back.

## Wiring it into an app

The `session_mgmt_backend` option selects the backend. It accepts a
backend object or a zero-argument function returning one; the function
form is resolved once per session, so credentials and paths are picked
up at runtime rather than when the app is defined.

``` r

options(
  blockr.session_mgmt_backend = function() demo_store("~/blockr-workflows")
)

library(blockr.core)
library(blockr.dock)

serve(
  new_dock_board(),
  plugins = custom_plugins(manage_project()),
  loader = rack_loader()
)
```

The
[`manage_project()`](https://bristolmyerssquibb.github.io/blockr.session/reference/manage_project.md)
plugin renders the navbar’s save controls and workflow menu;
[`rack_loader()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_loader.md)
is the board loader that restores the record named in the request URL
from the same backend. With the option set to our `demo_store`, saves
and loads now flow through the file store instead of pins. \`\`\`
