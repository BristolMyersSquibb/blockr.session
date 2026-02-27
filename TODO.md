# URL-based board loading — plan

## Phase 1: Board rack abstraction layer

Both `manage_session` and `manage_project` call pins directly for
save/load/list/delete/versions, each with its own error handling,
tag validation (or lack thereof), and metadata assumptions. Before
adding URL loading, introduce a rack abstraction that:

- Gives both plugins (and the URL feature) a single set of operations.
- Encapsulates pins-specific details (tag format, metadata shape,
  `pin_search` vs `pin_list`, namespaced names).
- Provides a seam for future backends (DB, git, caching).

Where the two plugins disagree, follow `manage_session`'s patterns
(consistent `cnd_to_notif()`, tag validation, `...` forwarding).

### 1.1 — Rack API

A new file `R/rack.R` with the following functions. All operate on a
`backend` object (currently always a `pins_board`) and handle their own
errors via `cnd_to_notif()`. Callers never call `pins::` directly.

#### Identifiers

```r
rack_id(...)
```
S3 class representing a reference to a specific board (and optionally a
version) in a backend. Inheritance hierarchy:

```
rack_id
├── rack_id_pins
│   ├── rack_id_pins_local    (fields: name, version)
│   └── rack_id_pins_connect  (fields: user, name, version)
├── rack_id_db                (fields: row_id)         [future]
└── rack_id_git               (fields: repo, path, commit)  [future]
```

Shared pins logic (JSON format handling, tag checking) lives on
`rack_id_pins` methods. Connect-specific behaviour (user namespace)
on `rack_id_pins_connect`.

Constructed by `rack_save()` (returned), `rack_list()` / `rack_info()`
(included in results), and from URL params (phase 3). Accepted by
`rack_load()`, `rack_delete()`.

Callers treat IDs as opaque.

S3 dispatch:
- **On `backend`:** `rack_list()`, `rack_save()` — these produce IDs.
- **On `id`:** `rack_info()`, `rack_load()`, `rack_delete()` — these
  consume IDs. `id` is the first argument for dispatch; `backend` is
  passed for connection/credentials.

Generic accessor methods on `rack_id`:
- `display_name(id)` — human-readable label for the board.
- `last_saved(id)` — last-saved timestamp. May extract from cached
  metadata, query the filesystem, etc. depending on backend.
- `format()` / `print()` — for display in UI and debugging.

#### Discovery

```r
rack_list(backend)
```
Lightweight discovery of available boards. Dispatches on `backend`.
Returns a list of `rack_id` objects (without version). Only returns
boards tagged with `blockr_session_tags()`. Callers pass these IDs
directly to `rack_info()` / `rack_load()`. Investigate whether
`pin_search()` returns a `tags` column consistently across backends
(local vs Connect) and handle differences.

```r
rack_info(id, backend)
```
Detailed info on a specific board. Dispatches on `id`. Returns version
history (dataframe with `version`, `created`, and any backend-specific
columns like `hash`) plus metadata (tags, format, etc.). This is what
the version history UI and "view all versions" modal consume.
Investigate which metadata fields differ across backends and normalize.

#### Read / write

```r
rack_load(id, backend)
```
Fetch a board from storage. Dispatches on `id`. Takes a `rack_id`
(with or without version — if version is NULL, resolves latest via
`rack_info()`). Fetches metadata, validates `has_tags()`, downloads
raw data, parses based on format metadata (currently JSON v1, future:
binary formats). The format dispatch is internal — callers always get
the same thing back. Returns an R list (board data in blockr's
serialization format) on success, NULL on failure. Does **not** call
`restore_board()` — that's the caller's job. No Shiny dependency.

```r
rack_save(backend, data, ...)
```
Store a board. Dispatches on `backend`. Takes an R list (board data in
blockr's serialization format, produced by `serialize_board()`).
Handles format encoding internally (currently: JSON → `pin_upload()`
with `versioned = TRUE`, format metadata, and blockr session tags).
Returns a `rack_id` with the newly assigned version on success, NULL
on failure.

#### Delete

```r
rack_delete(id, backend)
```
Delete a board. Dispatches on `id`. If `id` has a version, deletes
that single version. If `id` has no version, deletes all versions
(the entire board). Wraps `pins::pin_delete()` /
`pins::pin_version_delete()`.

### 1.2 — Migrate `manage_project` to the rack API

Replace all direct `pins::` calls in `R/server.R` with rack calls:

| Current code                        | Replacement                        |
|-------------------------------------|------------------------------------|
| `list_workflows(backend)`           | `rack_list(backend)`               |
| `pin_versions(name, backend)`       | `rack_info(backend, id)`           |
| `pins::pin_versions(backend, name)` | `rack_info(backend, id)`           |
| `pins::pin_meta(...)` + `download_board(...)` | `rack_load(backend, id)` |
| `upload_board(...)`                 | `rack_save(backend, data, id)`     |
| `pins::pin_delete(...)`             | `rack_delete(backend, id)`         |
| `pins::pin_version_delete(...)`     | `rack_delete(backend, id)` (with version in id) |

`restore_board()` calls remain at call sites (not inside rack).
This consolidates `manage_project`'s three separate load paths
(load_workflow, load_version, WIP URL observer) into `rack_load()`
+ `restore_board()`, and adds the tag validation it was missing.

### 1.3 — Remove `manage_session`

After `manage_project` uses the rack API, `manage_session` is
redundant. Remove it and its exclusive helpers.

**Delete:**
- `R/session.R` entirely — after moving `upload_board()` and
  `download_board()` into `R/rack.R` (as internal helpers behind
  `rack_save()` / `rack_load()`).
- `pin_list()`, `update_pins()`, `update_versions()`,
  `reset_versions()` from `R/utils.R`.
- `list_workflows()`, `pin_versions()` from `R/utils.R` (replaced by
  `rack_list()`, `rack_info()`).
- `tests/testthat/test-session.R`.
- NAMESPACE exports for `manage_session`, `manage_session_server`,
  `manage_session_ui`.

**Update:**
- `inst/scripts/app-core.R` — switch to `manage_project()`.
- `README.Rmd` / `README.md` — remove `manage_session` references.

No external blockr.* packages import `manage_session`.

---

## Phase 2: Testing strategy

The existing tests run against `pins::board_temp()` (local). This works
for basic operations but misses Connect-specific behaviour — most
importantly that pin names are **namespaced by user** (`user/pin_name`).
The URL API must handle this: two users can each have a pin called
`my_board`, so the URL needs both `user` and `board_name` to be
unambiguous.

### 2.1 — Record-and-replay fixtures from real Connect

Pins uses `httr` under the hood (`rsc_GET`, `rsc_POST`, …).
Use `httptest` to record real HTTP responses from a Connect server into
fixture files (JSON + headers), then replay them in CI by intercepting
`httr` calls. This way tests run against the actual wire format — real
`user/pin_name` namespacing, real metadata shapes, real version
structures — without needing a live server.

**Recording workflow:**
1. Set up a Connect API key + server URL.
2. Write a recording script that performs the operations we need fixtures
   for (see 2.3 below).
3. Run it with `httptest::capture_requests()` → fixture files land in
   `tests/testthat/fixtures/`.
4. Commit the fixtures. No secrets in the recorded data (API keys are
   stripped by `httptest`).

**Replay in tests:**
- Wrap test bodies in `httptest::with_mock_dir("fixtures/...", { ... })`.
- `board_connect(server, key)` calls hit the fixture files instead of
  the network.
- No Connect credentials needed in CI.

### 2.2 — Test matrix

| Layer                | Backend                     | What it covers               |
|----------------------|-----------------------------|------------------------------|
| Unit (CI, fast)      | `board_temp()` / local      | Basic pin ops, tag filtering,|
|                      |                             | `rack_load()`, wrappers     |
| Mock (CI, fast)      | `httptest` fixture replay   | Connect namespacing          |
|                      |                             | (`user/pin`), real metadata  |
|                      |                             | shapes, URL param → load     |
| Integration (manual/ | Real Connect                | Auth, token exchange,        |
| periodic)            |                             | network errors, end-to-end   |

Integration tests are guarded by an env-var / `testthat::skip()` so
they don't run in normal CI.

### 2.3 — Fixtures to record

Run against a real Connect instance to capture responses for:

- `pin_upload()` as user_a → creates `user_a/board_x`.
- `pin_upload()` same name as user_b → creates `user_b/board_x`.
- `pin_search()` → response includes both `user_a/board_x` and
  `user_b/board_x` with tags and metadata.
- `pin_versions(board, "user_a/board_x")` → version list.
- `pin_meta(board, "user_a/board_x", version)` → metadata with
  `pin_hash`, `user$format`, `tags`.
- `pin_download(board, "user_a/board_x", version)` → the actual
  board JSON file.
- `pin_meta(board, "nonexistent/pin")` → error response.
- A pin without blockr-session tags → for tag validation tests.

### 2.4 — Key test scenarios

**Local (`board_temp`):**
- `rack_load()` with valid name → restores board.
- `rack_load()` with missing pin → notification, no crash.
- `rack_load()` with pin missing blockr tags → notification, no restore.
- `rack_list()` filters out non-blockr pins.
- `rack_info()` returns sorted dataframe.

**Mock (httptest fixtures from Connect):**
- `rack_load(name = "user_a/board")` succeeds.
- Same `board_name` saved by `user_a` and `user_b` → two distinct pins,
  each loadable independently.
- `rack_list()` returns both `user_a/board` and `user_b/board`.
- URL `?user=user_a&board_name=my_board` → composes `user_a/my_board`,
  loads correctly.
- URL `?board_name=my_board` without `user` → passes `my_board` as-is,
  Connect resolves to current API key owner.
- URL `?user=user_a&board_name=my_board&version=xyz` → loads that exact
  version.
- Non-existent `user_a/nonexistent` → notification, no crash.
- Pin without blockr tags → notification, no restore.

---

## Phase 3: URL-based board loading

### 3.1 — URL parameters

| Param        | Required | Description                              |
|--------------|----------|------------------------------------------|
| `user`       | no       | Pin owner / namespace for reading.       |
|              |          | On Connect, names are `user/pin_name`.   |
|              |          | Omitted for local backends or to use the |
|              |          | current API key owner's namespace.       |
| `board_name` | yes      | Pin name of the board to load.           |
| `version`    | no       | Pin version ID; omit → load latest.      |

The server composes the full pin name as `user/board_name` when `user`
is present, or just `board_name` otherwise. The `user` param is
**read-only context** — it controls which namespace to load from, but
does not affect where saves go.

### 3.2 — Load from URL (init)

- Remove the current WIP observer (R/server.R lines 382-437).
- Add a one-shot `observeEvent(..., once = TRUE)` that fires on app init.
- Read `session$clientData$url_search` via `getQueryString()`.
- No `board_name` param → return silently (no `stopifnot`).
- If `user` is present, compose `name <- paste(user, board_name, sep = "/")`.
- Call `rack_load()` — tag validation, error handling, notifications
  all come for free.
- Support optional `version` param.
- Set the `board_name` option to the **bare** `board_name` (without
  user prefix) so subsequent saves go to the current user's own
  namespace. This avoids cross-user overwrites — if user_b loads
  `user_a/proj`, saving writes to `user_b/proj`, not `user_a/proj`.

### 3.3 — Update URL on save

After a successful save, update the browser URL bar via
`updateQueryString()` so the URL becomes a shareable, restorable link.

- Compose query params from the current board state: `board_name`
  (from the board option), `user` (from the pin name if namespaced).
- Include `version` if we want version-specific URLs, or omit to
  always point to latest.
- Trigger: the existing `save_btn` observer, after `rack_save()`
  succeeds. The save is the natural moment — the board now has a
  concrete pin name and version.

---

## Future: beyond pins

The rack API (`rack_*` functions) is designed as a backend-agnostic
layer. Currently it wraps pins, but the same interface can front other
storage backends.

### Caching layer

Repeatedly calling `pin_search()` / `pin_meta()` / `pin_versions()` is
expensive — each requires iterating over pins on the remote. A caching
layer between the `rack_*` functions and the pins calls would:

- Cache `rack_list()` results with a TTL (e.g. 30s).
- Cache `rack_info()` per pin name.
- Invalidate on `rack_save()` / `rack_delete()` /
  `rack_delete()` (we know exactly what changed).
- Keep the cache in-process (reactiveVal or environment) — no external
  cache infra needed.

The rack API provides the natural boundary for this: callers never
notice the cache, and invalidation is co-located with the write
operations.

### Alternative backends

Possible future backends behind the same `rack_*` interface:

- **Database** — boards stored as rows/blobs, versions as additional
  rows. `rack_list()` becomes a query, `rack_load()` a row fetch.
  User namespacing is a column, not a naming convention.
- **Git repos** — boards as JSON files, versions as commits/tags.
  `rack_info()` maps to git log, `rack_load()` to checkout.
- **S3 / cloud storage** — pins already supports this, but a direct
  S3 backend could skip pins overhead.

To support this, the `rack_*` functions would dispatch on
`class(backend)` (S3 methods), with the current pins implementation
as the default. New backends implement the same methods.
