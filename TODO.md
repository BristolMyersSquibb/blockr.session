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

### 2.1 — What we tried (and why it didn't work)

#### `httptest`

The original plan was to use `httptest` to record real HTTP transactions
from a Connect server and replay them in CI. `httptest` intercepts `httr`
calls, captures responses as JSON fixture files keyed by URL + HTTP
method, and plays them back without a network connection.

The fundamental problem: **`httptest` does not take request headers into
account when matching fixtures**. Pins authenticates to Connect via an
`Authorization` header carrying the API key. When two API keys (user A
and user B) make the same request to the same URL, `httptest` treats them
as identical and returns the same fixture — it cannot distinguish which
user is making the call. This makes it impossible to test user-namespaced
behaviour (e.g. user A's `pin_search()` returning `user_a/board` while
user B's returns `user_b/board`).

#### `vcr` / `webmockr`

We next investigated `vcr` (cassette-based HTTP recording) and its
companion `webmockr` (which stubs `httr` and `httr2` at the adapter
level). Both operate at the HTTP layer, not the R-function layer.

The blocker here was **how pins constructs HTTP requests internally**.
Pins' Connect backend uses internal helpers (`rsc_GET`, `rsc_POST`, …)
that build `httr` calls in ways the `webmockr` adapters could not
intercept reliably. Stubs registered against the expected URLs were not
matched, meaning actual network calls were still attempted. Even when
matching could be forced, the cassette format is keyed on URL + method
(same header-blindness as `httptest`), so the multi-user scenario
remained untestable.

### 2.2 — The solution: R-level mocking with serialised fixtures

Rather than intercepting HTTP traffic, we intercept at the **pins
function boundary** using `testthat::local_mocked_bindings()`. The key
insight is that we only care about what the rack functions do with the
values that `pins::` functions return — not about the HTTP wire format.

#### Two-phase design

**Recording** (runs only with real Connect credentials, never in CI):

`tests/testthat/setup.R` checks for `CONNECT_SERVER`,
`CONNECT_API_KEY_A`, and `CONNECT_API_KEY_B` environment variables. When
all three are set it creates real `board_a` / `board_b` objects, builds a
substitution map (real server hostname → `connect.example.com`, real
account names → `user_a` / `user_b`), and sets the
`connect_test_recording` active binding to `TRUE`.

Each test that needs Connect data calls `connect_fixture(name, record =
function() { ... })`. When `connect_test_recording` is `TRUE`, the
`record` lambda runs against the live server. The R object it returns
(the actual return value of `pins::pin_search()`, `pins::pin_versions()`,
etc.) is serialised with `jsonlite::serializeJSON()`, the substitution
map is applied to scrub secrets, and the result is written to
`tests/testthat/_fixtures/connect/<name>.json`. The fixture files are
committed to the repository. No secrets appear in the committed data.

**Replay** (always available, no credentials required):

When `connect_test_recording` is `FALSE` (i.e. in CI or any environment
without credentials), `connect_fixture()` reads the committed JSON file
and deserialises it with `jsonlite::unserializeJSON()`, recovering the
exact R object that the real pins call returned.

Tests then pass this deserialised object to `local_mocked_bindings()`,
which replaces the relevant `pins::` functions (e.g. `pin_search`,
`pin_versions`, `pin_meta`, `pin_download`) for the duration of the test.
The rack functions under test call the mocked bindings and receive the
same R objects that a real Connect server would have returned, with
Connect-accurate data shapes (qualified names, metadata layout, version
structure, tag format).

`mock_board_connect()` in `R/tests.R` creates a minimal fake
`pins_board_connect` S3 object (with the right class and plausible
fields) without making any network calls. This object is passed as the
`backend` argument so that S3 dispatch works correctly against the
Connect methods, while the stubbed pins functions handle all the actual
"data".

If a fixture file is missing and the test is not in recording mode,
`connect_fixture()` calls `testthat::skip()`, so the test is skipped
rather than failing.

#### Why this works where the others didn't

- No HTTP interception at all — headers are irrelevant.
- Each `record` lambda can target `board_a` or `board_b` independently,
  capturing their distinct responses, and each gets its own fixture file.
- `local_mocked_bindings()` is a standard testthat mechanism; it is
  unaffected by how pins internally constructs HTTP requests.
- Fixtures are plain R objects serialised to JSON — easy to inspect,
  diff, and update.

### 2.3 — Test matrix

| Layer                | Backend                      | What it covers                |
|----------------------|------------------------------|-------------------------------|
| Unit (CI, fast)      | `board_temp()` / local       | Basic pin ops, tag filtering, |
|                      |                              | `rack_load()`, wrappers       |
| Mock (CI, fast)      | `mock_board_connect()` +     | Connect namespacing           |
|                      | `local_mocked_bindings()`    | (`user/pin`), real metadata   |
|                      | + committed JSON fixtures    | shapes, URL param → load      |
| Integration (manual/ | Real Connect                 | Auth, token exchange,         |
| periodic)            | (`CONNECT_*` env vars set)   | network errors, end-to-end    |

Integration tests are guarded by `connect_test_recording` / the env-var
check in `setup.R`, so they don't run in normal CI.

### 2.4 — Fixtures recorded

The following fixture files live in `tests/testthat/_fixtures/connect/`.
Each captures the R-level return value of the corresponding `pins::`
call against a real Connect server:

| File                                 | Recorded call                                              |
|--------------------------------------|------------------------------------------------------------|
| `pin_search.json`                    | `pin_search(board_a)` — mix of tagged + untagged           |
| `pin_versions_tagged.json`           | `pin_versions(board_a, "user_a/blockr-fixture-board")` (2 versions) |
| `pin_versions_untagged.json`         | `pin_versions(board_a, "user_a/blockr-fixture-plain")`     |
| `pin_versions_single.json`           | `pin_versions(board_a, "user_a/blockr-fixture-new")` (1 version) |
| `pin_versions_error.json`            | Error caught from `pin_versions(board_a, "user_a/nonexistent-pin")` |
| `pin_meta_tagged.json`               | `pin_meta(board_a, qualified, version = latest_ver)`       |
| `pin_meta_untagged.json`             | `pin_meta(board_a, qualified, version = untagged_ver)`     |
| `pin_meta_specific_version.json`     | `pin_meta(board_a, qualified, version = older_ver)`        |
| `pin_download_tagged.json`           | `fromJSON(pin_download(..., latest_ver, hash))`            |
| `pin_download_specific_version.json` | `fromJSON(pin_download(..., older_ver, hash))`             |

### 2.5 — Key test scenarios

**Local (`board_temp`):**
- `rack_load()` with valid name → restores board.
- `rack_load()` with missing pin → error, no crash.
- `rack_load()` with pin missing blockr tags → error, no restore.
- `rack_list()` filters out non-blockr pins.
- `rack_info()` returns sorted dataframe.

**Mock (Connect fixtures + `local_mocked_bindings`):**
- `rack_list()` on Connect returns `rack_id_pins_connect` objects and
  filters out untagged pins.
- `rack_list()` correctly splits `user/name` qualified names into `user`
  and `name` fields.
- `rack_load(id = new_rack_id_pins_connect("user_a", "board"))` succeeds
  and requests `"user_a/board"` from pins.
- `rack_load()` with a specific version skips the `pin_versions` call.
- `rack_load()` from `user_b`'s board against `user_a`'s pin still uses
  the qualified name.
- `rack_load()` for a missing pin → `rack_load_no_versions` error.
- `rack_load()` for a pin without blockr tags → `rack_load_invalid_tags`
  error.
- `rack_save()` on Connect returns `rack_id_pins_connect` with correct
  `user` and `name`.
- `rack_save()` passes the bare name to `pin_upload` but the qualified
  name to `pin_versions`.
- `rack_delete()` without a version resolves the latest via
  `pin_versions` and calls `pin_version_delete` with the qualified name.
- `rack_delete()` with a version calls `pin_version_delete` directly,
  skipping `pin_versions`.
- `rack_delete()` for a missing pin → `rack_delete_no_versions` error.
- `rack_purge()` calls `pin_delete` with the qualified name.

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
