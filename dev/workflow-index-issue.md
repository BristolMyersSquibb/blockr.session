## Problem

The workflow loading menu (the `layers` navbar dropdown) lags in production,
and it gets worse the longer a board is used. Root cause is in how
`rack_list()` lists workflows on a **file/folder pins board** (what prod runs
on):

- `pin_search()` → `multi_meta()` calls `pin_meta()` per pin, which calls
  `pin_versions()` (a `dir_ls` over every version directory of that pin) just to
  find the latest — so listing is `O(pins × versions-per-pin)`.
- `record_time_ago()` then calls `pin_versions()` **again per row** (an N+1) to
  fetch a timestamp that `pin_search` already returned.
- `versioned = TRUE` saves accumulate version directories forever, so each
  `pin_versions` listing grows unboundedly → the "laggier over time" effect.
- All of this reruns on every dropdown/modal render.

Discussion also covered the loading-menu UX itself (only the last 4 workflows
are one click; older ones need a modal). Mockups for that are in
`dev/loading-menu-proposals.html` on this branch — **not implemented here**, this
issue is the backend performance fix that the UI redesign depends on.

## What's on `feat/workflow-index-pin`

A single `blockr-workflow-index` pin caches `{id, name, created}` for every
workflow. `rack_list()` reads that one file instead of scanning the board.

- **Derived cache, pins stay source of truth.** Mutations patch one row;
  `rebuild_index()` (the old full scan) survives only as a self-healing
  fallback, triggered when the index is missing **or** schema-invalid. This also
  means existing prod boards migrate automatically on first list.
- **Hooks:** `rack_upload`/`rack_rename` upsert, `rack_purge` removes,
  `rack_delete` reconciles the changed latest (or removes if the pin is emptied).
- **Scoped to folder boards** via `board_uses_index()`. Connect keeps its own
  `rack_list`; index mutations no-op there.
- **N+1 removed:** `record_time_ago()` reads the record's carried `created`.
  Measured: list+render of 8 workflows makes ≤1 `pin_versions` call (was 8+).
- **Version pruning:** every save prunes to `blockr.session_max_versions`
  (default 20), bounding per-workflow load cost and disk.
- **Best-effort writes:** all index maintenance is wrapped in `tryCatch` so it
  can never break the underlying save/delete.

Tests: `tests/testthat/test-index.R` (create/list/order, rename+purge, missing-
and corrupt-index rebuild, pruning, Connect guard). Full suite green (357 pass,
1 CRAN-skip). Existing pin-count assertions updated to exclude the index pin via
a `workflow_pins()` helper.

Key files: `R/index.R` (new), `R/pins.R` (rack_list + hooks), `R/utils.R`
(`record_time_ago`).

## Scaling

`rack_list` goes from `O(pins × versions)` filesystem round-trips to one file
read + one parse. That parse is `O(N)` in bytes but a 1000-row index is ~100KB
(couple ms), so **100 vs 1000 workflows is effectively flat** until tens of
thousands. Saves stay cheap: patch one row in memory, rewrite one small file.

## Deliberately deferred (your call)

- **Async / background reconcile.** Saves patch the index inline (fast, durable);
  a full reconcile scan could run off the session (ExtendedTask/mirai, or a
  separate scheduled process) for drift repair. Safe under a **merge rule**:
  foreground saves are authoritative and patch inline; a background scan only
  *unions in* what's missing, never overwrites from a stale snapshot. Not built.
- **Session-level `rack_list` cache** (reactiveVal invalidated on mutation) —
  would save the one-file read on repeat renders. Small, not done here.
- **Connect.** Left on its own `rack_list`. Whether Connect wants an equivalent
  index (and the Sharing tab, which surfaces *other* users' workflows and can't
  live in your per-user index) is an open question.
- **The loading-menu UI redesign** (`dev/loading-menu-proposals.html`) —
  recommended pairing is search-first box + scrollable time-grouped list, which
  makes the "View all" modal optional and is client-side (free per keystroke).

## Decision for @nicolas

Build on this branch or start fresh? The core (index + rebuild fallback + hooks
+ prune + N+1 fix) is working and tested; the open design choices above are the
parts worth a second opinion before hardening.
