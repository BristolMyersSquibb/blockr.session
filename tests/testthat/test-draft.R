draft_board <- function(...) {
  new_board(blocks = c(a = new_dataset_block("iris"), ...))
}

notice_text <- function(x) paste(as.character(x), collapse = " ")

draft_pins <- function(backend) {
  grep("^blockr-draft-", pins::pin_list(backend), value = TRUE)
}

plant_draft <- function(backend, key, kind = "session", data = list(x = 1)) {
  rack_create(backend, data, id = key, name = key, draft = kind)
}

local_draft_backend <- function(interval = 30, env = parent.frame()) {

  backend <- pins::board_temp(versioned = TRUE)

  withr::local_options(
    blockr.session_mgmt_backend = backend,
    blockr.session_autosave = interval,
    .local_envir = env
  )

  backend
}

test_that("a draft is an ordinary record at a reserved id", {

  backend <- local_draft_backend()

  id <- plant_draft(backend, "abc")

  expect_true(startsWith(id$id, "blockr-draft-session-"))
  expect_identical(draft_record_kind(id$id), "session")
  expect_identical(draft_record_hash(id$id), draft_key_hash("abc"))
  expect_true(rack_exists(id, backend))
  expect_equal(rack_load(id, backend), list(x = 1))

  expect_no_error(rack_purge(id, backend))
  expect_length(draft_pins(backend), 0L)
})

test_that("a minted slot is overwritten rather than accumulating", {

  backend <- local_draft_backend()

  slot <- plant_draft(backend, "once", data = list(x = 1))
  Sys.sleep(1.1)
  again <- plant_draft(backend, slot$id, data = list(x = 2))

  expect_identical(again$id, slot$id)
  expect_length(draft_pins(backend), 1L)
  expect_equal(rack_load(slot, backend), list(x = 2))
})

test_that("a fresh mint for the same key is a distinct slot", {

  backend <- local_draft_backend()

  first <- plant_draft(backend, "twice", data = list(x = 1))
  Sys.sleep(1.1)
  second <- plant_draft(backend, "twice", data = list(x = 2))

  expect_false(identical(first$id, second$id))
  expect_identical(draft_record_hash(first$id), draft_record_hash(second$id))
  expect_length(draft_pins(backend), 2L)
})


test_that("a later session writes to the same record slot", {

  backend <- local_draft_backend()

  earlier <- plant_draft(backend, "shared", "record")

  testServer(
    manage_project_server,
    {
      first_save(session, "shared")
      session$flushReact()

      board$board <- draft_board(b = new_subset_block())
      session$elapse(30 * 1000)

      drafts <- rack_records(backend, draft = "record")

      expect_length(drafts, 1L)
      expect_identical(drafts[[1L]]$id, earlier$id)
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "shared")
    )
  )
})

test_that("a record draft never expires, whatever its age", {

  backend <- local_draft_backend()

  plant_draft(backend, "kept", "record")

  local_mocked_bindings(
    draft_ttl_days = function() 0.000001,
    .package = "blockr.session"
  )

  expect_length(sweep_drafts(backend), 1L)
  expect_length(draft_pins(backend), 1L)
})

test_that("a save drops the draft it supersedes", {

  backend <- local_draft_backend()

  testServer(
    manage_project_server,
    {
      first_save(session, "superseded")
      session$flushReact()

      board$board <- draft_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_length(rack_records(backend, draft = "record"), 1L)

      session$setInputs(save_btn = 2)
      session$flushReact()

      expect_length(rack_records(backend, draft = TRUE), 0L)
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "superseded")
    )
  )
})

test_that("the reserved namespace is refused for a record the user saves", {

  backend <- local_draft_backend()

  expect_error(
    rack_create(backend, list(x = 1), id = "blockr-draft-session-1-abc",
                name = "sneaky"),
    class = "rack_id_reserved"
  )

  expect_error(
    draft_record_id("blockr-draft-record-abc", FALSE),
    class = "rack_id_reserved"
  )

  expect_identical(draft_record_id("ordinary", FALSE), "ordinary")
})

test_that("rack_records partitions a listing by kind", {

  local_mocked_bindings(
    rack_list = function(backend, ...) {
      list(
        new_rack_record(id = "a-workflow", name = "A workflow"),
        new_rack_record(id = "blockr-draft-record-aaaaaaaa", name = "A"),
        new_rack_record(id = "blockr-draft-session-2-bbbbbbbb", name = "X")
      )
    },
    .package = "blockr.session"
  )

  expect_identical(chr_xtr(rack_records(NULL), "id"), "a-workflow")
  expect_length(rack_records(NULL, draft = TRUE), 2L)
  expect_identical(
    chr_xtr(rack_records(NULL, draft = "record"), "id"),
    "blockr-draft-record-aaaaaaaa"
  )
  expect_identical(
    chr_xtr(rack_records(NULL, draft = "session"), "id"),
    "blockr-draft-session-2-bbbbbbbb"
  )
})

test_that("an untouched board is never drafted", {

  backend <- local_draft_backend()

  testServer(
    manage_project_server,
    {
      session$flushReact()
      session$elapse(30 * 1000)
      session$elapse(30 * 1000)

      expect_length(draft_pins(backend), 0L)
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "idle")
    )
  )
})

test_that("an idle tick touches the backend not at all", {

  backend <- local_draft_backend()

  calls <- 0L
  local_mocked_bindings(
    pin_upload = function(...) {
      calls <<- calls + 1L
      NULL
    },
    pin_meta = function(...) {
      calls <<- calls + 1L
      NULL
    },
    .package = "pins"
  )

  testServer(
    manage_project_server,
    {
      session$flushReact()
      session$elapse(30 * 1000)
      session$elapse(30 * 1000)

      expect_identical(calls, 0L)
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "quiet")
    )
  )
})

test_that("an edit between ticks writes one draft and overwrites the next", {

  backend <- local_draft_backend()

  testServer(
    manage_project_server,
    {
      session$flushReact()

      board$board <- draft_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_length(draft_pins(backend), 1L)

      first <- draft_pins(backend)

      board$board <- draft_board(b = new_subset_block(),
                                 c = new_subset_block())
      session$elapse(30 * 1000)

      expect_identical(draft_pins(backend), first)
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "edited")
    )
  )
})

test_that("a revert writes the reverted state back", {

  backend <- local_draft_backend()

  testServer(
    manage_project_server,
    {
      session$flushReact()

      start <- content_hash(serialize_now())

      board$board <- draft_board(b = new_subset_block())
      session$elapse(30 * 1000)

      slot <- as_rack_id(list(id = draft_pins(backend)), backend)

      expect_false(identical(rack_content_hash(slot, backend), start))

      board$board <- draft_board()
      session$elapse(30 * 1000)

      expect_identical(rack_content_hash(slot, backend), start)
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "reverted")
    )
  )
})

test_that("drafts stay out of the workflow listing", {

  backend <- local_draft_backend()

  testServer(
    manage_project_server,
    {
      session$flushReact()

      board$board <- draft_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_length(draft_pins(backend), 1L)
      expect_length(rack_records(backend), 0L)
      expect_length(rack_list(backend), 1L)
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "hidden")
    )
  )
})

test_that("autosave stays off unless an interval is configured", {

  backend <- pins::board_temp(versioned = TRUE)

  withr::local_options(
    blockr.session_mgmt_backend = backend,
    blockr.session_autosave = NULL
  )

  testServer(
    manage_project_server,
    {
      session$flushReact()

      board$board <- draft_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_length(draft_pins(backend), 0L)
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "off")
    )
  )
})

test_that("a save clears the session draft and moves to the record kind", {

  backend <- local_draft_backend()

  testServer(
    manage_project_server,
    {
      session$flushReact()

      board$board <- draft_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_length(rack_records(backend, draft = "session"), 1L)

      first_save(session, "moved")
      session$flushReact()

      expect_length(draft_pins(backend), 0L)

      board$board <- draft_board(b = new_subset_block(),
                                 c = new_subset_block())
      session$elapse(30 * 1000)

      drafts <- rack_records(backend, draft = "record")

      expect_length(drafts, 1L)
      expect_identical(
        draft_record_hash(drafts[[1L]]$id), draft_key_hash("moved")
      )
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "moved")
    )
  )
})

test_that("a loaded record is not drafted until it changes", {

  backend <- local_draft_backend()

  board_ser <- NULL

  testServer(
    manage_project_server,
    {
      first_save(session, "loaded")
      board_ser <<- serialize_now("loaded")
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "loaded")
    )
  )

  testServer(
    manage_project_server,
    {
      prev_query("?id=loaded")
      session$flushReact()
      session$elapse(30 * 1000)

      expect_length(draft_pins(backend), 0L)

      board$board <- draft_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_length(draft_pins(backend), 1L)
    },
    args = list(
      board = reactiveValues(
        board = blockr_deser(board_ser),
        board_id = "loaded"
      )
    )
  )
})

test_that("the sweep collects a draft past its TTL", {

  backend <- local_draft_backend()

  plant_draft(backend, "old")

  expect_length(sweep_drafts(backend), 1L)

  past <- Sys.time() + (draft_ttl_days() + 1) * 86400

  expect_length(sweep_drafts(backend, now = past), 0L)
  expect_length(draft_pins(backend), 0L)
})

test_that("a record draft is offered for its own record and no other", {

  backend <- local_draft_backend()

  plant_draft(backend, "offer", "record")

  records <- rack_records(backend, draft = TRUE)

  mine <- as_rack_id(list(id = "offer"), backend)
  other <- as_rack_id(list(id = "elsewhere"), backend)

  expect_length(draft_offers(records, mine, backend), 1L)
  expect_length(draft_offers(records, other, backend), 0L)
  expect_length(draft_offers(records, NULL, backend), 0L)
})

test_that("the draft just recovered from is not offered back", {

  backend <- local_draft_backend()

  slot <- plant_draft(backend, "mine")

  records <- rack_records(backend, draft = TRUE)

  expect_length(draft_offers(records, NULL, backend), 1L)
  expect_length(draft_offers(records, NULL, backend, skip = slot$id), 0L)
})

test_that("parking a draft says so, and a failure says that instead", {

  backend <- local_draft_backend()

  toasts <- 0L
  breaks <- FALSE
  real_create <- rack_create

  # mocks belong out here: local_mocked_bindings() called inside a testServer
  # block does not unwind with it and would stay installed for later tests
  local_mocked_bindings(
    notify = function(...) {
      toasts <<- toasts + 1L
      invisible(NULL)
    },
    rack_create = function(backend, data, id, name, draft = FALSE, ...) {
      if (breaks) {
        stop("backend down")
      }
      real_create(backend, data, id, name, draft = draft, ...)
    },
    .package = "blockr.session"
  )

  testServer(
    manage_project_server,
    {
      session$flushReact()

      board$board <- draft_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_equal(output$save_status, "Saved as draft")

      breaks <<- TRUE

      board$board <- draft_board(b = new_subset_block(),
                                 c = new_subset_block())
      session$elapse(30 * 1000)

      expect_equal(output$save_status, "Autosave failed")

      # a tick that keeps failing must not raise one toast per interval
      session$elapse(30 * 1000)
      session$elapse(30 * 1000)

      expect_identical(toasts, 0L)
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "status")
    )
  )
})

test_that("a failed write leaves the hash unadvanced, so a tick retries", {

  backend <- local_draft_backend()

  fail <- TRUE
  real_create <- rack_create

  local_mocked_bindings(
    rack_create = function(backend, data, id, name, draft = FALSE, ...) {
      if (fail) {
        stop("backend down")
      }
      real_create(backend, data, id, name, draft = draft, ...)
    },
    .package = "blockr.session"
  )

  testServer(
    manage_project_server,
    {
      session$flushReact()

      board$board <- draft_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_length(draft_pins(backend), 0L)

      fail <<- FALSE
      session$elapse(30 * 1000)

      expect_length(draft_pins(backend), 1L)
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "retry")
    )
  )
})

test_that("the recovery list opens on the bare handle, not on every load", {

  backend <- local_draft_backend()

  plant_draft(backend, "waiting")

  modals <- 0L
  local_mocked_bindings(
    showModal = function(...) {
      modals <<- modals + 1L
      invisible(NULL)
    },
    .package = "blockr.session"
  )

  testServer(
    manage_project_server,
    {
      session$flushReact()

      expect_identical(modals, 0L)
      expect_match(notice_text(output$recovery_notice), "1 draft")

      session$setInputs(draft_menu = "open")

      expect_identical(modals, 1L)
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "quiet-load")
    )
  )

  testServer(
    manage_project_server,
    {
      prev_query("?recover")
      session$flushReact()

      expect_identical(modals, 2L)
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "asked")
    )
  )
})

test_that("the notice disappears once every draft is dealt with", {

  backend <- local_draft_backend()

  slot <- plant_draft(backend, "only")

  testServer(
    manage_project_server,
    {
      session$flushReact()

      expect_match(notice_text(output$recovery_notice), "1 draft")

      session$setInputs(draft_discard = slot$id)

      expect_null(output$recovery_notice)
      expect_length(draft_pins(backend), 0L)
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "cleared")
    )
  )
})

test_that("discarding one offer leaves the others alone", {

  backend <- local_draft_backend()

  one <- plant_draft(backend, "one")
  two <- plant_draft(backend, "two")

  testServer(
    manage_project_server,
    {
      session$flushReact()

      session$setInputs(draft_discard = one$id)

      expect_identical(draft_pins(backend), two$id)
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "offers")
    )
  )
})

test_that("restoring points the URL at the draft without discarding it", {

  backend <- local_draft_backend()

  one <- plant_draft(backend, "one")
  two <- plant_draft(backend, "two")

  reloaded <- NULL
  local_mocked_bindings(
    reload_with_query = function(query, session) reloaded <<- query,
    .package = "blockr.session"
  )

  testServer(
    manage_project_server,
    {
      session$flushReact()

      session$setInputs(draft_restore = one$id)

      expect_match(reloaded, paste0("recover=", one$id), fixed = TRUE)
      expect_setequal(draft_pins(backend), c(one$id, two$id))
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "restore")
    )
  )
})

test_that("a record offer carries its workflow id into the recovery URL", {

  rec <- new_rack_record(id = "blockr-draft-record-abcdefgh", name = "wf")

  query <- recovery_query(rec, "?other=keep", as_rack_id(list(id = "wf"),
                                                         pins::board_temp()))

  expect_match(query, "recover=blockr-draft-record-abcdefgh", fixed = TRUE)
  expect_match(query, "id=wf", fixed = TRUE)
  expect_match(query, "other=keep", fixed = TRUE)
})

test_that("recovering keeps the old draft until the new one holds the work", {

  backend <- local_draft_backend()

  slot <- plant_draft(backend, "recovered")

  testServer(
    manage_project_server,
    {
      prev_query(paste0("?recover=", slot$id))
      session$flushReact()

      # nothing written yet, so the handle in the URL still resolves
      session$elapse(30 * 1000)

      expect_identical(draft_pins(backend), slot$id)

      board$board <- draft_board(b = new_subset_block())
      session$elapse(30 * 1000)

      drafts <- draft_pins(backend)

      expect_length(drafts, 1L)
      expect_false(identical(drafts, slot$id))
    },
    args = list(
      board = reactiveValues(board = draft_board(), board_id = "recovered")
    )
  )
})
