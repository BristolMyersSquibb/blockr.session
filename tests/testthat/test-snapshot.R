snapshot_board <- function(...) {
  new_board(blocks = c(a = new_dataset_block("iris"), ...))
}

notice_text <- function(x) paste(as.character(x), collapse = " ")

snapshot_pins <- function(backend) {
  grep(paste0("^", snapshot_prefix()), pins::pin_list(backend), value = TRUE)
}

local_snapshot_backend <- function(interval = 30, env = parent.frame()) {

  backend <- pins::board_temp(versioned = TRUE)

  withr::local_options(
    blockr.session_mgmt_backend = backend,
    blockr.session_autosave = interval,
    .local_envir = env
  )

  backend
}

test_that("an untouched board is never snapshotted", {

  backend <- local_snapshot_backend()

  testServer(
    manage_project_server,
    {
      session$flushReact()
      session$elapse(30 * 1000)
      session$elapse(30 * 1000)

      expect_length(snapshot_pins(backend), 0L)
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "idle")
    )
  )
})

test_that("an idle tick touches the backend not at all", {

  backend <- local_snapshot_backend()

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
      args = NULL,
      board = reactiveValues(board = snapshot_board(), board_id = "quiet")
    )
  )
})

test_that("an edit between ticks writes one snapshot and overwrites the next", {

  backend <- local_snapshot_backend()

  testServer(
    manage_project_server,
    {
      session$flushReact()

      board$board <- snapshot_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_length(snapshot_pins(backend), 1L)

      first <- rack_snapshot_list(backend)[[1L]]

      board$board <- snapshot_board(b = new_subset_block(),
                                    c = new_subset_block())
      session$elapse(30 * 1000)

      expect_length(snapshot_pins(backend), 1L)

      second <- rack_snapshot_list(backend)[[1L]]

      expect_identical(first$id, second$id)
      expect_false(identical(first$content_hash, second$content_hash))
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "edited")
    )
  )
})

test_that("a revert writes the reverted state back", {

  backend <- local_snapshot_backend()

  testServer(
    manage_project_server,
    {
      session$flushReact()

      start <- content_hash(serialize_now())

      board$board <- snapshot_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_false(
        identical(rack_snapshot_list(backend)[[1L]]$content_hash, start)
      )

      board$board <- snapshot_board()
      session$elapse(30 * 1000)

      expect_identical(rack_snapshot_list(backend)[[1L]]$content_hash, start)
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "reverted")
    )
  )
})

test_that("snapshots stay out of the workflow listing", {

  backend <- local_snapshot_backend()

  testServer(
    manage_project_server,
    {
      session$flushReact()

      board$board <- snapshot_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_length(snapshot_pins(backend), 1L)
      expect_length(rack_list(backend), 0L)
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "hidden")
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

      board$board <- snapshot_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_length(snapshot_pins(backend), 0L)
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "off")
    )
  )
})

test_that("a backend without the snapshot capability is never written to", {

  backend <- local_snapshot_backend()

  local_mocked_bindings(
    rack_capabilities = function(backend, ...) list(snapshot = FALSE),
    .package = "blockr.session"
  )

  testServer(
    manage_project_server,
    {
      session$flushReact()

      board$board <- snapshot_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_length(snapshot_pins(backend), 0L)
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "incapable")
    )
  )
})

test_that("a save clears the session slot and moves to the rack pool", {

  backend <- local_snapshot_backend()

  testServer(
    manage_project_server,
    {
      session$flushReact()

      board$board <- snapshot_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_identical(rack_snapshot_list(backend)[[1L]]$pool, "session")

      first_save(session, "moved")
      session$flushReact()

      expect_length(snapshot_pins(backend), 0L)

      board$board <- snapshot_board(b = new_subset_block(),
                                    c = new_subset_block())
      session$elapse(30 * 1000)

      rec <- rack_snapshot_list(backend)[[1L]]

      expect_identical(rec$pool, "rack")
      expect_identical(rec$rack_id, "moved")
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "moved")
    )
  )
})

test_that("a loaded record is not snapshotted until it changes", {

  backend <- local_snapshot_backend()

  board_ser <- NULL

  testServer(
    manage_project_server,
    {
      first_save(session, "loaded")
      board_ser <<- serialize_now("loaded")
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "loaded")
    )
  )

  testServer(
    manage_project_server,
    {
      prev_query("?id=loaded")
      session$flushReact()
      session$elapse(30 * 1000)

      expect_length(snapshot_pins(backend), 0L)

      board$board <- snapshot_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_length(snapshot_pins(backend), 1L)
    },
    args = list(
      board = reactiveValues(
        board = blockr_deser(board_ser),
        board_id = "loaded"
      )
    )
  )
})

test_that("the sweep collects a stamped session past the grace, not before", {

  backend <- local_snapshot_backend()

  stale <- as_snapshot_id(snapshot_pin_name("gone", "session"), backend)
  fresh <- as_snapshot_id(snapshot_pin_name("kept", "session"), backend)

  rack_snapshot(
    backend, stale, list(x = 1),
    meta = list(
      pool = "session",
      ended_at = format_stamp(Sys.time() - 3600)
    )
  )

  rack_snapshot(
    backend, fresh, list(x = 2),
    meta = list(
      pool = "session",
      ended_at = format_stamp(Sys.time() - 60)
    )
  )

  expect_length(snapshot_pins(backend), 2L)

  kept <- snapshot_sweep(backend)

  expect_length(kept, 1L)
  expect_identical(kept[[1L]]$id, fresh$id)
  expect_length(snapshot_pins(backend), 1L)
})

test_that("the sweep collects a snapshot past its TTL", {

  backend <- local_snapshot_backend()

  withr::local_options(blockr.session_snapshot_ttl = 7)

  slot <- as_snapshot_id(snapshot_pin_name("old", "session"), backend)

  rack_snapshot(backend, slot, list(x = 1), meta = list(pool = "session"))

  expect_length(snapshot_sweep(backend), 1L)

  local_mocked_bindings(
    snapshot_ttl_days = function() 0.000001,
    .package = "blockr.session"
  )

  expect_length(snapshot_sweep(backend), 0L)
  expect_length(snapshot_pins(backend), 0L)
})

test_that("a rack snapshot is offered only when it differs from the record", {

  backend <- local_snapshot_backend()

  testServer(
    manage_project_server,
    {
      first_save(session, "offer")
      saved <- serialize_now("offer")

      rid <- as_rack_id(list(id = "offer"), backend)
      slot <- as_snapshot_id(snapshot_pin_name("offer", "rack"), backend)

      rack_snapshot(
        backend, slot, saved,
        meta = list(pool = "rack", rack_id = "offer")
      )

      expect_length(
        snapshot_offers(rack_snapshot_list(backend), rid, backend),
        0L
      )

      rack_snapshot(
        backend, slot, list(nothing = "like the record"),
        meta = list(pool = "rack", rack_id = "offer")
      )

      expect_length(
        snapshot_offers(rack_snapshot_list(backend), rid, backend),
        1L
      )
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "offer")
    )
  )
})

test_that("the snapshot just recovered from is not offered back", {

  backend <- local_snapshot_backend()

  slot <- as_snapshot_id(snapshot_pin_name("mine", "session"), backend)

  rack_snapshot(backend, slot, list(x = 1), meta = list(pool = "session"))

  records <- rack_snapshot_list(backend)

  expect_length(snapshot_offers(records, NULL, backend), 1L)
  expect_length(snapshot_offers(records, NULL, backend, skip = slot$id), 0L)
})

test_that("discarding one offer leaves the others alone", {

  backend <- local_snapshot_backend()

  one <- as_snapshot_id(snapshot_pin_name("one", "session"), backend)
  two <- as_snapshot_id(snapshot_pin_name("two", "session"), backend)

  rack_snapshot(backend, one, list(x = 1), meta = list(pool = "session"))
  rack_snapshot(backend, two, list(x = 2), meta = list(pool = "session"))

  testServer(
    manage_project_server,
    {
      session$flushReact()

      session$setInputs(snapshot_discard = one$id)

      expect_identical(snapshot_pins(backend), two$id)
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "offers")
    )
  )
})

test_that("restoring points the URL at the snapshot without discarding it", {

  backend <- local_snapshot_backend()

  one <- as_snapshot_id(snapshot_pin_name("one", "session"), backend)
  two <- as_snapshot_id(snapshot_pin_name("two", "session"), backend)

  rack_snapshot(backend, one, list(x = 1), meta = list(pool = "session"))
  rack_snapshot(backend, two, list(x = 2), meta = list(pool = "session"))

  reloaded <- NULL
  local_mocked_bindings(
    reload_with_query = function(query, session) reloaded <<- query,
    .package = "blockr.session"
  )

  testServer(
    manage_project_server,
    {
      session$flushReact()

      session$setInputs(snapshot_restore = one$id)

      expect_match(reloaded, paste0("recover=", one$id), fixed = TRUE)
      expect_setequal(snapshot_pins(backend), c(one$id, two$id))
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "restore")
    )
  )
})

test_that("a rack offer carries its record id into the recovery URL", {

  rec <- new_rack_record(
    id = snapshot_pin_name("wf", "rack"),
    name = "wf",
    pool = "rack",
    rack_id = "wf"
  )

  query <- recovery_query(rec, "?other=keep")

  expect_match(query, "recover=blockr-snapshot-r-wf", fixed = TRUE)
  expect_match(query, "id=wf", fixed = TRUE)
  expect_match(query, "other=keep", fixed = TRUE)
})

test_that("parking a draft says so, and a failure says that instead", {

  backend <- local_snapshot_backend()

  toasts <- 0L
  breaks <- FALSE

  # mocks belong out here: local_mocked_bindings() called inside a testServer
  # block does not unwind with it and would stay installed for later tests
  local_mocked_bindings(
    notify = function(...) {
      toasts <<- toasts + 1L
      invisible(NULL)
    },
    rack_snapshot = function(backend, id, data, ...) {
      if (breaks) {
        stop("backend down")
      }
      rack_snapshot.pins_board(backend, id, data, ...)
    },
    .package = "blockr.session"
  )

  testServer(
    manage_project_server,
    {
      session$flushReact()

      board$board <- snapshot_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_equal(output$save_status, "Saved as draft")

      breaks <<- TRUE

      board$board <- snapshot_board(b = new_subset_block(),
                                    c = new_subset_block())
      session$elapse(30 * 1000)

      expect_equal(output$save_status, "Autosave failed")

      # a tick that keeps failing must not raise one toast per interval
      session$elapse(30 * 1000)
      session$elapse(30 * 1000)

      expect_identical(toasts, 0L)
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "status")
    )
  )
})

test_that("a failed write leaves the hash unadvanced, so a tick retries", {

  backend <- local_snapshot_backend()

  fail <- TRUE

  local_mocked_bindings(
    rack_snapshot = function(backend, id, data, ...) {
      if (fail) {
        stop("backend down")
      }
      rack_snapshot.pins_board(backend, id, data, ...)
    },
    .package = "blockr.session"
  )

  testServer(
    manage_project_server,
    {
      session$flushReact()

      board$board <- snapshot_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_length(snapshot_pins(backend), 0L)

      fail <<- FALSE
      session$elapse(30 * 1000)

      expect_length(snapshot_pins(backend), 1L)
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "retry")
    )
  )
})

test_that("a session that ends stamps its snapshot instead of dropping it", {

  backend <- local_snapshot_backend()

  testServer(
    manage_project_server,
    {
      session$flushReact()

      board$board <- snapshot_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_length(snapshot_pins(backend), 1L)

      # a pin version is named to the second and the stamp rewrites the same
      # payload, so ending within the same second as the write collides
      Sys.sleep(1.1)

      # testServer leaves the session open, so the ended callbacks that carry
      # the stamp only run once the session is actually closed
      session$close()
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "ending")
    )
  )

  stamped <- rack_snapshot_list(backend)

  expect_length(stamped, 1L)
  expect_false(is.null(stamped[[1L]]$ended))
  expect_length(snapshot_pins(backend), 1L)
})

test_that("the recovery list opens on the bare handle, not on every load", {

  backend <- local_snapshot_backend()

  slot <- as_snapshot_id(snapshot_pin_name("waiting", "session"), backend)

  rack_snapshot(backend, slot, list(x = 1), meta = list(pool = "session"))

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

      session$setInputs(snapshot_menu = "open")

      expect_identical(modals, 1L)
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "quiet-load")
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
      board = reactiveValues(board = snapshot_board(), board_id = "asked")
    )
  )
})

test_that("the notice disappears once every draft is dealt with", {

  backend <- local_snapshot_backend()

  slot <- as_snapshot_id(snapshot_pin_name("only", "session"), backend)

  rack_snapshot(backend, slot, list(x = 1), meta = list(pool = "session"))

  testServer(
    manage_project_server,
    {
      session$flushReact()

      expect_match(notice_text(output$recovery_notice), "1 draft")

      session$setInputs(snapshot_discard = slot$id)

      expect_null(output$recovery_notice)
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "cleared")
    )
  )
})

test_that("the Connect workflow listing holds snapshots back", {

  backend <- mock_board_connect()

  items <- list(
    list(
      content_category = "pin",
      name = "a-workflow",
      title = "A workflow",
      last_deployed_time = "2026-08-16T10:00:00Z",
      owner = list(username = "user_a")
    ),
    list(
      content_category = "pin",
      name = snapshot_pin_name("secret", "session"),
      title = "blockr snapshot: secret",
      last_deployed_time = "2026-08-16T11:00:00Z",
      owner = list(username = "user_a")
    ),
    list(
      content_category = "pin",
      name = snapshot_pin_name("a-workflow", "rack"),
      title = "blockr snapshot: A workflow",
      last_deployed_time = "2026-08-16T11:30:00Z",
      owner = list(username = "user_a")
    )
  )

  records <- connect_pin_records(backend, items)

  expect_length(records, 1L)
  expect_identical(records[[1L]]$id, "a-workflow")
})

test_that("a recovering session adopts the slot it was offered", {

  backend <- local_snapshot_backend()

  slot <- as_snapshot_id(snapshot_pin_name("adopted", "session"), backend)

  rack_snapshot(backend, slot, list(x = 1), meta = list(pool = "session"))

  testServer(
    manage_project_server,
    {
      prev_query(paste0("?recover=", slot$id))
      session$flushReact()

      board$board <- snapshot_board(b = new_subset_block())
      session$elapse(30 * 1000)

      expect_identical(snapshot_pins(backend), slot$id)
    },
    args = list(
      board = reactiveValues(board = snapshot_board(), board_id = "adopted")
    )
  )
})
