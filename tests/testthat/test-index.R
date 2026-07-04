# Workflow index ------------------------------------------------------------

test_that("a save creates the index and rack_list reads it", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "wf-a", name = "A")

  expect_true(index_pin_name() %in% pins::pin_list(backend))

  records <- rack_list(backend)
  expect_length(records, 1L)
  expect_equal(records[[1L]]$id, "wf-a")
  expect_equal(records[[1L]]$name, "A")
  # the record carries a timestamp so the list render needs no per-row lookup
  expect_false(is.null(records[[1L]]$created))
})

test_that("rack_list is ordered most-recent-first", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "old", name = "Old")
  Sys.sleep(1.1)
  rack_create(backend, list(blocks = list()), id = "new", name = "New")

  expect_equal(chr_xtr(rack_list(backend), "id"), c("new", "old"))
})

test_that("rename and purge are reflected in the index", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "keep", name = "Keep")
  rack_create(backend, list(blocks = list()), id = "gone", name = "Gone")

  Sys.sleep(1.1)
  rack_rename(new_rack_id_pins("keep"), backend, "Kept")
  rack_purge(new_rack_id_pins("gone"), backend)

  records <- rack_list(backend)
  expect_equal(chr_xtr(records, "id"), "keep")
  expect_equal(records[[1L]]$name, "Kept")
})

test_that("a missing index is rebuilt from a board scan", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "wf-1", name = "One")
  rack_create(backend, list(blocks = list()), id = "wf-2", name = "Two")

  pins::pin_delete(backend, index_pin_name())

  records <- rack_list(backend)
  expect_setequal(chr_xtr(records, "id"), c("wf-1", "wf-2"))
  expect_true(index_pin_name() %in% pins::pin_list(backend))
})

test_that("a corrupt index is rejected and rebuilt", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "wf-1", name = "One")

  # invalid shape: no records data frame / wrong schema
  suppressMessages(
    pins::pin_write(backend, list(bogus = TRUE), name = index_pin_name(),
                    type = "rds", versioned = FALSE)
  )
  expect_null(read_index(backend))

  records <- rack_list(backend)
  expect_equal(chr_xtr(records, "id"), "wf-1")
})

test_that("saves prune version history to the configured cap", {

  withr::local_options(blockr.session_max_versions = 2L)
  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(a = 0), id = "wf", name = "WF")
  for (i in 1:3) {
    Sys.sleep(1.1)
    rack_append(new_rack_id_pins("wf"), backend, list(a = i))
  }

  expect_lte(nrow(rack_info(new_rack_id_pins("wf"), backend)), 2L)
})

test_that("the index is scoped to folder boards, not Connect", {

  connect <- structure(list(), class = c("pins_board_connect", "pins_board"))
  folder <- structure(list(), class = c("pins_board_folder", "pins_board"))

  expect_false(board_uses_index(connect))
  expect_true(board_uses_index(folder))

  # index mutations no-op on a Connect board (no pin I/O attempted)
  expect_invisible(index_upsert(connect, "x", "X", Sys.time()))
  expect_invisible(index_remove(connect, "x"))
})
