# Drive the first-save id chooser: open it with Save, then confirm the id.
first_save <- function(session, id, n = 1L) {
  session$setInputs(save_btn = n)
  session$setInputs(rack_id_input = id, rack_id_confirm = n)
}

test_that("manage_project plugin", {

  a <- manage_project()

  expect_s3_class(a, "plugin")
  expect_s3_class(a, "preserve_board")
  expect_true(is_plugin(a))

  expect_snapshot(print(a))
})

test_that("manage_project server", {

  withr::local_options(
    blockr.session_mgmt_backend = pins::board_temp(versioned = TRUE)
  )

  test_board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    links = links(from = "a", to = "b")
  )

  testServer(
    manage_project_server,
    {
      first_save(session, "test")
      expect_no_error(session$flushReact())
    },
    args = list(board = reactiveValues(board = test_board, board_id = "test"))
  )

  testServer(
    manage_project_server,
    {
      session$setInputs(new_btn = 1)
      expect_no_error(session$flushReact())
    },
    args = list(board = reactiveValues(board = test_board, board_id = "test"))
  )

  testServer(
    manage_project_server,
    {
      session$setInputs(view_all_workflows = 1)
      expect_no_error(session$flushReact())
    },
    args = list(board = reactiveValues(board = test_board, board_id = "test"))
  )
})

test_that("manage_project ui", {
  expect_s3_class(manage_project_ui("project", new_board()), "shiny.tag.list")
})

test_that("manage_project ui puts Save As in a split-button dropdown (#67)", {
  doc <- xml2::read_html(
    as.character(manage_project_ui("project", new_board()))
  )

  save_btn <- xml2::xml_find_all(doc, "//button[@id='project-save_btn']")
  expect_length(save_btn, 1)
  expect_match(xml2::xml_attr(save_btn, "onclick"), "save_btn", fixed = TRUE)

  toggle <- xml2::xml_find_all(
    doc,
    paste0(
      "//button[@data-bs-toggle='dropdown' and contains(",
      "concat(' ', normalize-space(@class), ' '), ' dropdown-toggle ')]"
    )
  )
  expect_length(toggle, 1)

  save_as <- xml2::xml_find_all(doc, "//button[@id='project-save_as_btn']")
  expect_length(save_as, 1)
  expect_match(xml2::xml_attr(save_as, "class"), "dropdown-item")
  expect_match(xml2::xml_attr(save_as, "onclick"), "save_as_btn", fixed = TRUE)
})

test_that("save persists board to backend", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      first_save(session, "save-test")
      expect_equal(output$save_status, "Just now")
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "save-test")
    )
  )

  saved <- pins::pin_search(backend)
  expect_true("save-test" %in% saved$name)
})

test_that("recent_workflows renders the saved record keyed on its id (#61)", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      first_save(session, "rebel_eyas")
      html <- as.character(output$recent_workflows)

      expect_true(any(grepl("rebel_eyas", html, fixed = TRUE)))
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "rebel_eyas")
    )
  )
})

test_that("loading a record seeds the board name from the stored name (#61)", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  rack_create(backend, list(blocks = list()), id = "seed-name",
              name = "Stored Display Name")

  captured <- NULL
  local_mocked_bindings(
    set_board_option_value = function(opt, val, board, ...) {
      if (identical(opt, "board_name")) {
        captured <<- val
      }
      invisible(val)
    },
    .package = "blockr.session"
  )

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      prev_query("?id=seed-name")
      session$flushReact()

      expect_equal(captured, "Stored Display Name")
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "seed-name")
    )
  )
})

test_that("a cold re-save appends to the existing board_id record (#61)", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  # a prior session already saved this board id
  rack_create(backend, list(blocks = list()), id = "cold-board", name = "Cold")

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      # cold load (no current_id) but the board_id record exists -> append
      session$setInputs(save_btn = 1)
      session$flushReact()
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "cold-board")
    )
  )

  expect_length(pins::pin_list(backend), 1L)
  expect_equal(nrow(pins::pin_versions(backend, "cold-board")), 2L)
})

test_that("saving after a content change creates a new version", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      first_save(session, "version-test")
      Sys.sleep(1)
      # a real content change between saves
      board$board <- new_board(
        blocks = c(a = new_dataset_block("iris"), b = new_subset_block())
      )
      session$setInputs(save_btn = 2)
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "version-test")
    )
  )

  versions <- pins::pin_versions(backend, "version-test")
  expect_equal(nrow(versions), 2)
})

test_that("a no-op save does not create a new version (#61)", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      first_save(session, "noop-test")
      Sys.sleep(1)
      session$setInputs(save_btn = 2)   # nothing changed since the first save
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "noop-test")
    )
  )

  versions <- pins::pin_versions(backend, "noop-test")
  expect_equal(nrow(versions), 1)
})

test_that("load_workflow navigates to the selected board", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  navigated <- NULL
  local_mocked_bindings(
    navigate_to_board = function(id, backend, session) navigated <<- id
  )

  test_board <- new_board(blocks = c(a = new_dataset_block("iris")))

  testServer(
    manage_project_server,
    session$setInputs(load_workflow = list(id = "load-test", user = "")),
    args = list(
      board = reactiveValues(board = test_board, board_id = "load-test")
    )
  )

  expect_s3_class(navigated, "rack_id")
  expect_identical(navigated$id, "load-test")
})

test_that("load_version navigates to the selected version", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  navigated <- NULL
  local_mocked_bindings(
    navigate_to_board = function(id, backend, session) navigated <<- id
  )

  test_board <- new_board(blocks = c(a = new_dataset_block("iris")))

  testServer(
    manage_project_server,
    session$setInputs(
      load_version = list(id = "ver-test", version = "20240101", user = "")
    ),
    args = list(
      board = reactiveValues(board = test_board, board_id = "ver-test")
    )
  )

  expect_s3_class(navigated, "rack_id")
  expect_identical(navigated$id, "ver-test")
  expect_identical(navigated$version, "20240101")
})

test_that("save_as forks the loaded board into a fresh record (#67)", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  navigated <- NULL
  local_mocked_bindings(
    navigate_to_board = function(id, backend, session) navigated <<- id
  )

  test_board <- new_board(blocks = c(a = new_dataset_block("iris")))

  testServer(
    manage_project_server,
    {
      first_save(session, "fork-origin")
      session$flushReact()

      prev_query("?id=fork-origin")
      session$setInputs(save_as_btn = 1)
      session$setInputs(rack_id_input = "fork-copy", rack_id_confirm = 2)
      session$flushReact()
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "fork-origin")
    )
  )

  forked <- setdiff(pins::pin_list(backend), "fork-origin")

  expect_length(forked, 1)
  expect_equal(nrow(pins::pin_versions(backend, "fork-origin")), 1)

  expect_s3_class(navigated, "rack_id")
  expect_identical(navigated$id, forked)
})

test_that("rack_loader returns a board_loader", {
  expect_true(is_board_loader(rack_loader()))
})

test_that("loader resolve serves the cleared default without a board ref", {
  initial <- new_board(blocks = c(a = new_dataset_block("iris")))

  res <- rack_loader()$resolve(list(QUERY_STRING = ""), NULL, initial)
  expect_s3_class(res, "board")
  expect_length(board_block_ids(res), 0)

  empty_id <- rack_loader()$resolve(
    list(QUERY_STRING = "id="), NULL, initial
  )
  expect_s3_class(empty_id, "board")
  expect_length(board_block_ids(empty_id), 0)
})

test_that("loader resolve loads a saved board from the backend", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(blocks = c(a = new_dataset_block("iris")))

  testServer(
    manage_project_server,
    first_save(session, "loader-test"),
    args = list(
      board = reactiveValues(board = test_board, board_id = "loader-test")
    )
  )

  loaded <- rack_loader()$resolve(
    list(QUERY_STRING = "id=loader-test"), NULL, new_board()
  )

  expect_s3_class(loaded, "board")
  expect_setequal(board_block_ids(loaded), "a")
})

test_that("loader resolve still reads a legacy board_name handle", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(blocks = c(a = new_dataset_block("iris")))

  testServer(
    manage_project_server,
    first_save(session, "legacy-test"),
    args = list(
      board = reactiveValues(board = test_board, board_id = "legacy-test")
    )
  )

  loaded <- rack_loader()$resolve(
    list(QUERY_STRING = "board_name=legacy-test"), NULL, new_board()
  )

  expect_s3_class(loaded, "board")
  expect_setequal(board_block_ids(loaded), "a")
})

test_that("loader resolve serves the cleared default for an unknown board", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  res <- rack_loader()$resolve(
    list(QUERY_STRING = "id=does-not-exist"), NULL, new_board()
  )
  expect_s3_class(res, "board")
  expect_length(board_block_ids(res), 0)
})

test_that("loader GET user-scopes via the configured user_pins_board (#70)", {
  visitor_backend <- pins::board_temp(versioned = TRUE)

  seed <- new_board(blocks = c(a = new_dataset_block("iris")))

  withr::with_options(
    list(blockr.session_mgmt_backend = visitor_backend),
    testServer(
      manage_project_server,
      first_save(session, "scoped"),
      args = list(
        board = reactiveValues(board = seed, board_id = "scoped")
      )
    )
  )

  withr::local_options(blockr.session_mgmt_backend = user_pins_board)
  local_mocked_bindings(connect_board = function(token) visitor_backend)

  loaded <- rack_loader()$resolve(
    list(
      QUERY_STRING = "board_name=scoped",
      HTTP_POSIT_CONNECT_USER_SESSION_TOKEN = "viewer-token"
    ),
    NULL,
    new_board()
  )

  expect_s3_class(loaded, "board")
  expect_setequal(board_block_ids(loaded), "a")
})

test_that("loader leaves a non-Connect backend untouched under a token (#70)", {
  shared_backend <- pins::board_temp(versioned = TRUE)

  seed <- new_board(blocks = c(a = new_dataset_block("iris")))

  withr::local_options(blockr.session_mgmt_backend = shared_backend)

  testServer(
    manage_project_server,
    first_save(session, "shared"),
    args = list(
      board = reactiveValues(board = seed, board_id = "shared")
    )
  )

  local_mocked_bindings(
    connect_board = function(token) stop("must not user-scope a plain backend")
  )

  loaded <- rack_loader()$resolve(
    list(
      QUERY_STRING = "board_name=shared",
      HTTP_POSIT_CONNECT_USER_SESSION_TOKEN = "viewer-token"
    ),
    NULL,
    new_board()
  )

  expect_s3_class(loaded, "board")
  expect_setequal(board_block_ids(loaded), "a")
})

test_that("loader WS user-scopes via the configured user_pins_board (#70)", {
  visitor_backend <- pins::board_temp(versioned = TRUE)

  seed <- new_board(blocks = c(a = new_dataset_block("iris")))

  withr::with_options(
    list(blockr.session_mgmt_backend = visitor_backend),
    testServer(
      manage_project_server,
      first_save(session, "ws-scoped"),
      args = list(
        board = reactiveValues(board = seed, board_id = "ws-scoped")
      )
    )
  )

  withr::local_options(blockr.session_mgmt_backend = user_pins_board)
  local_mocked_bindings(connect_board = function(token) visitor_backend)

  fake_session <- list(
    request = list(HTTP_POSIT_CONNECT_USER_SESSION_TOKEN = "viewer-token"),
    clientData = list(url_search = "?board_name=ws-scoped")
  )

  loaded <- rack_loader()$resolve(NULL, fake_session, new_board())

  expect_s3_class(loaded, "board")
  expect_setequal(board_block_ids(loaded), "a")
})

test_that("loader refuses a non-blockr pin instead of blanking (#90)", {

  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  upload_blockr_json(backend, list(x = 1), "not-blockr")

  refused <- NULL
  local_mocked_bindings(
    refuse_incompatible_load = function(cnd, session) {
      refused <<- conditionMessage(cnd)
      invisible()
    }
  )

  fake_session <- list(
    request = list(),
    clientData = list(url_search = "?id=not-blockr")
  )

  res <- rack_loader()$resolve(NULL, fake_session, new_board())

  expect_s3_class(res, "board")
  expect_length(board_block_ids(res), 0)
  expect_false(is.null(refused))
  expect_match(refused, "not compatible with blockr")
})

test_that("refuse_incompatible_load is a no-op without a session", {
  expect_silent(refuse_incompatible_load(simpleCondition("x"), NULL))
})

test_that("version history marks the URL version as current (#19)", {

  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(blocks = c(a = new_dataset_block("iris")))

  testServer(
    manage_project_server,
    {
      first_save(session, "hist-current")
      Sys.sleep(1)
      board$board <- new_board(
        blocks = c(a = new_dataset_block("iris"), b = new_subset_block())
      )
      session$setInputs(save_btn = 2)
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "hist-current")
    )
  )

  versions <- pins::pin_versions(backend, "hist-current")
  versions <- versions[order(versions$created, decreasing = TRUE), ]
  newer_version <- versions$version[1]
  older_version <- versions$version[2]

  testServer(
    manage_project_server,
    {
      prev_query(paste0("?id=hist-current&version=", older_version))

      html <- output$version_history

      expect_true(any(grepl(newer_version, html, fixed = TRUE)))
      expect_false(any(grepl(older_version, html, fixed = TRUE)))
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "hist-current")
    )
  )
})

test_that("delete_workflows removes pin from backend", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      first_save(session, "del-test")
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "del-test")
    )
  )

  expect_true("del-test" %in% pins::pin_list(backend))

  testServer(
    manage_project_server,
    {
      session$setInputs(
        delete_workflows = list(list(id = "del-test", user = ""))
      )
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "del-test")
    )
  )

  expect_false("del-test" %in% pins::pin_list(backend))
})

test_that("delete_versions removes specific version", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      first_save(session, "ver-del-test")
      Sys.sleep(1)
      board$board <- new_board(
        blocks = c(a = new_dataset_block("iris"), b = new_subset_block())
      )
      session$setInputs(save_btn = 2)

      versions <- pins::pin_versions(backend, "ver-del-test")
      expect_equal(nrow(versions), 2)

      versions <- versions[order(versions$created), ]
      older_version <- versions$version[1]

      session$setInputs(
        delete_versions = list(
          id = "ver-del-test", user = "", version = older_version
        )
      )
      session$flushReact()

      remaining <- pins::pin_versions(backend, "ver-del-test")
      expect_equal(nrow(remaining), 1)
      expect_false(older_version %in% remaining$version)
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "ver-del-test")
    )
  )
})

test_that("version_history output shows versions after save", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      first_save(session, "vh-test")
      Sys.sleep(1)
      session$setInputs(save_btn = 2)

      html <- output$version_history
      expect_true(any(grepl("Current", html)))
      expect_true(any(grepl("blockr-workflow-item", html)))
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "vh-test")
    )
  )
})

test_that("version_history output shows empty message for unsaved board", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      html <- output$version_history
      expect_true(
        any(grepl("No versions found", html)) ||
          any(grepl("Save workflow to see history", html))
      )
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "vh-empty-test")
    )
  )
})

test_that("expanding a picker row surfaces that workflow's version history", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  ser <- function(board) {
    shiny::isolate(serialize_board(board, blocks = list(), session = NULL))
  }

  one_block <- new_board(blocks = c(a = new_dataset_block("iris")))
  two_blocks <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = new_subset_block())
  )

  # a proper, listed workflow that is NOT the loaded board -- the #51 case
  rack_create(backend, ser(one_block), "other-wf", "Other workflow")
  Sys.sleep(1)
  rack_append(
    rack_id_from_input(backend, list(id = "other-wf")), backend,
    ser(two_blocks)
  )

  test_board <- new_board(blocks = c(a = new_dataset_block("iris")))

  testServer(
    manage_project_server,
    {
      first_save(session, "vvf-test")

      session$setInputs(modal_toggle_expand = list(id = "other-wf", user = ""))
      session$flushReact()

      expect_equal(modal_expanded(), "other-wf")
      expect_equal(nrow(expanded_versions()[["other-wf"]]), 2)

      # toggling the same row again collapses it
      session$setInputs(modal_toggle_expand = list(id = "other-wf", user = ""))
      session$flushReact()

      expect_length(modal_expanded(), 0)
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "vvf-test")
    )
  )
})

test_that("delete_versions removes a version from the named record", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  pins::pin_write(backend, list(x = 1), "other-wf", type = "json")
  Sys.sleep(1)
  pins::pin_write(backend, list(x = 2), "other-wf", type = "json")

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      first_save(session, "dvf-test")
      Sys.sleep(1)
      board$board <- new_board(
        blocks = c(a = new_dataset_block("iris"), b = new_subset_block())
      )
      session$setInputs(save_btn = 2)

      expect_equal(nrow(pins::pin_versions(backend, "dvf-test")), 2)

      # each inline history row carries its own id, so delete follows the
      # named record rather than the loaded board
      victim <- pins::pin_versions(backend, "other-wf")$version[1]
      session$setInputs(
        delete_versions = list(id = "other-wf", user = "", version = victim)
      )
      session$flushReact()

      remaining <- pins::pin_versions(backend, "other-wf")
      expect_equal(nrow(remaining), 1)
      expect_false(victim %in% remaining$version)

      expect_equal(nrow(pins::pin_versions(backend, "dvf-test")), 2)
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "dvf-test")
    )
  )
})

test_that("float_to_front moves the focused record to the front", {

  wfs <- list(list(id = "a"), list(id = "b"), list(id = "c"))

  expect_equal(chr_xtr(float_to_front(wfs, "c"), "id"), c("c", "a", "b"))
  expect_equal(chr_xtr(float_to_front(wfs, "a"), "id"), c("a", "b", "c"))

  # a focus id absent from the list, or none at all, leaves order untouched
  expect_equal(chr_xtr(float_to_front(wfs, "z"), "id"), c("a", "b", "c"))
  expect_equal(chr_xtr(float_to_front(wfs, NULL), "id"), c("a", "b", "c"))
})

test_that("no version is current when history is not the loaded board", {

  # loaded board, no ?version= in the URL: the newest row is the current one
  expect_true(version_is_current(1L, "v2", NULL, is_active = TRUE))
  expect_false(version_is_current(2L, "v1", NULL, is_active = TRUE))

  # loaded board pinned to an older version via ?version=
  expect_false(version_is_current(1L, "v2", "v1", is_active = TRUE))
  expect_true(version_is_current(2L, "v1", "v1", is_active = TRUE))

  # a workflow that is not the loaded board flags nothing, so every row --
  # including the newest, the corrupted one a user needs to load past or
  # remove -- keeps its Load and Delete actions
  expect_false(version_is_current(1L, "v2", NULL, is_active = FALSE))
  expect_false(version_is_current(1L, "v2", "v2", is_active = FALSE))
})

test_that("view_all_versions triggers modal", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      first_save(session, "vav-test")
      Sys.sleep(1)
      session$setInputs(save_btn = 2)
      session$setInputs(view_all_versions = 1)
      expect_no_error(session$flushReact())
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "vav-test")
    )
  )
})

test_that("sharing tab absent with pins backend", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      # With pins_board (sharing=FALSE), req() fails silently
      expect_error(output$sharing_tab, class = "shiny.silent.error")
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "no-sharing-test")
    )
  )
})

test_that("sharing tab rendered with sharing-capable backend", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  local_mocked_bindings(
    rack_capabilities = function(backend, ...) {
      list(
        versioning = TRUE, tags = TRUE, metadata = TRUE,
        sharing = TRUE, visibility = TRUE, user_discovery = TRUE
      )
    }
  )

  testServer(
    manage_project_server,
    {
      html <- output$sharing_tab
      expect_true(any(grepl("Sharing", html)))

      html <- output$sharing_panel
      expect_true(any(grepl("VISIBILITY", html)))

      # Sharing controls only render in "Restricted" (acl) mode
      session$setInputs(visibility_select = "acl")
      html <- output$sharing_controls
      expect_true(any(grepl("SHARED WITH", html)))
      expect_true(any(grepl("ADD PEOPLE", html)))
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "sharing-test")
    )
  )
})

test_that("sharing observers fire with sharing-capable backend", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  local_mocked_bindings(
    rack_capabilities = function(backend, ...) {
      list(
        versioning = TRUE, tags = TRUE, metadata = TRUE,
        sharing = TRUE, visibility = TRUE, user_discovery = TRUE
      )
    },
    rack_set_acl = function(id, backend, acl_type, ...) invisible(id),
    rack_share = function(id, backend, with_sub, ...) invisible(id),
    rack_unshare = function(id, backend, with_sub, ...) invisible(id),
    rack_shares = function(id, backend, ...) list(),
    rack_find_users = function(backend, query, ...) list()
  )

  testServer(
    manage_project_server,
    {
      first_save(session, "sharing-obs-test")

      session$setInputs(visibility_change = "all")
      expect_no_error(session$flushReact())

      session$setInputs(share_user = "user-guid-123")
      expect_no_error(session$flushReact())

      session$setInputs(unshare_user = "user-guid-123")
      expect_no_error(session$flushReact())
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "sharing-obs-test")
    )
  )
})

test_that("navbar shows a rack id area, not an editable title (#81)", {
  doc <- xml2::read_html(
    as.character(manage_project_ui("project", new_board()))
  )

  area <- xml2::xml_find_all(doc, "//*[@id='project-rack_id_area']")
  expect_length(area, 1)

  title_input <- xml2::xml_find_all(doc, "//input[@id='project-title_input']")
  expect_length(title_input, 0)
})

test_that("the rack id area shows only for a saved workflow (#81)", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(blocks = c(a = new_dataset_block("iris")))

  testServer(
    manage_project_server,
    {
      # a never-saved board has no chosen id, so the area renders nothing
      expect_error(output$rack_id_area, class = "shiny.silent.error")

      prev_query("?id=loaded-board")
      session$flushReact()
      expect_true(any(grepl("loaded-board", output$rack_id_area, fixed = TRUE)))
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "fresh-board")
    )
  )
})

test_that("first save mints the chosen id, not the board id (#81)", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(blocks = c(a = new_dataset_block("iris")))

  testServer(
    manage_project_server,
    {
      session$setInputs(save_btn = 1)
      session$setInputs(rack_id_input = "chosen-id", rack_id_confirm = 1)
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "auto-slug")
    )
  )

  expect_true("chosen-id" %in% pins::pin_list(backend))
  expect_false("auto-slug" %in% pins::pin_list(backend))
})

test_that("the id chooser rejects an invalid id (#81)", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(blocks = c(a = new_dataset_block("iris")))

  testServer(
    manage_project_server,
    {
      session$setInputs(save_btn = 1)
      session$setInputs(rack_id_input = "no spaces!", rack_id_confirm = 1)
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "invalid-test")
    )
  )

  expect_length(pins::pin_list(backend), 0L)
})

test_that("the id chooser refuses to overwrite an existing id (#81)", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  rack_create(backend, list(blocks = list()), id = "taken", name = "taken")

  test_board <- new_board(blocks = c(a = new_dataset_block("iris")))

  testServer(
    manage_project_server,
    {
      session$setInputs(save_btn = 1)
      session$setInputs(rack_id_input = "taken", rack_id_confirm = 1)
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "collision-test")
    )
  )

  expect_equal(pins::pin_list(backend), "taken")
  expect_equal(nrow(pins::pin_versions(backend, "taken")), 1L)
})
