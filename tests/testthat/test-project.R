test_that("manage_project plugin", {

  a <- manage_project()

  expect_s3_class(a, "plugin")
  expect_s3_class(a, "preserve_board")
  expect_true(is_plugin(a))

  expect_snapshot(print(a))
})

test_that("manage_project server", {

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
      session$setInputs(save_btn = 1)
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

test_that("title edit forwards a board to set_board_option_value (#60)", {

  captured <- new.env()

  testServer(
    manage_project_server,
    {
      local_mocked_bindings(
        set_board_option_value = function(opt, val, board, ...) {
          captured$board <- board
          invisible(val)
        },
        .package = "blockr.session"
      )

      session$setInputs(title_edit = "Renamed board")
      session$flushReact()

      expect_true(is_board(captured$board))
    },
    args = list(
      board = reactiveValues(board = new_board(), board_id = "title-test")
    )
  )
})

test_that("manage_project ui", {
  expect_s3_class(manage_project_ui("project", new_board()), "shiny.tag.list")
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
      session$setInputs(save_btn = 1)
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
      session$setInputs(save_btn = 1)
      html <- as.character(output$recent_workflows)

      expect_true(any(grepl("rebel_eyas", html, fixed = TRUE)))
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "rebel_eyas")
    )
  )
})

test_that("editing the title renames the loaded record (#61)", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  # an existing record we're editing; stored name differs from the edit
  rack_create(backend, list(blocks = list()), id = "rename-edit",
              name = "Old name")

  renamed_to <- NULL
  local_mocked_bindings(
    set_board_option_value = function(opt, val, board, ...) invisible(val),
    rack_rename = function(id, backend, name, ...) {
      renamed_to <<- name
      id
    },
    .package = "blockr.session"
  )

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      prev_query("?id=rename-edit")     # a loaded record (current_id set)
      session$setInputs(title_edit = "New name")
      session$flushReact()

      expect_equal(renamed_to, "New name")
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "rename-edit")
    )
  )
})

test_that("editing the title of an unsaved board does not rename (#61)", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  renamed <- FALSE
  local_mocked_bindings(
    set_board_option_value = function(opt, val, board, ...) invisible(val),
    rack_rename = function(id, backend, name, ...) {
      renamed <<- TRUE
      id
    },
    .package = "blockr.session"
  )

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      # no current_id (unsaved board): the name rides along until first save
      session$setInputs(title_edit = "Fresh name")
      session$flushReact()

      expect_false(renamed)
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "unsaved-board")
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

test_that("saving multiple times creates versions", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      session$setInputs(save_btn = 1)
      Sys.sleep(1)
      session$setInputs(save_btn = 2)
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "version-test")
    )
  )

  versions <- pins::pin_versions(backend, "version-test")
  expect_equal(nrow(versions), 2)
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
    session$setInputs(save_btn = 1),
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
    session$setInputs(save_btn = 1),
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

test_that("version history marks the URL version as current (#19)", {

  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(blocks = c(a = new_dataset_block("iris")))

  testServer(
    manage_project_server,
    {
      session$setInputs(save_btn = 1)
      Sys.sleep(1)
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
      session$setInputs(save_btn = 1)
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
      session$setInputs(save_btn = 1)
      Sys.sleep(1)
      session$setInputs(save_btn = 2)

      versions <- pins::pin_versions(backend, "ver-del-test")
      expect_equal(nrow(versions), 2)

      versions <- versions[order(versions$created), ]
      older_version <- versions$version[1]

      session$setInputs(delete_versions = list(older_version))

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
      session$setInputs(save_btn = 1)
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

test_that("view_all_versions triggers modal", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      session$setInputs(save_btn = 1)
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
      session$setInputs(save_btn = 1)

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
