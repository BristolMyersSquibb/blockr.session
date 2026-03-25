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

test_that("load_workflow triggers restore", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      session$setInputs(save_btn = 1)
      session$setInputs(load_workflow = list(name = "load-test", user = ""))

      res <- session$returned()
      expect_true(!is.null(res))
      expect_true(is_board(res$board))
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "load-test")
    )
  )
})

test_that("load_version restores specific version", {
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
      board = reactiveValues(board = test_board, board_id = "ver-load-test")
    )
  )

  versions <- pins::pin_versions(backend, "ver-load-test")
  versions <- versions[order(versions$created), ]
  older_version <- versions$version[1]

  testServer(
    manage_project_server,
    {
      session$setInputs(
        load_version = list(name = "ver-load-test", version = older_version)
      )

      res <- session$returned()
      expect_true(!is.null(res))
      expect_true(is_board(res$board))
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "ver-load-test")
    )
  )
})

test_that("version history marks loaded version as current (#19)", {

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
      session$setInputs(
        load_version = list(
          name = "hist-current",
          version = older_version,
          user = ""
        )
      )

      html <- output$version_history

      expect_true(any(grepl(newer_version, html, fixed = TRUE)))
      expect_false(any(grepl(older_version, html, fixed = TRUE)))
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "hist-current")
    )
  )
})

test_that("safe_restore_board returns TRUE on success", {
  local_mocked_bindings(
    restore_board = function(...) invisible(NULL)
  )
  expect_true(
    safe_restore_board(
      "board", "ser", "result", session = MockShinySession$new()
    )
  )
})

test_that("safe_restore_board returns FALSE and notifies on error", {
  local_mocked_bindings(
    restore_board = function(...) stop("boom")
  )
  session <- MockShinySession$new()
  expect_false(
    safe_restore_board("board", "ser", "result", session = session)
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
        delete_workflows = list(list(name = "del-test", user = ""))
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
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "ver-del-test")
    )
  )

  versions <- pins::pin_versions(backend, "ver-del-test")
  expect_equal(nrow(versions), 2)

  versions <- versions[order(versions$created), ]
  older_version <- versions$version[1]

  testServer(
    manage_project_server,
    {
      session$setInputs(delete_versions = list(older_version))
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "ver-del-test")
    )
  )

  remaining <- pins::pin_versions(backend, "ver-del-test")
  expect_equal(nrow(remaining), 1)
  expect_false(older_version %in% remaining$version)
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

test_that("new_btn sets restore_result to cleared board", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    manage_project_server,
    {
      session$setInputs(new_btn = 1)

      res <- session$returned()
      expect_true(!is.null(res))
      expect_s3_class(res, "board")
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "new-test")
    )
  )
})

test_that("new_btn resets board_name to match new board ID", {
  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris")),
    options = c(
      new_board_name_option("Old name"),
      default_board_options()[-1]
    )
  )

  testServer(
    manage_project_server,
    {
      session$setInputs(new_btn = 1)

      res <- session$returned()
      new_id <- attr(res, "id")
      board_name_opt <- res[["options"]][[1]]
      expect_equal(
        board_option_default(board_name_opt),
        id_to_sentence_case(new_id)
      )
    },
    args = list(
      board = reactiveValues(board = test_board, board_id = "old-test")
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
