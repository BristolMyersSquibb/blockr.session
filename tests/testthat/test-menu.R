test_that("workflow menu windows results and filters server-side", {

  withr::local_options(
    blockr.session_mgmt_backend = pins::board_temp(versioned = TRUE)
  )

  records <- lapply(
    seq_len(25L),
    function(i) {
      new_rack_record(
        id = sprintf("wf-%02d", i),
        name = if (i <= 5L) sprintf("alpha-%d", i) else sprintf("beta-%d", i),
        user = "tester",
        saved = Sys.time()
      )
    }
  )

  n_items <- function(out) {
    html <- paste(as.character(out), collapse = "")
    hits <- gregexpr("class=\"blockr-workflow-item\"", html, fixed = TRUE)[[1L]]
    if (length(hits) == 1L && hits == -1L) 0L else length(hits)
  }

  local_mocked_bindings(
    rack_list = function(backend, ...) records,
    board_query_string = function(x, backend, ...) "?board=test",
    .package = "blockr.session"
  )

  testServer(
    manage_project_server,
    {
      session$flushReact()

      expect_length(all_workflows(), 25L)
      expect_identical(n_shown(), 10L)
      expect_identical(n_items(output$recent_workflows), 10L)
      expect_match(
        paste(as.character(output$recent_workflows), collapse = ""),
        "blockr-workflow-sentinel"
      )

      session$setInputs(workflow_load_more = 1)
      session$flushReact()

      expect_identical(n_shown(), 20L)
      expect_identical(n_items(output$recent_workflows), 20L)

      session$setInputs(workflow_filter = "alpha")
      session$elapse(300)
      session$flushReact()

      expect_length(filtered_workflows(), 5L)
      expect_identical(n_shown(), 10L)
      expect_identical(n_items(output$recent_workflows), 5L)
      expect_identical(output$workflow_count, "5 / 25")
      expect_no_match(
        paste(as.character(output$recent_workflows), collapse = ""),
        "blockr-workflow-sentinel"
      )

      session$setInputs(workflow_filter = "zzz")
      session$elapse(300)
      session$flushReact()

      expect_length(filtered_workflows(), 0L)
      expect_identical(n_items(output$recent_workflows), 0L)
      expect_match(
        paste(as.character(output$recent_workflows), collapse = ""),
        "blockr-workflow-noresults"
      )
    },
    args = list(
      board = reactiveValues(board = new_board(), board_id = "menu-test")
    )
  )
})

test_that("manage-workflows modal windows results and filters server-side", {

  withr::local_options(
    blockr.session_mgmt_backend = pins::board_temp(versioned = TRUE)
  )

  records <- lapply(
    seq_len(25L),
    function(i) {
      new_rack_record(
        id = sprintf("wf-%02d", i),
        name = if (i <= 5L) sprintf("alpha-%d", i) else sprintf("beta-%d", i),
        user = "tester",
        saved = Sys.time()
      )
    }
  )

  n_rows <- function(out) {
    html <- paste(as.character(out), collapse = "")
    hits <- gregexpr("class=\"blockr-workflow-row\"", html, fixed = TRUE)[[1L]]
    if (length(hits) == 1L && hits == -1L) 0L else length(hits)
  }

  local_mocked_bindings(
    rack_list = function(backend, ...) records,
    board_query_string = function(x, backend, ...) "?board=test",
    .package = "blockr.session"
  )

  testServer(
    manage_project_server,
    {
      session$flushReact()

      expect_length(modal_filtered(), 25L)
      expect_identical(modal_n_shown(), 10L)
      expect_identical(n_rows(output$workflows_modal_rows), 10L)
      expect_match(
        paste(as.character(output$workflows_modal_rows), collapse = ""),
        "blockr-wf-modal-sentinel"
      )

      session$setInputs(modal_load_more = 1)
      session$flushReact()

      expect_identical(modal_n_shown(), 20L)
      expect_identical(n_rows(output$workflows_modal_rows), 20L)

      session$setInputs(modal_workflow_filter = "alpha")
      session$elapse(300)
      session$flushReact()

      expect_length(modal_filtered(), 5L)
      expect_identical(modal_n_shown(), 10L)
      expect_identical(n_rows(output$workflows_modal_rows), 5L)
      expect_identical(output$modal_workflow_count, "5 / 25")
      expect_no_match(
        paste(as.character(output$workflows_modal_rows), collapse = ""),
        "blockr-wf-modal-sentinel"
      )

      session$setInputs(modal_workflow_filter = "zzz")
      session$elapse(300)
      session$flushReact()

      expect_length(modal_filtered(), 0L)
      expect_match(
        paste(as.character(output$workflows_modal_rows), collapse = ""),
        "No workflows match your search"
      )
    },
    args = list(
      board = reactiveValues(board = new_board(), board_id = "modal-test")
    )
  )
})

test_that("modal select-all and delete act over the whole filtered set (#92)", {

  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  for (i in seq_len(12L)) {
    rack_create(
      backend, list(blocks = list()),
      id = sprintf("wf-%02d", i), name = sprintf("wf-%02d", i)
    )
  }

  testServer(
    manage_project_server,
    {
      session$flushReact()

      expect_length(modal_filtered(), 12L)
      expect_identical(modal_n_shown(), 10L)

      session$setInputs(modal_select_all = list(checked = TRUE, nonce = 1))
      session$flushReact()

      # every filtered id is selected, including the two beyond the window
      expect_length(modal_selection(), 12L)

      session$setInputs(modal_delete = 1)
      session$flushReact()

      expect_length(pins::pin_list(backend), 0L)
    },
    args = list(
      board = reactiveValues(board = new_board(), board_id = "del-all")
    )
  )
})

test_that("modal delete removes only the filtered selection (#92)", {

  backend <- pins::board_temp(versioned = TRUE)
  withr::local_options(blockr.session_mgmt_backend = backend)

  for (i in seq_len(6L)) {
    nm <- if (i <= 3L) sprintf("alpha-%d", i) else sprintf("beta-%d", i)
    rack_create(backend, list(blocks = list()), id = nm, name = nm)
  }

  testServer(
    manage_project_server,
    {
      session$setInputs(modal_workflow_filter = "alpha")
      session$elapse(300)
      session$flushReact()

      expect_length(modal_filtered(), 3L)

      session$setInputs(modal_select_all = list(checked = TRUE, nonce = 1))
      session$flushReact()

      expect_length(modal_selection(), 3L)

      session$setInputs(modal_delete = 1)
      session$flushReact()

      remaining <- pins::pin_list(backend)
      expect_length(remaining, 3L)
      expect_true(all(grepl("^beta", remaining)))
    },
    args = list(
      board = reactiveValues(board = new_board(), board_id = "del-flt")
    )
  )
})

test_that("modal_toggle updates the server-side selection", {

  withr::local_options(
    blockr.session_mgmt_backend = pins::board_temp(versioned = TRUE)
  )

  records <- lapply(
    seq_len(3L),
    function(i) {
      new_rack_record(
        id = sprintf("wf-%d", i), name = sprintf("wf-%d", i),
        user = "tester", saved = Sys.time()
      )
    }
  )

  local_mocked_bindings(
    rack_list = function(backend, ...) records,
    .package = "blockr.session"
  )

  testServer(
    manage_project_server,
    {
      session$flushReact()

      session$setInputs(
        modal_toggle = list(id = "wf-2", checked = TRUE, nonce = 1)
      )
      session$flushReact()
      expect_identical(modal_selection(), "wf-2")

      session$setInputs(
        modal_toggle = list(id = "wf-1", checked = TRUE, nonce = 2)
      )
      session$flushReact()
      expect_setequal(modal_selection(), c("wf-1", "wf-2"))

      session$setInputs(
        modal_toggle = list(id = "wf-2", checked = FALSE, nonce = 3)
      )
      session$flushReact()
      expect_identical(modal_selection(), "wf-1")

      # switching the filter clears the selection so we never delete unseen rows
      session$setInputs(modal_workflow_filter = "wf-3")
      session$elapse(300)
      session$flushReact()
      expect_length(modal_selection(), 0L)
    },
    args = list(
      board = reactiveValues(board = new_board(), board_id = "toggle-test")
    )
  )
})
