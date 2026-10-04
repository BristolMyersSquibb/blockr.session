test_that("the name menu's recent list hears about each load", {

  withr::local_options(
    blockr.session_mgmt_backend = pins::board_temp(versioned = TRUE)
  )

  added <- list()
  dropped <- list()
  found <- TRUE

  local_mocked_bindings(
    rack_name = function(id, backend, ...) "Alpha",
    rack_info = function(id, backend, ...) {
      if (found) data.frame(created = Sys.time()) else NULL
    },
    recent_add = function(id, name, backend, session) {
      added[[length(added) + 1L]] <<- list(id = id$id, name = name)
    },
    recent_drop_missing = function(id, backend, session) {
      dropped[[length(dropped) + 1L]] <<- id$id
    },
    .package = "blockr.session"
  )

  testServer(
    manage_project_server,
    {
      session$flushReact()

      # an untitled board is not a workflow yet
      expect_length(added, 0L)
      expect_length(dropped, 0L)

      prev_query("?id=wf-01")
      session$flushReact()

      expect_identical(added, list(list(id = "wf-01", name = "Alpha")))

      found <<- FALSE
      prev_query("?id=gone")
      session$flushReact()

      expect_length(added, 1L)
      expect_identical(dropped, list("gone"))
    },
    args = list(
      board = reactiveValues(board = new_board(), board_id = "menu-test")
    )
  )
})

test_that("recent entries open the latest save, and only gone ones drop", {

  backend <- pins::board_temp(versioned = TRUE)

  sent <- list()
  session <- list(
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    }
  )

  local_mocked_bindings(
    notify = function(...) invisible(NULL),
    .package = "blockr.session"
  )

  id <- as_rack_id(list(id = "wf-01", version = "v2"), backend)
  recent_add(id, "Alpha", backend, session)

  expect_identical(sent[[1L]]$type, "blockr-recent-add")
  expect_identical(sent[[1L]]$message$name, "Alpha")
  expect_identical(sent[[1L]]$message$href, "?id=wf-01")

  local_mocked_bindings(
    rack_exists = function(id, backend, ...) stop("backend down"),
    .package = "blockr.session"
  )
  recent_drop_missing(id, backend, session)
  expect_length(sent, 1L)

  local_mocked_bindings(
    rack_exists = function(id, backend, ...) FALSE,
    .package = "blockr.session"
  )
  recent_drop_missing(id, backend, session)
  expect_identical(sent[[2L]]$type, "blockr-recent-drop")
  expect_identical(sent[[2L]]$message, list(id = "wf-01", user = ""))
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

test_that("download outputs are registered even while hidden (#119)", {

  ast_calls <- function(x) {

    if (!is.call(x)) {
      return(list())
    }

    c(list(x), unlst(lapply(as.list(x), ast_calls)))
  }

  call_head <- function(x) {

    if (is.call(x[[1L]])) {
      return(x[[1L]][[3L]])
    }

    x[[1L]]
  }

  is_handler <- function(x) {
    identical(call_head(x), quote(`<-`)) &&
      length(x[[2L]]) == 3L && identical(x[[2L]][[2L]], quote(output)) &&
      is.call(x[[3L]]) && identical(call_head(x[[3L]]), quote(downloadHandler))
  }

  is_unsuspended <- function(x) {
    identical(call_head(x), quote(outputOptions)) &&
      isFALSE(as.list(x)$suspendWhenHidden)
  }

  handler_name <- function(x) as.character(x[[2L]][[3L]])
  option_name <- function(x) as.character(as.list(x)[[3L]])

  fns <- Filter(is.function, eapply(asNamespace("blockr.session"), identity))
  calls <- unlst(lapply(lapply(fns, body), ast_calls))

  handlers <- chr_ply(Filter(is_handler, calls), handler_name)
  unsuspended <- chr_ply(Filter(is_unsuspended, calls), option_name)

  expect_gt(length(handlers), 0L)
  expect_identical(sort(setdiff(handlers, unsuspended)), character())
})

test_that("icon-only controls in the workflow lists are named (#122)", {

  backend <- pins::board_temp(versioned = TRUE)
  ns <- NS("project")

  wf <- new_rack_record(
    id = "wf-01",
    name = "alpha",
    user = "tester",
    saved = Sys.time()
  )

  versions <- data.frame(
    version = c("v2", "v1"),
    created = Sys.time() - c(60, 120),
    ref = c("bbb", "aaa")
  )

  doc <- xml2::read_html(
    as.character(
      tagList(
        recent_row_template(),
        tags$table(
          workflow_modal_row(wf, character(), backend, ns),
          version_subrows(wf, versions, TRUE, NULL, backend, ns)
        )
      )
    )
  )

  icon_only <- xml2::xml_find_all(
    doc,
    "//*[self::a or self::button][not(normalize-space())]"
  )

  expect_length(xml2::xml_find_all(doc, "//*[@title]"), 0L)
  expect_setequal(
    xml2::xml_attr(icon_only, "data-blockr-tooltip"),
    c("Open in new tab", "Version history", "Download", "Delete")
  )
  expect_identical(
    xml2::xml_attr(icon_only, "aria-label"),
    xml2::xml_attr(icon_only, "data-blockr-tooltip")
  )
})

test_that("suggest_copy_id suffixes -copy when no copy exists (#99)", {

  backend <- pins::board_temp(versioned = TRUE)
  rack_create(backend, list(blocks = list()), id = "sales", name = "sales")

  expect_identical(suggest_copy_id("sales", backend), "sales-copy")
})

test_that("suggest_copy_id increments past existing copies (#99)", {

  backend <- pins::board_temp(versioned = TRUE)

  for (nm in c("sales", "sales-copy", "sales-copy-2")) {
    rack_create(backend, list(blocks = list()), id = nm, name = nm)
  }

  expect_identical(suggest_copy_id("sales", backend), "sales-copy-3")
})

test_that("suggest_copy_id falls back to the plain suffix at max_tries (#99)", {

  backend <- pins::board_temp(versioned = TRUE)
  rack_create(backend, list(blocks = list()), id = "x-copy", name = "x-copy")

  expect_identical(suggest_copy_id("x", backend, max_tries = 1L), "x-copy")
})

test_that("save-as prefills the current id plus -copy as the default (#99)", {

  withr::local_options(
    blockr.session_mgmt_backend = pins::board_temp(versioned = TRUE)
  )

  captured <- NULL
  local_mocked_bindings(
    show_rack_id_modal = function(session, default) captured <<- default
  )

  testServer(
    manage_project_server,
    {
      session$setInputs(save_as_btn = 1)
      session$flushReact()

      expect_identical(captured, "sales-copy")
    },
    args = list(
      board = reactiveValues(board = new_board(), board_id = "sales")
    )
  )
})
