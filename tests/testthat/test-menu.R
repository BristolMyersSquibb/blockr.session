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
