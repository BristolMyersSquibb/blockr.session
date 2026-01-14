test_that("manage_project plugin", {

  a <- manage_project()

  expect_s3_class(a, "plugin")
  expect_s3_class(a, "preserve_board")
  expect_true(is_plugin(a))

  expect_output(print(a), "preserve_board")
})

test_that("manage_project server", {

  skip_if_not_installed("pins")

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