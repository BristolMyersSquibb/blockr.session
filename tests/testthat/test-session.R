test_that("manage_session plugin", {

  a <- manage_session()

  expect_s3_class(a, "plugin")
  expect_s3_class(a, "preserve_board")
  expect_true(is_plugin(a))

  expect_output(print(a), "preserve_board")
})

test_that("manage_session server", {

  skip_if_not_installed("pins")

  test_board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    links = links(from = "a", to = "b")
  )

  testServer(
    manage_session_server,
    {
      session$setInputs(save = 1)
      expect_no_error(session$flushReact())
    },
    args = list(board = reactiveValues(board = test_board, board_id = "test"))
  )

  testServer(
    manage_session_server,
    {
      session$setInputs(browse = 1)
      expect_no_error(session$flushReact())

      session$setInputs(cancel = 1)
      expect_no_error(session$flushReact())
    },
    args = list(board = reactiveValues(board = test_board, board_id = "test"))
  )
})

test_that("manage_session ui", {
  expect_s3_class(manage_session_ui("session", new_board()), "shiny.tag.list")
})