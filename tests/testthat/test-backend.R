test_that("user_pins_board uses a local board with no Connect creds", {

  withr::local_envvar(CONNECT_SERVER = "", CONNECT_API_KEY = "")

  board <- user_pins_board(NULL)

  expect_s3_class(board, "pins_board")
  expect_false(inherits(board, "pins_board_connect"))
})

test_that("user_pins_board scopes the board to the viewer's account", {

  fake_session <- list(
    request = list(HTTP_POSIT_CONNECT_USER_SESSION_TOKEN = "viewer-token")
  )

  local_mocked_bindings(
    connect_board = function(token) mock_board_connect(account = "viewer")
  )

  board <- user_pins_board(fake_session)

  expect_s3_class(board, "pins_board_connect")
  expect_equal(board$account, "viewer")
})

test_that("user_pins_board uses the app board with app creds, no token", {

  withr::local_envvar(
    CONNECT_SERVER = "https://connect.example.com",
    CONNECT_API_KEY = "app-key"
  )

  local_mocked_bindings(
    app_pins_board = function() mock_board_connect(account = "publisher")
  )

  board <- user_pins_board(NULL)

  expect_s3_class(board, "pins_board_connect")
  expect_equal(board$account, "publisher")
})

test_that("connect_board errors when connectapi is unavailable", {

  skip_if(
    requireNamespace("connectapi", quietly = TRUE),
    "connectapi is installed"
  )

  expect_error(
    connect_board("viewer-token"),
    class = "connectapi_not_installed"
  )
})
