test_that("board_query_string local board, no version", {
  id <- new_rack_id_pins("my_board")
  backend <- pins::board_temp()
  expect_equal(board_query_string(id, backend), "?board_name=my_board")
})

test_that("board_query_string local board with version", {
  id <- new_rack_id_pins("my_board", version = "v1")
  backend <- pins::board_temp()
  expect_equal(
    board_query_string(id, backend),
    "?board_name=my_board&version=v1"
  )
})

test_that("board_query_string omits user when same as backend", {
  id <- new_rack_id_pins_connect("alice", "my_board")
  backend <- mock_board_connect(account = "alice")
  expect_equal(board_query_string(id, backend), "?board_name=my_board")
})

test_that("board_query_string includes user when different from backend", {
  id <- new_rack_id_pins_connect("alice", "my_board")
  backend <- mock_board_connect(account = "bob")
  expect_equal(
    board_query_string(id, backend),
    "?board_name=my_board&user=alice"
  )
})

test_that("board_query_string different user with version", {
  id <- new_rack_id_pins_connect("alice", "my_board", version = "v1")
  backend <- mock_board_connect(account = "bob")
  expect_equal(
    board_query_string(id, backend),
    "?board_name=my_board&user=alice&version=v1"
  )
})

test_that("board_query_string same user with version omits user", {
  id <- new_rack_id_pins_connect("alice", "my_board", version = "v1")
  backend <- mock_board_connect(account = "alice")
  expect_equal(
    board_query_string(id, backend),
    "?board_name=my_board&version=v1"
  )
})
