test_that("board_query_string local board, no version", {
  id <- new_rack_id_pins("my_board")
  backend <- pins::board_temp()
  expect_equal(board_query_string(id, backend), "?id=my_board")
})

test_that("board_query_string local board with version", {
  id <- new_rack_id_pins("my_board", version = "v1")
  backend <- pins::board_temp()
  expect_equal(
    board_query_string(id, backend),
    "?id=my_board&version=v1"
  )
})

test_that("board_query_string omits user when same as backend", {
  id <- new_rack_id_pins_connect("alice", "my_board")
  backend <- mock_board_connect(account = "alice")
  expect_equal(board_query_string(id, backend), "?id=my_board")
})

test_that("board_query_string includes user when different from backend", {
  id <- new_rack_id_pins_connect("alice", "my_board")
  backend <- mock_board_connect(account = "bob")
  expect_equal(
    board_query_string(id, backend),
    "?id=my_board&user=alice"
  )
})

test_that("board_query_string different user with version", {
  id <- new_rack_id_pins_connect("alice", "my_board", version = "v1")
  backend <- mock_board_connect(account = "bob")
  expect_equal(
    board_query_string(id, backend),
    "?id=my_board&user=alice&version=v1"
  )
})

test_that("board_query_string same user with version omits user", {
  id <- new_rack_id_pins_connect("alice", "my_board", version = "v1")
  backend <- mock_board_connect(account = "alice")
  expect_equal(
    board_query_string(id, backend),
    "?id=my_board&version=v1"
  )
})

test_that("board_query_string accepts a rack_record", {
  rec <- new_rack_record("slug", "Display Name")
  backend <- pins::board_temp()
  expect_equal(board_query_string(rec, backend), "?id=slug")
})

test_that("board_query_string keep preserves non-session params", {
  id <- new_rack_id_pins("my_board")
  backend <- pins::board_temp()
  expect_equal(
    board_query_string(id, backend, keep = "?new=xyz&theme=dark&tab=2"),
    "?id=my_board&theme=dark&tab=2"
  )
})

test_that("drop_session_query keeps only non-session query params", {
  keep <- drop_session_query(
    "?id=b&board_name=n&user=u&version=v&new=x&theme=dark&tab=2"
  )
  expect_named(keep, c("theme", "tab"))
  expect_equal(keep[["theme"]], "dark")
})

test_that("build_query_string url-encodes reserved characters in values", {
  expect_equal(
    build_query_string(list(id = "b", q = "a b")),
    "?id=b&q=a%20b"
  )
})
