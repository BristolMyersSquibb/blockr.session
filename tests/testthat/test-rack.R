# new_rack_id --------------------------------------------------------------

test_that("new_rack_id rejects empty string", {
  expect_error(
    new_rack_id(""),
    class = "rack_id_invalid_id"
  )
})

test_that("new_rack_id rejects non-string", {
  expect_error(
    new_rack_id(42),
    class = "rack_id_invalid_id"
  )
})

test_that("new_rack_id stores id, version and user", {
  id <- new_rack_id("board", version = "v1", user = "alice")
  expect_equal(id$id, "board")
  expect_equal(id$version, "v1")
  expect_equal(id$user, "alice")
})

test_that("format.rack_id", {
  id <- new_rack_id("my_board")
  expect_equal(format(id), "<rack_id: my_board>")
})

test_that("print.rack_id", {
  id <- new_rack_id("my_board")
  expect_output(print(id), "<rack_id: my_board>")
})

# new_rack_record ----------------------------------------------------------

test_that("new_rack_record stores id, name and extra fields", {
  rec <- new_rack_record("egoistic_lowchen", "My Board")
  expect_s3_class(rec, "rack_record")
  expect_equal(rec$id, "egoistic_lowchen")
  expect_equal(rec$name, "My Board")

  with_user <- new_rack_record("slug", "Name", user = "alice")
  expect_equal(with_user$user, "alice")
})

test_that("new_rack_record rejects empty id", {
  expect_error(
    new_rack_record("", "name"),
    class = "rack_record_invalid_id"
  )
})

test_that("new_rack_record rejects non-string name", {
  expect_error(
    new_rack_record("id", 42),
    class = "rack_record_invalid_name"
  )
})

test_that("format.rack_record", {
  rec <- new_rack_record("slug", "Display Name")
  expect_equal(format(rec), "<rack_record: slug (Display Name)>")
})

test_that("print.rack_record", {
  rec <- new_rack_record("slug", "Display Name")
  expect_output(print(rec), "rack_record: slug")
})

# as_rack_id ----------------------------------------------------------------

test_that("as_rack_id returns an existing rack_id unchanged", {
  rid <- new_rack_id("board", version = "v1")
  expect_identical(as_rack_id(rid), rid)
})

test_that("as_rack_id coerces a rack_record listing row via the backend", {

  rid <- as_rack_id(new_rack_record("board", "Board"), pins::board_temp())

  expect_s3_class(rid, "rack_id")
  expect_equal(rid$id, "board")
})
