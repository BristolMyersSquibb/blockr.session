test_that("new_rack_id rejects empty string", {
  expect_error(
    new_rack_id(""),
    class = "rack_id_invalid_name"
  )
})

test_that("new_rack_id rejects non-string", {
  expect_error(
    new_rack_id(42),
    class = "rack_id_invalid_name"
  )
})

test_that("format.rack_id", {
  id <- new_rack_id("my_board")
  expect_equal(format(id), "<rack_id: my_board>")
})

test_that("print.rack_id", {
  id <- new_rack_id("my_board")
  expect_output(print(id), "<rack_id: my_board>")
})

test_that("display_name.rack_id returns name", {
  id <- new_rack_id("my_board")
  expect_equal(display_name(id), "my_board")
})
