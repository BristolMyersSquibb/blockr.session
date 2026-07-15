# session_connect_tag resolution --------------------------------------------

test_that("connect_tag_id returns NULL when the option is unset", {

  board <- mock_board_connect()

  expect_null(connect_tag_id(board))
})

test_that("connect_tag_id resolves a bare tag name to its id", {

  board <- mock_board_connect()
  withr::local_options(blockr.session_connect_tag = "blockr")

  local_mocked_bindings(
    connect_api = function(board, route, ...) {
      list(
        list(id = 3, name = "other", parent_id = NULL),
        list(id = 7, name = "blockr", parent_id = NULL)
      )
    }
  )

  expect_equal(connect_tag_id(board), 7)
})

test_that("connect_tag_id resolves a Category/Name path past a root decoy", {

  board <- mock_board_connect()
  withr::local_options(blockr.session_connect_tag = "Apps/blockr")

  local_mocked_bindings(
    connect_api = function(board, route, ...) {
      list(
        list(id = 1, name = "Apps", parent_id = NULL),
        list(id = 2, name = "blockr", parent_id = 1),
        list(id = 3, name = "blockr", parent_id = NULL)
      )
    }
  )

  expect_equal(connect_tag_id(board), 2)
})

test_that("connect_tag_id returns NULL for an unknown tag", {

  board <- mock_board_connect()
  withr::local_options(blockr.session_connect_tag = "nope")

  local_mocked_bindings(
    connect_api = function(board, route, ...) {
      list(list(id = 1, name = "blockr", parent_id = NULL))
    }
  )

  expect_null(connect_tag_id(board))
})

test_that("connect_tag_id warns and picks one when a bare name is ambiguous", {

  board <- mock_board_connect()
  withr::local_options(blockr.session_connect_tag = "blockr")

  local_mocked_bindings(
    connect_api = function(board, route, ...) {
      list(
        list(id = 1, name = "blockr", parent_id = 10),
        list(id = 2, name = "blockr", parent_id = 20)
      )
    }
  )

  expect_warning(
    id <- connect_tag_id(board),
    class = "connect_tag_ambiguous"
  )
  expect_equal(id, 1)
})

# Configured listing --------------------------------------------------------

test_that("rack_list filters server-side by tag when configured", {

  board <- mock_board_connect()
  connect_owner_cache$reset()
  withr::defer(connect_owner_cache$reset())
  withr::local_options(blockr.session_connect_tag = "blockr")

  routes <- character()

  local_mocked_bindings(
    connect_api = function(board, route, ...) {
      routes[[length(routes) + 1L]] <<- route
      if (identical(route, "GET /tags")) {
        return(list(list(id = 5, name = "blockr", parent_id = NULL)))
      }
      if (grepl("GET /tags/", route, fixed = TRUE)) {
        return(
          list(
            list(name = "wf-a", title = "WF A", content_category = "pin",
                 owner_guid = "g", last_deployed_time = "2020-01-01T00:00:00Z")
          )
        )
      }
      if (grepl("GET /users", route, fixed = TRUE)) {
        return(list(username = "user_a"))
      }
      list()
    }
  )
  local_mocked_bindings(
    pin_meta = function(...) stop("must not probe a pin"),
    pin_search = function(...) stop("must not pin_search"),
    .package = "pins"
  )

  result <- rack_list(board)

  expect_length(result, 1L)
  expect_equal(result[[1L]]$id, "wf-a")
  expect_equal(result[[1L]]$user, "user_a")
  expect_true(any(grepl("GET /tags/", routes, fixed = TRUE)))
})

# Tag on upload -------------------------------------------------------------

single_version <- function() {
  data.frame(
    version = "20200101T000000Z-aaaaa",
    created = as.POSIXct("2020-01-01", tz = "UTC"),
    hash = "abc123",
    stringsAsFactors = FALSE
  )
}

test_that("rack_upload applies the configured tag to the content", {

  board <- mock_board_connect(account = "user_a")
  withr::local_options(blockr.session_connect_tag = "blockr")

  posted <- NULL

  local_mocked_bindings(
    pin_upload = function(...) invisible(),
    pin_versions = function(...) single_version(),
    pin_exists = function(...) FALSE,
    .package = "pins"
  )
  local_mocked_bindings(
    connect_content_find = function(board, name) list(guid = "guid-xyz"),
    connect_api = function(board, route, ..., body = NULL) {
      if (identical(route, "GET /tags")) {
        return(list(list(id = 9, name = "blockr", parent_id = NULL)))
      }
      if (grepl("POST /content/.*/tags", route)) {
        posted <<- list(route = route, body = body)
      }
      list()
    }
  )

  rack_create(board, blockr_test_session, id = "wf", name = "WF")

  expect_false(is.null(posted))
  expect_equal(posted$body$tag_id, 9)
})

test_that("rack_upload does not tag when no tag is configured", {

  board <- mock_board_connect(account = "user_a")

  posted <- FALSE

  local_mocked_bindings(
    pin_upload = function(...) invisible(),
    pin_versions = function(...) single_version(),
    pin_exists = function(...) FALSE,
    .package = "pins"
  )
  local_mocked_bindings(
    connect_content_find = function(board, name) list(guid = "g"),
    connect_api = function(board, route, ...) {
      if (grepl("POST", route, fixed = TRUE)) {
        posted <<- TRUE
      }
      list()
    }
  )

  rack_create(board, blockr_test_session, id = "wf", name = "WF")

  expect_false(posted)
})

test_that("a failed tag write does not fail the upload", {

  board <- mock_board_connect(account = "user_a")
  withr::local_options(blockr.session_connect_tag = "blockr")

  local_mocked_bindings(
    pin_upload = function(...) invisible(),
    pin_versions = function(...) single_version(),
    pin_exists = function(...) FALSE,
    .package = "pins"
  )
  local_mocked_bindings(
    connect_content_find = function(board, name) list(guid = "g"),
    connect_api = function(board, route, ...) {
      if (identical(route, "GET /tags")) {
        return(list(list(id = 9, name = "blockr", parent_id = NULL)))
      }
      if (grepl("POST", route, fixed = TRUE)) {
        stop("tag write failed")
      }
      list()
    }
  )

  expect_warning(
    result <- rack_create(board, blockr_test_session, id = "wf", name = "WF"),
    class = "connect_tag_apply_failed"
  )
  expect_s3_class(result, "rack_id_pins_connect")
})
