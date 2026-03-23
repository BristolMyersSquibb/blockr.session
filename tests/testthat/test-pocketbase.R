# Constructor tests -----------------------------------------------------------

test_that("new_rack_backend_pb creates backend", {
  be <- mock_backend_pb()
  expect_s3_class(be, "rack_backend_pb")
  expect_equal(be$url, "https://pb.example.com")
  expect_equal(be$auth$token, "fake-token-a")
  expect_equal(be$user_id, "user001")
})

test_that("new_rack_id_pb basic construction", {
  id <- new_rack_id_pb("my_board")
  expect_s3_class(id, "rack_id_pb")
  expect_s3_class(id, "rack_id")
  expect_equal(id$name, "my_board")
  expect_null(id$record_id)
  expect_null(id$version_id)
})

test_that("new_rack_id_pb with record_id and version_id", {
  id <- new_rack_id_pb(
    "board", record_id = "r1", version_id = "v1"
  )
  expect_equal(id$record_id, "r1")
  expect_equal(id$version_id, "v1")
})

test_that("new_rack_id_pb rejects empty record_id", {
  expect_error(
    new_rack_id_pb("board", record_id = ""),
    class = "rack_id_pb_invalid_record_id"
  )
})

test_that("new_rack_id_pb rejects empty version_id", {
  expect_error(
    new_rack_id_pb("board", version_id = ""),
    class = "rack_id_pb_invalid_version_id"
  )
})

# Accessor tests --------------------------------------------------------------

test_that("pin_name.rack_id_pb returns name", {
  id <- new_rack_id_pb("my_board")
  expect_equal(pin_name(id), "my_board")
})

test_that("format.rack_id_pb", {
  id <- new_rack_id_pb("board")
  expect_equal(format(id), "<rack_id_pb: board>")

  id_v <- new_rack_id_pb(
    "board", version_id = "ver001"
  )
  expect_equal(
    format(id_v), "<rack_id_pb: board@ver001>"
  )
})

# rack_id routing tests -------------------------------------------------------

test_that("rack_id_from_input creates pb id from record_id", {
  input <- list(
    name = "board",
    user = "",
    record_id = "r1",
    version = "v1"
  )
  id <- rack_id_from_input(input)

  expect_s3_class(id, "rack_id_pb")
  expect_equal(id$name, "board")
  expect_equal(id$record_id, "r1")
  expect_equal(id$version_id, "v1")
})

test_that("rack_id_from_input creates pb id from backend", {
  backend <- mock_backend_pb()
  input <- list(name = "board", user = "")
  id <- rack_id_from_input(input, backend)

  expect_s3_class(id, "rack_id_pb")
  expect_equal(id$name, "board")
  expect_null(id$record_id)
})

test_that("rack_id_from_input empty record_id falls through", {
  input <- list(
    name = "board", user = "alice", record_id = ""
  )
  id <- rack_id_from_input(input)
  expect_s3_class(id, "rack_id_pins_connect")
})

test_that("rack_id_for_board with pb backend", {
  backend <- mock_backend_pb()
  id <- rack_id_for_board("my_board", backend)

  expect_s3_class(id, "rack_id_pb")
  expect_equal(id$name, "my_board")
  expect_null(id$record_id)
})

test_that("rack_id_for_board sanitizes name for pb", {
  backend <- mock_backend_pb()
  id <- rack_id_for_board("Rebel eyas", backend)
  expect_equal(id$name, "Rebel_eyas")
})

# Capabilities ----------------------------------------------------------------

test_that("rack_capabilities all TRUE for pb", {
  backend <- mock_backend_pb()
  caps <- rack_capabilities(backend)

  expect_true(caps$versioning)
  expect_true(caps$tags)
  expect_true(caps$metadata)
  expect_true(caps$sharing)
  expect_true(caps$visibility)
  expect_true(caps$user_discovery)
})

# Fixture-based operation tests -----------------------------------------------

pb_fixture <- function(name) {
  path <- testthat::test_path(
    "_fixtures", "pocketbase", paste0(name, ".json")
  )
  jsonlite::fromJSON(
    readLines(path, warn = FALSE),
    simplifyVector = FALSE
  )
}

test_that("rack_list filters by blockr-session tag", {
  backend <- mock_backend_pb()
  fixture <- pb_fixture("board_list")

  local_mocked_bindings(
    pb_request = function(backend, method, path,
                          body = NULL, query = NULL) {
      fixture
    }
  )

  result <- rack_list(backend)
  expect_length(result, 2L)

  expect_s3_class(result[[1L]], "rack_id_pb")
  expect_equal(result[[1L]]$name, "test-board")
  expect_equal(result[[1L]]$record_id, "board001")
  expect_equal(result[[2L]]$name, "analysis")
})

test_that("rack_list filters by user tags", {
  backend <- mock_backend_pb()
  fixture <- pb_fixture("board_list")

  local_mocked_bindings(
    pb_request = function(backend, method, path,
                          body = NULL, query = NULL) {
      fixture
    }
  )

  result <- rack_list(backend, tags = "demo")
  expect_length(result, 1L)
  expect_equal(result[[1L]]$name, "analysis")
})

test_that("rack_list returns empty list when no boards", {
  backend <- mock_backend_pb()

  local_mocked_bindings(
    pb_request = function(backend, method, path,
                          body = NULL, query = NULL) {
      pb_empty_list()
    }
  )

  result <- rack_list(backend)
  expect_length(result, 0L)
})

test_that("rack_save creates board and version", {
  backend <- mock_backend_pb()
  board_rec <- pb_board_record()
  ver_rec <- pb_version_record()

  local_mocked_bindings(
    pb_request = mock_pb_responses(
      pb_empty_list(),
      board_rec,
      ver_rec
    )
  )

  data <- list(
    blocks = list(a = list(type = "dataset_block")),
    links = list(),
    format = "test"
  )
  result <- rack_save(backend, data, name = "test-board")

  expect_s3_class(result, "rack_id_pb")
  expect_equal(result$name, "test-board")
  expect_equal(result$record_id, "board001")
  expect_equal(result$version_id, "ver001")
})

test_that("rack_save reuses existing board", {
  backend <- mock_backend_pb()
  board_rec <- pb_board_record()
  ver_rec <- pb_version_record(id = "ver002")

  local_mocked_bindings(
    pb_request = mock_pb_responses(
      pb_list_response(board_rec),
      ver_rec
    )
  )

  data <- list(blocks = list())
  result <- rack_save(backend, data, name = "test-board")

  expect_equal(result$record_id, "board001")
  expect_equal(result$version_id, "ver002")
})

test_that("rack_info returns version data frame", {
  backend <- mock_backend_pb()
  fixture <- pb_fixture("version_list")

  local_mocked_bindings(
    pb_request = function(backend, method, path,
                          body = NULL, query = NULL) {
      fixture
    }
  )

  id <- new_rack_id_pb(
    "test-board", record_id = "board001"
  )
  info <- rack_info(id, backend)

  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), 2L)
  expect_true("version" %in% colnames(info))
  expect_true("created" %in% colnames(info))
  expect_true("hash" %in% colnames(info))
  expect_equal(info$version[1L], "ver002")
  expect_equal(info$version[2L], "ver001")
})

test_that("rack_info returns empty df for missing board", {
  backend <- mock_backend_pb()

  local_mocked_bindings(
    pb_request = function(backend, method, path,
                          body = NULL, query = NULL) {
      pb_empty_list()
    }
  )

  id <- new_rack_id_pb("nonexistent")
  info <- rack_info(id, backend)

  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), 0L)
})

test_that("rack_load round-trip with data", {
  backend <- mock_backend_pb()

  test_data <- list(
    blocks = list(a = list(type = "dataset_block")),
    links = list(),
    format = "test"
  )
  data_json <- as.character(
    jsonlite::toJSON(
      test_data, null = "null", auto_unbox = TRUE
    )
  )

  local_mocked_bindings(
    pb_request = function(backend, method, path,
                          body = NULL, query = NULL) {
      pb_version_record(data = data_json)
    }
  )

  id <- new_rack_id_pb(
    "test-board",
    record_id = "board001",
    version_id = "ver001"
  )
  result <- rack_load(id, backend)

  expect_type(result, "list")
  expect_equal(result$blocks$a$type, "dataset_block")
  expect_equal(result$format, "test")
})

test_that("rack_load resolves latest version", {
  backend <- mock_backend_pb()
  fixture <- pb_fixture("version_list")

  ver_rec <- pb_version_record(
    id = "ver002",
    data = '{"blocks":{"b":{"type":"filter_block"}}}'
  )

  local_mocked_bindings(
    pb_request = mock_pb_responses(
      pb_list_response(pb_board_record()),
      fixture,
      ver_rec
    )
  )

  id <- new_rack_id_pb("test-board")
  result <- rack_load(id, backend)

  expect_equal(result$blocks$b$type, "filter_block")
})

test_that("rack_load errors on missing board", {
  backend <- mock_backend_pb()

  local_mocked_bindings(
    pb_request = mock_pb_responses(
      pb_empty_list(),
      pb_empty_list()
    )
  )

  id <- new_rack_id_pb("nonexistent")
  expect_error(
    rack_load(id, backend),
    class = "rack_load_no_versions"
  )
})

test_that("rack_delete removes version by id", {
  backend <- mock_backend_pb()

  local_mocked_bindings(
    pb_request = function(backend, method, path,
                          body = NULL, query = NULL) {
      expect_equal(method, "DELETE")
      expect_match(path, "ver001")
      invisible(NULL)
    }
  )

  id <- new_rack_id_pb(
    "test-board",
    record_id = "board001",
    version_id = "ver001"
  )
  result <- rack_delete(id, backend)
  expect_true(result)
})

test_that("rack_purge removes board by record_id", {
  backend <- mock_backend_pb()

  local_mocked_bindings(
    pb_request = function(backend, method, path,
                          body = NULL, query = NULL) {
      expect_equal(method, "DELETE")
      expect_match(path, "board001")
      invisible(NULL)
    }
  )

  id <- new_rack_id_pb(
    "test-board", record_id = "board001"
  )
  result <- rack_purge(id, backend)
  expect_true(result)
})

test_that("rack_purge looks up board by name", {
  backend <- mock_backend_pb()
  board_rec <- pb_board_record()
  deleted <- FALSE

  local_mocked_bindings(
    pb_request = mock_pb_responses(
      pb_list_response(board_rec),
      function(method, path, body, query) {
        deleted <<- TRUE
        expect_equal(method, "DELETE")
        invisible(NULL)
      }
    )
  )

  id <- new_rack_id_pb("test-board")
  rack_purge(id, backend)
  expect_true(deleted)
})

# Tags ------------------------------------------------------------------------

test_that("rack_tags returns user tags", {
  backend <- mock_backend_pb()

  board_rec <- pb_board_record(
    tags = list("blockr-session", "analysis", "demo")
  )

  local_mocked_bindings(
    pb_request = function(backend, method, path,
                          body = NULL, query = NULL) {
      board_rec
    }
  )

  id <- new_rack_id_pb(
    "test-board", record_id = "board001"
  )
  tags <- rack_tags(id, backend)

  expect_equal(sort(tags), c("analysis", "demo"))
  expect_false("blockr-session" %in% tags)
})

test_that("rack_set_tags preserves session marker", {
  backend <- mock_backend_pb()
  board_rec <- pb_board_record()
  captured_body <- NULL

  local_mocked_bindings(
    pb_request = mock_pb_responses(
      board_rec,
      function(method, path, body, query) {
        captured_body <<- body
        board_rec
      }
    )
  )

  id <- new_rack_id_pb(
    "test-board", record_id = "board001"
  )
  rack_set_tags(id, backend, c("analysis", "demo"))

  expect_true(
    "blockr-session" %in% captured_body$tags
  )
  expect_true("analysis" %in% captured_body$tags)
  expect_true("demo" %in% captured_body$tags)
})

# ACL -------------------------------------------------------------------------

test_that("rack_acl returns acl_type", {
  backend <- mock_backend_pb()

  local_mocked_bindings(
    pb_request = function(backend, method, path,
                          body = NULL, query = NULL) {
      pb_board_record(acl_type = "logged_in")
    }
  )

  id <- new_rack_id_pb(
    "test-board", record_id = "board001"
  )
  expect_equal(rack_acl(id, backend), "logged_in")
})

test_that("rack_set_acl updates acl_type", {
  backend <- mock_backend_pb()
  board_rec <- pb_board_record()
  captured_body <- NULL

  local_mocked_bindings(
    pb_request = mock_pb_responses(
      board_rec,
      function(method, path, body, query) {
        captured_body <<- body
        expect_equal(method, "PATCH")
        board_rec
      }
    )
  )

  id <- new_rack_id_pb(
    "test-board", record_id = "board001"
  )
  rack_set_acl(id, backend, "logged_in")
  expect_equal(captured_body$acl_type, "logged_in")
})

# Sharing ---------------------------------------------------------------------

test_that("rack_share sends PATCH with shared_with+", {
  backend <- mock_backend_pb()
  board_rec <- pb_board_record()
  captured_body <- NULL

  local_mocked_bindings(
    pb_request = mock_pb_responses(
      board_rec,
      function(method, path, body, query) {
        captured_body <<- body
        expect_equal(method, "PATCH")
        board_rec
      }
    )
  )

  id <- new_rack_id_pb(
    "test-board", record_id = "board001"
  )
  rack_share(id, backend, "usr002")
  expect_equal(captured_body[["shared_with+"]], "usr002")
})

test_that("rack_unshare sends PATCH with shared_with-", {
  backend <- mock_backend_pb()
  board_rec <- pb_board_record()
  captured_body <- NULL

  local_mocked_bindings(
    pb_request = mock_pb_responses(
      board_rec,
      function(method, path, body, query) {
        captured_body <<- body
        board_rec
      }
    )
  )

  id <- new_rack_id_pb(
    "test-board", record_id = "board001"
  )
  rack_unshare(id, backend, "usr002")
  expect_equal(
    captured_body[["shared_with-"]], "usr002"
  )
})

test_that("rack_shares returns expanded users", {
  backend <- mock_backend_pb()
  fixture <- pb_fixture("board_with_shares")

  local_mocked_bindings(
    pb_request = mock_pb_responses(
      fixture,
      fixture
    )
  )

  id <- new_rack_id_pb(
    "test-board", record_id = "board001"
  )
  shares <- rack_shares(id, backend)

  expect_length(shares, 2L)

  expect_equal(shares[[1L]]$principal_guid, "usr002")
  expect_equal(
    shares[[1L]]$display_name, "Alice Smith alice@example.com"
  )
  expect_equal(shares[[2L]]$principal_guid, "usr003")
})

test_that("rack_shares returns empty for no shares", {
  backend <- mock_backend_pb()
  board_rec <- pb_board_record()

  local_mocked_bindings(
    pb_request = mock_pb_responses(
      board_rec,
      board_rec
    )
  )

  id <- new_rack_id_pb(
    "test-board", record_id = "board001"
  )
  shares <- rack_shares(id, backend)
  expect_length(shares, 0L)
})

# Sharing round-trip ----------------------------------------------------------

test_that("share then list then unshare round-trip", {
  backend <- mock_backend_pb()
  board_rec <- pb_board_record()
  board_shared <- pb_fixture("board_with_shares")

  local_mocked_bindings(
    pb_request = mock_pb_responses(
      board_rec,
      board_rec,
      board_rec,
      board_shared,
      board_rec,
      board_rec
    )
  )

  id <- new_rack_id_pb(
    "test-board", record_id = "board001"
  )

  rack_share(id, backend, "usr002")

  shares <- rack_shares(id, backend)
  expect_length(shares, 2L)

  rack_unshare(id, backend, "usr002")
})

# User discovery --------------------------------------------------------------

test_that("rack_find_users searches by name/email", {
  backend <- mock_backend_pb()
  fixture <- pb_fixture("user_search")

  local_mocked_bindings(
    pb_request = function(backend, method, path,
                          body = NULL, query = NULL) {
      expect_match(query$filter, "al")
      fixture
    }
  )

  users <- rack_find_users(backend, "al")
  expect_length(users, 2L)

  expect_equal(users[[1L]]$guid, "usr002")
  expect_equal(users[[1L]]$first_name, "Alice")
  expect_equal(users[[1L]]$last_name, "Smith")
  expect_equal(users[[1L]]$email, "alice@example.com")

  expect_equal(users[[2L]]$guid, "usr004")
  expect_equal(users[[2L]]$first_name, "Alex")
})

# Error paths -----------------------------------------------------------------

test_that("pb_request errors on 403", {
  backend <- mock_backend_pb()

  local_mocked_bindings(
    pb_request = function(backend, method, path,
                          body = NULL, query = NULL) {
      blockr_abort(
        paste0(
          "PocketBase request failed (403): ",
          "Only admins can perform this action."
        ),
        class = "pb_request_error"
      )
    }
  )

  id <- new_rack_id_pb(
    "test-board", record_id = "board001"
  )
  expect_error(
    rack_acl(id, backend),
    class = "pb_request_error"
  )
})

test_that("pb_request errors on 404", {
  backend <- mock_backend_pb()

  local_mocked_bindings(
    pb_request = function(backend, method, path,
                          body = NULL, query = NULL) {
      blockr_abort(
        paste0(
          "PocketBase request failed (404): ",
          "The requested resource wasn't found."
        ),
        class = "pb_request_error"
      )
    }
  )

  id <- new_rack_id_pb(
    "test-board",
    record_id = "board001",
    version_id = "missing"
  )
  expect_error(
    rack_load(id, backend),
    class = "pb_request_error"
  )
})

test_that("rack_purge errors on missing board", {
  backend <- mock_backend_pb()

  local_mocked_bindings(
    pb_request = function(backend, method, path,
                          body = NULL, query = NULL) {
      pb_empty_list()
    }
  )

  id <- new_rack_id_pb("nonexistent")
  expect_error(
    rack_purge(id, backend),
    class = "rack_purge_not_found"
  )
})

# board_query_string with PB IDs ---------------------------------------------

test_that("board_query_string includes record_id", {
  backend <- mock_backend_pb()
  id <- new_rack_id_pb(
    "board", record_id = "r1", version_id = "v1"
  )
  qs <- board_query_string(id, backend)

  expect_match(qs, "board_name=board")
  expect_match(qs, "record_id=r1")
  expect_match(qs, "version=v1")
})

test_that("board_query_string omits record_id when NULL", {
  backend <- mock_backend_pb()
  id <- new_rack_id_pb("board")
  qs <- board_query_string(id, backend)

  expect_match(qs, "board_name=board")
  expect_no_match(qs, "record_id")
  expect_no_match(qs, "version")
})

# last_saved ------------------------------------------------------------------

test_that("last_saved returns latest timestamp", {
  backend <- mock_backend_pb()
  fixture <- pb_fixture("version_list")

  local_mocked_bindings(
    pb_request = function(backend, method, path,
                          body = NULL, query = NULL) {
      fixture
    }
  )

  id <- new_rack_id_pb(
    "test-board", record_id = "board001"
  )
  ts <- last_saved(id, backend)

  expect_s3_class(ts, "POSIXct")
})
