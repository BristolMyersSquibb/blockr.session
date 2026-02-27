test_that("new_rack_id_pins basic construction", {
  id <- new_rack_id_pins("my_board")
  expect_s3_class(id, "rack_id_pins")
  expect_s3_class(id, "rack_id")
  expect_equal(id$name, "my_board")
  expect_null(id$version)
})

test_that("new_rack_id_pins with version", {
  id <- new_rack_id_pins("my_board", "v1")
  expect_equal(id$version, "v1")
})

test_that("new_rack_id_pins rejects empty version", {
  expect_error(
    new_rack_id_pins("my_board", ""),
    class = "rack_id_pins_invalid_version"
  )
})

test_that("new_rack_id_pins_connect basic construction", {

  id <- new_rack_id_pins_connect("alice", "my_board")

  expect_s3_class(id, "rack_id_pins_connect")
  expect_s3_class(id, "rack_id_pins")
  expect_s3_class(id, "rack_id")
  expect_equal(id$user, "alice")
  expect_equal(id$name, "my_board")
  expect_null(id$version)
})

test_that("new_rack_id_pins_connect with version", {
  id <- new_rack_id_pins_connect("alice", "my_board", "v1")
  expect_equal(id$version, "v1")
})

test_that("new_rack_id_pins_connect rejects empty user", {
  expect_error(
    new_rack_id_pins_connect("", "my_board"),
    class = "rack_id_pins_connect_invalid_user"
  )
})

test_that("pin_name.rack_id_pins returns bare name", {
  id <- new_rack_id_pins("my_board")
  expect_equal(pin_name(id), "my_board")
})

test_that("pin_name.rack_id_pins_connect returns qualified name", {
  id <- new_rack_id_pins_connect("alice", "my_board")
  expect_equal(pin_name(id), "alice/my_board")
})

test_that("display_name.rack_id_pins_connect returns bare name", {
  id <- new_rack_id_pins_connect("alice", "my_board")
  expect_equal(display_name(id), "my_board")
})

test_that("format.rack_id_pins", {

  id <- new_rack_id_pins("board")
  expect_equal(format(id), "<rack_id_pins: board>")

  id_v <- new_rack_id_pins("board", "v1")
  expect_equal(format(id_v), "<rack_id_pins: board@v1>")
})

test_that("format.rack_id_pins_connect", {

  id <- new_rack_id_pins_connect("alice", "board")
  expect_equal(format(id), "<rack_id_pins_connect: alice/board>")

  id_v <- new_rack_id_pins_connect("alice", "board", "v1")
  expect_equal(format(id_v), "<rack_id_pins_connect: alice/board@v1>")
})

test_that("rack_id_from_input local", {

  input <- list(name = "board", user = "")
  id <- rack_id_from_input(input)

  expect_s3_class(id, "rack_id_pins")
  is_connect <- inherits(id, "rack_id_pins_connect")
  expect_false(is_connect)
  expect_equal(id$name, "board")
})

test_that("rack_id_from_input connect", {

  input <- list(name = "board", user = "alice")
  id <- rack_id_from_input(input)

  expect_s3_class(id, "rack_id_pins_connect")
  expect_equal(id$user, "alice")
  expect_equal(id$name, "board")
})

test_that("rack_id_from_input with version", {

  input <- list(name = "board", user = "alice", version = "v1")
  id <- rack_id_from_input(input)

  expect_equal(id$version, "v1")
})

test_that("rack_id_from_input null version", {

  input <- list(name = "board", user = "")
  id <- rack_id_from_input(input)
  expect_null(id$version)

  input_null <- list(name = "board", user = "", version = "null")
  id2 <- rack_id_from_input(input_null)
  expect_null(id2$version)
})

test_that("rack_id_for_board local", {

  backend <- pins::board_temp()
  id <- rack_id_for_board("my_board", backend)

  expect_s3_class(id, "rack_id_pins")
  is_connect <- inherits(id, "rack_id_pins_connect")
  expect_false(is_connect)
})

test_that("rack_save persists and rack_list finds it", {

  backend <- pins::board_temp(versioned = TRUE)
  data <- list(
    blocks = list(),
    links = list()
  )

  res <- rack_save(backend, data, name = "test-board")

  expect_s3_class(res, "rack_id_pins")
  expect_equal(res$name, "test-board")

  has_version <- not_null(res$version)
  expect_true(has_version)

  boards <- rack_list(backend)
  expect_length(boards, 1L)
  expect_equal(display_name(boards[[1L]]), "test-board")
})

test_that("rack_info returns version data.frame", {

  backend <- pins::board_temp(versioned = TRUE)
  data <- list(
    blocks = list()
  )

  rack_save(backend, data, name = "info-test")
  Sys.sleep(1)
  rack_save(backend, data, name = "info-test")

  info <- rack_info(new_rack_id_pins("info-test"), backend)

  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), 2L)

  cols <- colnames(info)
  expect_true("version" %in% cols)
  expect_true("created" %in% cols)
  expect_true("hash" %in% cols)
})

test_that("rack_info returns empty data.frame for missing pin", {

  backend <- pins::board_temp(versioned = TRUE)

  expect_warning(
    info <- rack_info(new_rack_id_pins("nonexistent"), backend),
    class = "rack_info_failed"
  )

  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), 0L)
})

test_that("rack_load succeeds with valid pin", {

  backend <- pins::board_temp(versioned = TRUE)
  data <- list(blocks = list(a = 1), format = "test")

  rack_save(backend, data, name = "load-test")

  result <- rack_load(new_rack_id_pins("load-test"), backend)
  expect_type(result, "list")
  expect_equal(result$blocks$a, 1L)
})

test_that("rack_load with specific version", {

  backend <- pins::board_temp(versioned = TRUE)

  v1_data <- list(
    blocks = list(v = 1)
  )
  v2_data <- list(
    blocks = list(v = 2)
  )

  rack_save(backend, v1_data, name = "ver-load")
  Sys.sleep(1)
  rack_save(backend, v2_data, name = "ver-load")

  info <- rack_info(new_rack_id_pins("ver-load"), backend)
  older <- info$version[2L]

  result <- rack_load(new_rack_id_pins("ver-load", older), backend)
  expect_equal(result$blocks$v, 1L)
})

test_that("rack_load errors on missing pin", {

  backend <- pins::board_temp(versioned = TRUE)

  suppressWarnings(
    expect_error(
      rack_load(new_rack_id_pins("does-not-exist"), backend),
      class = "rack_load_no_versions"
    )
  )
})

test_that("rack_load errors on pin without blockr tags", {

  backend <- pins::board_temp(versioned = TRUE)

  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(x = 1), tmp)

  pins::pin_upload(
    backend, tmp, "no-tags",
    metadata = list(format = "v1")
  )

  expect_error(
    rack_load(new_rack_id_pins("no-tags"), backend),
    class = "rack_load_invalid_tags"
  )
})

test_that("rack_list filters out non-blockr pins", {

  backend <- pins::board_temp(versioned = TRUE)
  data <- list(
    blocks = list()
  )

  rack_save(backend, data, name = "blockr-board")

  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(x = 1), tmp)
  pins::pin_upload(backend, tmp, "other-pin")

  names <- chr_ply(rack_list(backend), display_name)

  expect_true("blockr-board" %in% names)
  expect_false("other-pin" %in% names)
})

test_that("rack_delete removes specific version", {

  backend <- pins::board_temp(versioned = TRUE)
  data <- list(
    blocks = list()
  )

  rack_save(backend, data, name = "del-ver")
  Sys.sleep(1)
  rack_save(backend, data, name = "del-ver")

  info <- rack_info(new_rack_id_pins("del-ver"), backend)
  expect_equal(nrow(info), 2L)

  older <- info$version[2L]
  rack_delete(new_rack_id_pins("del-ver", older), backend)

  remaining <- rack_info(new_rack_id_pins("del-ver"), backend)
  expect_equal(nrow(remaining), 1L)
  expect_false(older %in% remaining$version)
})

test_that("rack_purge removes entire pin", {

  backend <- pins::board_temp(versioned = TRUE)
  data <- list(
    blocks = list()
  )

  rack_save(backend, data, name = "purge-me")

  available <- pins::pin_list(backend)
  expect_true("purge-me" %in% available)

  rack_purge(new_rack_id_pins("purge-me"), backend)

  remaining <- pins::pin_list(backend)
  expect_false("purge-me" %in% remaining)
})

test_that("last_saved returns timestamp", {

  backend <- pins::board_temp(versioned = TRUE)
  data <- list(
    blocks = list()
  )

  rack_save(backend, data, name = "ts-test")

  ts <- last_saved(new_rack_id_pins("ts-test"), backend)
  expect_s3_class(ts, "POSIXct")
})

test_that("last_saved returns NULL for missing pin", {

  backend <- pins::board_temp(versioned = TRUE)
  id <- new_rack_id_pins("no-such-pin")

  expect_warning(
    ts <- last_saved(id, backend),
    class = "rack_info_failed"
  )
  expect_null(ts)
})

test_that("rack_list on Connect returns rack_id_pins_connect, filters tags", {

  board <- fake_board_connect()

  search_result <- fixture_df(
    connect_fixture(
      "pin_search",
      function() pins::pin_search(board_a)
    )
  )

  tagged <- lgl_ply(search_result$meta, has_tags)
  has_tagged <- any(tagged)
  has_untagged <- any(!tagged)

  expect_true(has_tagged)
  expect_true(has_untagged)

  local_mocked_bindings(
    pin_search = function(...) search_result,
    .package = "pins"
  )

  result <- rack_list(board)
  n_tagged <- sum(tagged)
  expect_length(result, n_tagged)

  classes <- lgl_ply(result, inherits, "rack_id_pins_connect")
  all_connect <- all(classes)
  expect_true(all_connect)
})

test_that("rack_list on Connect splits user/name from qualified names", {

  board <- fake_board_connect()

  search_result <- fixture_df(
    connect_fixture(
      "pin_search",
      function() pins::pin_search(board_a)
    )
  )

  tagged <- lgl_ply(search_result$meta, has_tags)
  tagged_names <- search_result$name[tagged]

  local_mocked_bindings(
    pin_search = function(...) search_result,
    .package = "pins"
  )

  result <- rack_list(board)

  actual_users <- chr_ply(result, function(x) x$user)
  expected_users <- chr_ply(
    strsplit(tagged_names, "/", fixed = TRUE),
    `[[`,
    1L
  )
  expect_setequal(actual_users, expected_users)

  actual_names <- chr_ply(result, display_name)
  expected_names <- chr_ply(
    strsplit(tagged_names, "/", fixed = TRUE),
    `[[`,
    2L
  )
  expect_setequal(actual_names, expected_names)
})

test_that("rack_info on Connect returns version data.frame", {

  board <- fake_board_connect()

  versions <- fixture_df(
    connect_fixture(
      "pin_versions_tagged",
      function() pins::pin_versions(board_a, qualified_a_board)
    )
  )

  local_mocked_bindings(
    pin_versions = function(board, name, ...) versions,
    .package = "pins"
  )

  id <- new_rack_id_pins_connect("user_a", "my_board")
  info <- rack_info(id, board)

  expect_s3_class(info, "data.frame")
  n_versions <- nrow(versions)
  expect_equal(nrow(info), n_versions)

  cols <- colnames(info)
  expect_true("version" %in% cols)
  expect_true("created" %in% cols)
  expect_true("hash" %in% cols)
})

test_that("rack_load on Connect uses qualified pin name", {

  board <- fake_board_connect()

  versions <- fixture_df(
    connect_fixture(
      "pin_versions_tagged",
      function() pins::pin_versions(board_a, qualified_a_board)
    )
  )

  latest_ver <- versions$version[1L]

  record_meta <- function() {
    pins::pin_meta(board_a, qualified_a_board, version = latest_ver)
  }
  meta <- connect_fixture("pin_meta_tagged", record_meta)

  record_download <- function() {

    dl_path <- pins::pin_download(
      board_a, qualified_a_board, latest_ver, meta$pin_hash
    )

    jsonlite::fromJSON(dl_path, simplifyVector = FALSE)
  }
  download_data <- connect_fixture("pin_download_tagged", record_download)

  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(download_data, tmp, auto_unbox = TRUE, null = "null")

  requested_names <- character()

  local_mocked_bindings(
    pin_versions = function(board, name, ...) {
      requested_names[[length(requested_names) + 1L]] <<- name
      versions
    },
    pin_meta = function(board, name, ...) meta,
    pin_download = function(...) tmp,
    .package = "pins"
  )

  id <- new_rack_id_pins_connect("user_a", "my_board")
  result <- rack_load(id, board)

  expect_type(result, "list")
  has_blocks <- not_null(result$blocks)
  expect_true(has_blocks)
  expect_true("user_a/my_board" %in% requested_names)
})

test_that("rack_load on Connect errors for missing pin", {

  board <- fake_board_connect()

  record_error <- function() {

    nonexistent <- paste0(board_a$account, "/nonexistent-pin")

    tryCatch(
      pins::pin_versions(board_a, nonexistent),
      error = function(e) {
        msg <- conditionMessage(e)
        list(message = msg)
      }
    )
  }
  versions_err <- connect_fixture("pin_versions_error", record_error)

  local_mocked_bindings(
    pin_versions = function(...) stop(versions_err$message),
    .package = "pins"
  )

  id <- new_rack_id_pins_connect("user_a", "nonexistent")

  suppressWarnings(
    expect_error(rack_load(id, board), class = "rack_load_no_versions")
  )
})

test_that("rack_load on Connect errors for pin without blockr tags", {

  board <- fake_board_connect()

  versions <- fixture_df(
    connect_fixture(
      "pin_versions_untagged",
      function() pins::pin_versions(board_a, qualified_a_plain)
    )
  )

  untagged_ver <- versions$version[1L]

  record_meta <- function() {
    pins::pin_meta(board_a, qualified_a_plain, version = untagged_ver)
  }
  meta <- connect_fixture("pin_meta_untagged", record_meta)

  local_mocked_bindings(
    pin_versions = function(...) versions,
    pin_meta = function(...) meta,
    .package = "pins"
  )

  id <- new_rack_id_pins_connect("user_a", "plain_pin")

  expect_error(rack_load(id, board), class = "rack_load_invalid_tags")
})

test_that("rack_save on Connect returns rack_id_pins_connect", {

  board <- fake_board_connect(account = "user_a")

  versions <- fixture_df(
    connect_fixture(
      "pin_versions_tagged",
      function() pins::pin_versions(board_a, qualified_a_board)
    )
  )

  local_mocked_bindings(
    pin_upload = function(...) invisible(),
    pin_versions = function(...) versions,
    .package = "pins"
  )

  data <- list(
    blocks = list(
      a = list(type = "dataset_block")
    ),
    links = list(),
    format = "test"
  )

  result <- rack_save(board, data, name = "my_board")

  expect_s3_class(result, "rack_id_pins_connect")
  expect_equal(result$user, "user_a")
  expect_equal(result$name, "my_board")

  has_version <- not_null(result$version)
  expect_true(has_version)
})
