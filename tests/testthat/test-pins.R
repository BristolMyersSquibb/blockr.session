# Constructors & accessors --------------------------------------------------

test_that("new_rack_id_pins basic construction", {
  id <- new_rack_id_pins("my_board")
  expect_s3_class(id, "rack_id_pins")
  expect_s3_class(id, "rack_id")
  expect_equal(id$id, "my_board")
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
  expect_equal(id$id, "my_board")
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

test_that("pin_name.rack_id_pins returns the slug", {
  id <- new_rack_id_pins("my_board")
  expect_equal(pin_name(id), "my_board")
})

test_that("pin_name.rack_id_pins_connect returns qualified slug", {
  id <- new_rack_id_pins_connect("alice", "my_board")
  expect_equal(pin_name(id), "alice/my_board")
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

  input <- list(id = "board", user = "")
  id <- rack_id_from_input(input)

  expect_s3_class(id, "rack_id_pins")
  expect_false(inherits(id, "rack_id_pins_connect"))
  expect_equal(id$id, "board")
})

test_that("rack_id_from_input connect", {

  input <- list(id = "board", user = "alice")
  id <- rack_id_from_input(input)

  expect_s3_class(id, "rack_id_pins_connect")
  expect_equal(id$user, "alice")
  expect_equal(id$id, "board")
})

test_that("rack_id_from_input with version", {

  input <- list(id = "board", user = "alice", version = "v1")
  id <- rack_id_from_input(input)

  expect_equal(id$version, "v1")
})

test_that("rack_id_from_input null version", {

  input <- list(id = "board", user = "")
  id <- rack_id_from_input(input)
  expect_null(id$version)

  input_null <- list(id = "board", user = "", version = "null")
  id2 <- rack_id_from_input(input_null)
  expect_null(id2$version)
})

test_that("rack_id_from_input accepts legacy board_name key", {
  id <- rack_id_from_input(list(name = "legacy_board", user = ""))
  expect_equal(id$id, "legacy_board")
})

test_that("rack_id_from_input connect backend infers user", {

  backend <- mock_board_connect(account = "nicolas")
  input <- list(id = "board", user = "")
  id <- rack_id_from_input(input, backend)

  expect_s3_class(id, "rack_id_pins_connect")
  expect_equal(id$user, "nicolas")
  expect_equal(id$id, "board")
  expect_equal(pin_name(id), "nicolas/board")
})

test_that("sanitize_pin_name replaces spaces and invalid chars", {
  expect_equal(sanitize_pin_name("Rebel eyas"), "Rebel_eyas")
  expect_equal(sanitize_pin_name("hello world!"), "hello_world")
  expect_equal(sanitize_pin_name("a.b-c_d"), "a.b-c_d")
  expect_equal(sanitize_pin_name("  spaced  "), "spaced")
  expect_equal(sanitize_pin_name("ab"), "abx")
  expect_equal(
    sanitize_pin_name(paste(rep("a", 100), collapse = "")),
    paste(rep("a", 64), collapse = "")
  )
})

# rack_create / rack_update -------------------------------------------------

test_that("rack_create keys on the supplied id; name is a separate attribute", {

  backend <- pins::board_temp(versioned = TRUE)

  id <- rack_create(
    backend, list(blocks = list()),
    id = "egoistic_lowchen", name = "My Cool Board!"
  )

  expect_equal(id$id, "egoistic_lowchen")
  expect_equal(pins::pin_list(backend), "egoistic_lowchen")
  expect_equal(rack_name(id, backend), "My Cool Board!")
})

test_that("rack_create errors on a colliding explicit id (no overwrite)", {

  # A create never overwrites: appending to an existing record is rack_update's
  # job. The save observer upserts (update when the id exists), so it only
  # reaches rack_create for a genuinely new id.
  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list(a = 1)), id = "dup", name = "First")

  expect_error(
    rack_create(backend, list(blocks = list(b = 2)), id = "dup",
                name = "Second"),
    class = "rack_create_exists"
  )

  expect_length(pins::pin_list(backend), 1L)
  expect_equal(nrow(rack_info(new_rack_id_pins("dup"), backend)), 1L)
})

test_that("distinct boards with the same name stay distinct (no merge)", {

  # Regression for #61: a name-keyed store appended a version and silently
  # merged two genuinely different boards into one pin.
  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list(a = 1)), id = "egoistic_lowchen",
              name = "Quarterly report")
  rack_create(backend, list(blocks = list(b = 2)), id = "crass_gecko",
              name = "Quarterly report")

  expect_setequal(
    pins::pin_list(backend),
    c("egoistic_lowchen", "crass_gecko")
  )

  records <- rack_list(backend)
  expect_length(records, 2L)
  expect_equal(chr_xtr(records, "name"),
               c("Quarterly report", "Quarterly report"))
})

test_that("titles that sanitize alike stay distinct (no collapse)", {

  # `My Workflow!` and `My Workflow?` both collapse to one slug; distinct ids
  # keep them apart.
  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "id_one",
              name = "My Workflow!")
  rack_create(backend, list(blocks = list()), id = "id_two",
              name = "My Workflow?")

  records <- rack_list(backend)
  expect_length(records, 2L)
  expect_setequal(chr_xtr(records, "name"),
                  c("My Workflow!", "My Workflow?"))
})

test_that("rack_update adds a version and preserves the name", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list(a = 1)), id = "board-x",
              name = "Original")
  Sys.sleep(1.1)
  res <- rack_update(new_rack_id_pins("board-x"), backend,
                     list(blocks = list(a = 2)))

  expect_equal(res$id, "board-x")
  expect_length(pins::pin_list(backend), 1L)
  expect_equal(nrow(rack_info(new_rack_id_pins("board-x"), backend)), 2L)
  expect_equal(rack_name(new_rack_id_pins("board-x"), backend), "Original")
})

test_that("rack_update errors when the record does not exist", {

  # rack_update only appends; creating a missing record is rack_create's job.
  backend <- pins::board_temp(versioned = TRUE)

  expect_error(
    rack_update(new_rack_id_pins("absent"), backend, list(blocks = list())),
    class = "rack_update_missing"
  )

  expect_length(pins::pin_list(backend), 0L)
})

test_that("rack_create persists and rack_list finds it as a record", {

  backend <- pins::board_temp(versioned = TRUE)

  res <- rack_create(backend, list(blocks = list()), id = "test-board",
                     name = "Test Board")

  expect_s3_class(res, "rack_id_pins")
  expect_equal(res$id, "test-board")
  expect_true(not_null(res$version))

  boards <- rack_list(backend)
  expect_length(boards, 1L)
  expect_s3_class(boards[[1L]], "rack_record")
  expect_equal(boards[[1L]]$id, "test-board")
  expect_equal(boards[[1L]]$name, "Test Board")
})

# rack_name / rack_rename ---------------------------------------------------

test_that("rack_name falls back to the slug for legacy pins", {

  backend <- pins::board_temp(versioned = TRUE)

  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(x = 1), tmp, null = "null")
  pins::pin_upload(backend, tmp, "legacy_slug", versioned = TRUE,
                   metadata = list(format = "v1"), tags = "blockr-session")

  expect_equal(rack_name(new_rack_id_pins("legacy_slug"), backend),
               "legacy_slug")
})

test_that("rack_rename writes the name without changing identity", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "stable-id", name = "Before")
  Sys.sleep(1.1)
  res <- rack_rename(new_rack_id_pins("stable-id"), backend, "After")

  expect_equal(res$id, "stable-id")
  expect_equal(pins::pin_list(backend), "stable-id")
  expect_equal(rack_name(new_rack_id_pins("stable-id"), backend), "After")
})

test_that("rack_rename errors when the record has no versions", {

  backend <- pins::board_temp(versioned = TRUE)

  suppressWarnings(
    expect_error(
      rack_rename(new_rack_id_pins("nope"), backend, "X"),
      class = "rack_rename_no_versions"
    )
  )
})

# rack_exists ---------------------------------------------------------------

test_that("rack_exists reflects whether the record is stored", {

  backend <- pins::board_temp(versioned = TRUE)
  rack_create(backend, list(blocks = list()), id = "here", name = "Here")

  expect_true(rack_exists(new_rack_id_pins("here"), backend))
  expect_false(rack_exists(new_rack_id_pins("absent"), backend))
})

# rack_record ---------------------------------------------------------------

test_that("rack_record returns id, name and version count", {

  backend <- pins::board_temp(versioned = TRUE)
  rack_create(backend, list(blocks = list()), id = "rec-id", name = "Rec Name")
  Sys.sleep(1.1)
  rack_update(new_rack_id_pins("rec-id"), backend, list(blocks = list(a = 1)))

  rec <- rack_record(new_rack_id_pins("rec-id"), backend)

  expect_s3_class(rec, "rack_record")
  expect_equal(rec$id, "rec-id")
  expect_equal(rec$name, "Rec Name")
  expect_equal(rec$n_versions, 2L)
})

# rack_info / rack_load -----------------------------------------------------

test_that("rack_info returns version data.frame", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "info-test", name = "Info")
  rack_update(new_rack_id_pins("info-test"), backend,
              list(blocks = list(v = 2)))

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

  info <- rack_info(new_rack_id_pins("nonexistent"), backend)

  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), 0L)
})

test_that("rack_load succeeds with valid pin", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list(a = 1), format = "test"),
              id = "load-test", name = "Load Test")

  result <- rack_load(new_rack_id_pins("load-test"), backend)
  expect_type(result, "list")
  expect_equal(result$blocks$a, 1L)
})

test_that("rack_load with specific version", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list(v = 1)), id = "ver-load", name = "VL")
  rack_update(new_rack_id_pins("ver-load"), backend, list(blocks = list(v = 2)))

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

  rack_create(backend, list(blocks = list()), id = "blockr-board",
              name = "Blockr Board")

  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(x = 1), tmp)
  pins::pin_upload(backend, tmp, "other-pin")

  ids <- chr_xtr(rack_list(backend), "id")

  expect_true("blockr-board" %in% ids)
  expect_false("other-pin" %in% ids)
})

test_that("rack_delete removes specific version", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "del-ver", name = "Del")
  rack_update(new_rack_id_pins("del-ver"), backend, list(blocks = list(v = 2)))

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

  rack_create(backend, list(blocks = list()), id = "purge-me", name = "Purge")

  expect_true("purge-me" %in% pins::pin_list(backend))

  rack_purge(new_rack_id_pins("purge-me"), backend)

  expect_false("purge-me" %in% pins::pin_list(backend))
})

test_that("last_saved returns timestamp", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "ts-test", name = "TS")

  ts <- last_saved(new_rack_id_pins("ts-test"), backend)
  expect_s3_class(ts, "POSIXct")
})

test_that("last_saved returns NULL for missing pin", {

  backend <- pins::board_temp(versioned = TRUE)
  id <- new_rack_id_pins("no-such-pin")

  ts <- last_saved(id, backend)
  expect_null(ts)
})

# Connect: rack_list --------------------------------------------------------

test_that("rack_list on Connect returns records, filters by tags", {

  board <- mock_board_connect()

  record_search <- function() {
    rack_create(
      board_a, blockr_test_session,
      id = "blockr-fixture-board", name = "blockr-fixture-board"
    )
    upload_blockr_json(
      board_a, list(x = 1), "blockr-fixture-plain",
      versioned = TRUE,
      metadata = list(format = "v1")
    )
    result <- pins::pin_search(board_a)
    result[grepl("blockr-fixture-", result$name, fixed = TRUE), ]
  }
  search_result <- connect_fixture(
    "pin_search",
    record_search,
    pin_cleanup(board_a, "blockr-fixture-board", "blockr-fixture-plain")
  )

  tagged <- lgl_ply(search_result$meta, has_tags)
  expect_true(any(tagged))
  expect_true(any(!tagged))

  local_mocked_bindings(
    pin_search = function(...) search_result,
    .package = "pins"
  )
  local_mocked_bindings(connect_content_titles = function(backend) list())

  result <- rack_list(board)
  expect_length(result, sum(tagged))

  classes <- lgl_ply(result, inherits, "rack_record")
  expect_true(all(classes))
})

test_that("rack_list on Connect splits user/id from qualified names", {

  board <- mock_board_connect()

  record_search <- function() {
    rack_create(
      board_a, blockr_test_session,
      id = "blockr-fixture-board", name = "blockr-fixture-board"
    )
    upload_blockr_json(
      board_a, list(x = 1), "blockr-fixture-plain",
      versioned = TRUE,
      metadata = list(format = "v1")
    )
    result <- pins::pin_search(board_a)
    result[grepl("blockr-fixture-", result$name, fixed = TRUE), ]
  }
  search_result <- connect_fixture(
    "pin_search",
    record_search,
    pin_cleanup(board_a, "blockr-fixture-board", "blockr-fixture-plain")
  )

  tagged <- lgl_ply(search_result$meta, has_tags)
  tagged_names <- search_result$name[tagged]

  local_mocked_bindings(
    pin_search = function(...) search_result,
    .package = "pins"
  )
  local_mocked_bindings(connect_content_titles = function(backend) list())

  result <- rack_list(board)

  actual_users <- chr_xtr(result, "user")
  expected_users <- chr_xtr(strsplit(tagged_names, "/", fixed = TRUE), 1L)
  expect_setequal(actual_users, expected_users)

  actual_ids <- chr_xtr(result, "id")
  expected_ids <- chr_xtr(strsplit(tagged_names, "/", fixed = TRUE), 2L)
  expect_setequal(actual_ids, expected_ids)
})

test_that("rack_list on Connect names records from the content title", {

  board <- mock_board_connect()

  record_search <- function() {
    rack_create(
      board_a, blockr_test_session,
      id = "blockr-fixture-board", name = "blockr-fixture-board"
    )
    upload_blockr_json(
      board_a, list(x = 1), "blockr-fixture-plain",
      versioned = TRUE,
      metadata = list(format = "v1")
    )
    result <- pins::pin_search(board_a)
    result[grepl("blockr-fixture-", result$name, fixed = TRUE), ]
  }
  search_result <- connect_fixture(
    "pin_search",
    record_search,
    pin_cleanup(board_a, "blockr-fixture-board", "blockr-fixture-plain")
  )

  tagged <- lgl_ply(search_result$meta, has_tags)
  slug <- chr_xtr(strsplit(search_result$name[tagged], "/", fixed = TRUE),
                  2L)[[1L]]

  local_mocked_bindings(
    pin_search = function(...) search_result,
    .package = "pins"
  )
  local_mocked_bindings(
    connect_content_titles = function(backend) {
      set_names(list("A Friendly Title"), slug)
    }
  )

  result <- rack_list(board)
  named <- Filter(function(r) identical(r$id, slug), result)
  expect_equal(named[[1L]]$name, "A Friendly Title")
})

# Connect: rack_info / rack_load --------------------------------------------

test_that("rack_info on Connect returns version data.frame", {

  board <- mock_board_connect()

  record_versions <- function() {
    qualified <- paste0(board_a$account, "/blockr-fixture-board")
    rack_create(
      board_a, blockr_test_session,
      id = "blockr-fixture-board", name = "blockr-fixture-board"
    )
    modified <- blockr_test_session
    modified$blocks$c <- list(type = "plot_block")
    rack_update(
      new_rack_id_pins_connect(board_a$account, "blockr-fixture-board"),
      board_a, modified
    )
    pins::pin_versions(board_a, qualified)
  }
  versions <- connect_fixture(
    "pin_versions_tagged",
    record_versions,
    pin_cleanup(board_a, "blockr-fixture-board")
  )

  local_mocked_bindings(
    pin_versions = function(board, name, ...) versions,
    .package = "pins"
  )

  id <- new_rack_id_pins_connect("user_a", "my_board")
  info <- rack_info(id, board)

  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), nrow(versions))

  cols <- colnames(info)
  expect_true("version" %in% cols)
  expect_true("created" %in% cols)
  expect_true("hash" %in% cols)
})

test_that("rack_load on Connect uses qualified pin name", {

  board <- mock_board_connect()

  record_versions <- function() {
    qualified <- paste0(board_a$account, "/blockr-fixture-board")
    rack_create(
      board_a, blockr_test_session,
      id = "blockr-fixture-board", name = "blockr-fixture-board"
    )
    modified <- blockr_test_session
    modified$blocks$c <- list(type = "plot_block")
    rack_update(
      new_rack_id_pins_connect(board_a$account, "blockr-fixture-board"),
      board_a, modified
    )
    pins::pin_versions(board_a, qualified)
  }
  versions <- connect_fixture(
    "pin_versions_tagged",
    record_versions,
    pin_cleanup(board_a, "blockr-fixture-board")
  )

  latest_ver <- versions$version[1L]

  record_meta <- function() {
    qualified <- paste0(board_a$account, "/blockr-fixture-board")
    pins::pin_meta(board_a, qualified, version = latest_ver)
  }
  meta <- connect_fixture("pin_meta_tagged", record_meta)

  record_download <- function() {
    qualified <- paste0(board_a$account, "/blockr-fixture-board")

    dl_path <- pins::pin_download(
      board_a, qualified, latest_ver, meta$pin_hash
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
  expect_true(not_null(result$blocks))
  expect_true("user_a/my_board" %in% requested_names)
})

test_that("rack_load on Connect errors for missing pin", {

  board <- mock_board_connect()

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

  board <- mock_board_connect()

  record_versions <- function() {
    qualified <- paste0(board_a$account, "/blockr-fixture-plain")
    upload_blockr_json(
      board_a, list(x = 1), "blockr-fixture-plain",
      versioned = TRUE,
      metadata = list(format = "v1")
    )
    pins::pin_versions(board_a, qualified)
  }
  versions <- connect_fixture(
    "pin_versions_untagged",
    record_versions,
    pin_cleanup(board_a, "blockr-fixture-plain")
  )

  untagged_ver <- versions$version[1L]

  record_meta <- function() {
    qualified <- paste0(board_a$account, "/blockr-fixture-plain")
    pins::pin_meta(board_a, qualified, version = untagged_ver)
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

test_that("rack_load on Connect loads specific version directly", {

  board <- mock_board_connect()

  record_versions <- function() {
    qualified <- paste0(board_a$account, "/blockr-fixture-board")
    rack_create(
      board_a, blockr_test_session,
      id = "blockr-fixture-board", name = "blockr-fixture-board"
    )
    modified <- blockr_test_session
    modified$blocks$c <- list(type = "plot_block")
    rack_update(
      new_rack_id_pins_connect(board_a$account, "blockr-fixture-board"),
      board_a, modified
    )
    pins::pin_versions(board_a, qualified)
  }
  versions <- connect_fixture(
    "pin_versions_tagged",
    record_versions,
    pin_cleanup(board_a, "blockr-fixture-board")
  )

  older_ver <- versions$version[2L]

  record_meta <- function() {
    qualified <- paste0(board_a$account, "/blockr-fixture-board")
    pins::pin_meta(board_a, qualified, version = older_ver)
  }
  meta <- connect_fixture("pin_meta_specific_version", record_meta)

  record_download <- function() {
    qualified <- paste0(board_a$account, "/blockr-fixture-board")
    dl_path <- pins::pin_download(
      board_a, qualified, older_ver, meta$pin_hash
    )
    jsonlite::fromJSON(dl_path, simplifyVector = FALSE)
  }
  download_data <- connect_fixture(
    "pin_download_specific_version",
    record_download
  )

  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(download_data, tmp, auto_unbox = TRUE, null = "null")

  versions_called <- FALSE

  local_mocked_bindings(
    pin_versions = function(...) {
      versions_called <<- TRUE
      versions
    },
    pin_meta = function(...) meta,
    pin_download = function(...) tmp,
    .package = "pins"
  )

  id <- new_rack_id_pins_connect("user_a", "my_board", version = older_ver)
  result <- rack_load(id, board)

  expect_type(result, "list")
  expect_true(not_null(result$blocks))
  expect_false(versions_called)
})

test_that("rack_load on Connect from another user uses qualified name", {

  board <- mock_board_connect(account = "user_b")

  record_versions <- function() {
    qualified <- paste0(board_a$account, "/blockr-fixture-board")
    rack_create(
      board_a, blockr_test_session,
      id = "blockr-fixture-board", name = "blockr-fixture-board"
    )
    modified <- blockr_test_session
    modified$blocks$c <- list(type = "plot_block")
    rack_update(
      new_rack_id_pins_connect(board_a$account, "blockr-fixture-board"),
      board_a, modified
    )
    pins::pin_versions(board_a, qualified)
  }
  versions <- connect_fixture(
    "pin_versions_tagged",
    record_versions,
    pin_cleanup(board_a, "blockr-fixture-board")
  )

  latest_ver <- versions$version[1L]

  record_meta <- function() {
    qualified <- paste0(board_a$account, "/blockr-fixture-board")
    pins::pin_meta(board_a, qualified, version = latest_ver)
  }
  meta <- connect_fixture("pin_meta_tagged", record_meta)

  record_download <- function() {
    qualified <- paste0(board_a$account, "/blockr-fixture-board")
    dl_path <- pins::pin_download(
      board_a, qualified, latest_ver, meta$pin_hash
    )
    jsonlite::fromJSON(dl_path, simplifyVector = FALSE)
  }
  download_data <- connect_fixture("pin_download_tagged", record_download)

  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(download_data, tmp, auto_unbox = TRUE, null = "null")

  requested_names <- character()

  local_mocked_bindings(
    pin_versions = function(board, name, ...) {
      requested_names[length(requested_names) + 1L] <<- name
      versions
    },
    pin_meta = function(...) meta,
    pin_download = function(...) tmp,
    .package = "pins"
  )

  id <- new_rack_id_pins_connect("user_a", "my_board")
  result <- rack_load(id, board)

  expect_type(result, "list")
  expect_true("user_a/my_board" %in% requested_names)
})

# Connect: rack_create / rack_rename / rack_name ----------------------------

test_that("rack_create on Connect returns a connect rack_id", {

  board <- mock_board_connect(account = "user_a")

  record_versions <- function() {
    qualified <- paste0(board_a$account, "/blockr-fixture-board")
    rack_create(
      board_a, blockr_test_session,
      id = "blockr-fixture-board", name = "blockr-fixture-board"
    )
    pins::pin_versions(board_a, qualified)
  }
  versions <- connect_fixture(
    "pin_versions_single",
    record_versions,
    pin_cleanup(board_a, "blockr-fixture-board")
  )

  local_mocked_bindings(
    pin_upload = function(...) invisible(),
    pin_versions = function(...) versions,
    pin_exists = function(...) FALSE,
    .package = "pins"
  )
  local_mocked_bindings(
    connect_content_find = function(board, name) list(guid = "g"),
    connect_api = function(...) list()
  )

  result <- rack_create(board, blockr_test_session, id = "my_board",
                        name = "my_board")

  expect_s3_class(result, "rack_id_pins_connect")
  expect_equal(result$user, "user_a")
  expect_equal(result$id, "my_board")
  expect_true(not_null(result$version))
})

test_that("rack_create on Connect uploads to the owner-qualified slug", {

  board <- mock_board_connect(account = "user_a")

  record_versions <- function() {
    qualified <- paste0(board_a$account, "/blockr-fixture-new")
    try(pins::pin_delete(board_a, qualified), silent = TRUE)
    rack_create(
      board_a, blockr_test_session,
      id = "blockr-fixture-new", name = "blockr-fixture-new"
    )
    pins::pin_versions(board_a, qualified)
  }
  versions <- connect_fixture(
    "pin_versions_single",
    record_versions,
    pin_cleanup(board_a, "blockr-fixture-new")
  )

  uploaded_name <- NULL

  local_mocked_bindings(
    pin_upload = function(board, paths, name, ...) {
      uploaded_name <<- name
      invisible()
    },
    pin_versions = function(...) versions,
    pin_exists = function(...) FALSE,
    .package = "pins"
  )
  local_mocked_bindings(
    connect_content_find = function(board, name) list(guid = "g"),
    connect_api = function(...) list()
  )

  result <- rack_create(board, blockr_test_session, id = "my_new_board",
                        name = "my_new_board")

  expect_s3_class(result, "rack_id_pins_connect")
  expect_equal(result$id, "my_new_board")
  expect_equal(result$version, versions$version[1L])
  expect_equal(uploaded_name, "user_a/my_new_board")
})

test_that("rack_create on Connect sets the content title to the name", {

  board <- mock_board_connect(account = "user_a")

  versions <- data.frame(
    version = "20200101T000000Z-aaaaa",
    created = as.POSIXct("2020-01-01", tz = "UTC"),
    hash = "abc123",
    stringsAsFactors = FALSE
  )

  patched <- NULL

  local_mocked_bindings(
    pin_upload = function(...) invisible(),
    pin_versions = function(...) versions,
    pin_exists = function(...) FALSE,
    .package = "pins"
  )
  local_mocked_bindings(
    connect_content_find = function(board, name) list(guid = "the-guid"),
    connect_api = function(board, route, ..., body = NULL, query = NULL,
                           env = parent.frame()) {
      patched <<- list(route = glue::glue(route, .envir = env), body = body)
      list()
    }
  )

  rack_create(board, blockr_test_session, id = "fixed_slug",
              name = "A Human Title")

  expect_match(patched$route, "^PATCH /content/the-guid$")
  expect_equal(patched$body$title, "A Human Title")
})

test_that("rack_create on Connect scopes the write to the caller's account", {

  versions <- data.frame(
    version = "20200101T000000Z-aaaaa",
    created = as.POSIXct("2020-01-01", tz = "UTC"),
    hash = "abc123",
    stringsAsFactors = FALSE
  )

  upload_target <- function(account) {

    board <- mock_board_connect(account = account)
    uploaded_name <- NULL

    local_mocked_bindings(
      pin_upload = function(board, paths, name, ...) {
        uploaded_name <<- name
        invisible()
      },
      pin_versions = function(...) versions,
      pin_exists = function(...) FALSE,
      .package = "pins"
    )
    local_mocked_bindings(
      connect_content_find = function(board, name) list(guid = "g"),
      connect_api = function(...) list()
    )

    rack_create(board, blockr_test_session, id = "shared_board",
                name = "shared_board")
    uploaded_name
  }

  expect_equal(upload_target("user_a"), "user_a/shared_board")
  expect_equal(upload_target("user_b"), "user_b/shared_board")
})

test_that("rack_rename on Connect PATCHes the content title", {

  board <- mock_board_connect(account = "user_a")

  patched <- NULL

  local_mocked_bindings(
    connect_content_find = function(board, name) list(guid = "the-guid"),
    connect_api = function(board, route, ..., body = NULL, query = NULL,
                           env = parent.frame()) {
      patched <<- list(route = glue::glue(route, .envir = env), body = body)
      list()
    }
  )

  id <- new_rack_id_pins_connect("user_a", "my_board")
  res <- rack_rename(id, board, "Brand New Title")

  expect_s3_class(res, "rack_id_pins_connect")
  expect_equal(res$id, "my_board")
  expect_match(patched$route, "^PATCH /content/the-guid$")
  expect_equal(patched$body$title, "Brand New Title")
})

test_that("rack_name on Connect reads the content title", {

  board <- mock_board_connect(account = "user_a")

  local_mocked_bindings(
    connect_content_find = function(board, name) list(title = "Stored Title")
  )

  id <- new_rack_id_pins_connect("user_a", "my_board")
  expect_equal(rack_name(id, board), "Stored Title")
})

test_that("rack_name on Connect falls back to the slug when title is blank", {

  board <- mock_board_connect(account = "user_a")

  local_mocked_bindings(
    connect_content_find = function(board, name) list(title = "")
  )

  id <- new_rack_id_pins_connect("user_a", "my_board")
  expect_equal(rack_name(id, board), "my_board")
})

# Connect: rack_delete / rack_purge -----------------------------------------

test_that("rack_delete on Connect deletes latest version when none specified", {

  board <- mock_board_connect()

  record_versions <- function() {
    qualified <- paste0(board_a$account, "/blockr-fixture-board")
    rack_create(
      board_a, blockr_test_session,
      id = "blockr-fixture-board", name = "blockr-fixture-board"
    )
    modified <- blockr_test_session
    modified$blocks$c <- list(type = "plot_block")
    rack_update(
      new_rack_id_pins_connect(board_a$account, "blockr-fixture-board"),
      board_a, modified
    )
    pins::pin_versions(board_a, qualified)
  }
  versions <- connect_fixture(
    "pin_versions_tagged",
    record_versions,
    pin_cleanup(board_a, "blockr-fixture-board")
  )

  deleted_name <- NULL
  deleted_version <- NULL

  local_mocked_bindings(
    pin_versions = function(...) versions,
    pin_version_delete = function(board, name, version) {
      deleted_name <<- name
      deleted_version <<- version
      invisible()
    },
    .package = "pins"
  )

  id <- new_rack_id_pins_connect("user_a", "my_board")
  result <- rack_delete(id, board)

  expect_true(result)
  expect_equal(deleted_name, "user_a/my_board")
  expect_equal(deleted_version, versions$version[1L])
})

test_that("rack_delete on Connect deletes specific version directly", {

  board <- mock_board_connect()

  versions_called <- FALSE
  deleted_name <- NULL
  deleted_version <- NULL

  local_mocked_bindings(
    pin_versions = function(...) {
      versions_called <<- TRUE
      stop("should not be called")
    },
    pin_version_delete = function(board, name, version) {
      deleted_name <<- name
      deleted_version <<- version
      invisible()
    },
    .package = "pins"
  )

  target_ver <- "20240101T000000Z-abc"
  id <- new_rack_id_pins_connect("user_a", "my_board", version = target_ver)
  result <- rack_delete(id, board)

  expect_true(result)
  expect_equal(deleted_name, "user_a/my_board")
  expect_equal(deleted_version, target_ver)
  expect_false(versions_called)
})

test_that("rack_delete on Connect errors for missing pin", {

  board <- mock_board_connect()

  record_error <- function() {
    nonexistent <- paste0(board_a$account, "/nonexistent-pin")
    tryCatch(
      pins::pin_versions(board_a, nonexistent),
      error = function(e) list(message = conditionMessage(e))
    )
  }
  versions_err <- connect_fixture("pin_versions_error", record_error)

  local_mocked_bindings(
    pin_versions = function(...) stop(versions_err$message),
    .package = "pins"
  )

  id <- new_rack_id_pins_connect("user_a", "nonexistent")

  suppressWarnings(
    expect_error(rack_delete(id, board), class = "rack_delete_no_versions")
  )
})

test_that("rack_purge on Connect deletes entire pin", {

  board <- mock_board_connect()

  deleted_name <- NULL

  local_mocked_bindings(
    pin_delete = function(board, name) {
      deleted_name <<- name
      invisible()
    },
    .package = "pins"
  )

  id <- new_rack_id_pins_connect("user_a", "my_board")
  result <- rack_purge(id, board)

  expect_true(result)
  expect_equal(deleted_name, "user_a/my_board")
})

# rack_exists on Connect ----------------------------------------------------

test_that("rack_exists on Connect queries the qualified slug", {

  board <- mock_board_connect(account = "user_a")

  queried <- NULL

  local_mocked_bindings(
    pin_exists = function(board, name) {
      queried <<- name
      TRUE
    },
    .package = "pins"
  )

  id <- new_rack_id_pins_connect("user_a", "my_board")
  expect_true(rack_exists(id, board))
  expect_equal(queried, "user_a/my_board")
})

# rack_capabilities --------------------------------------------------------

test_that("rack_capabilities on local board returns expected list", {

  backend <- pins::board_temp()
  caps <- rack_capabilities(backend)

  expect_type(caps, "list")
  expect_true(caps$versioning)
  expect_true(caps$tags)
  expect_true(caps$metadata)
  expect_false(caps$sharing)
  expect_false(caps$visibility)
  expect_false(caps$user_discovery)
})

test_that("rack_capabilities on Connect board returns expected list", {

  board <- mock_board_connect()
  caps <- rack_capabilities(board)

  expect_type(caps, "list")
  expect_true(caps$versioning)
  expect_true(caps$tags)
  expect_true(caps$metadata)
  expect_true(caps$sharing)
  expect_true(caps$visibility)
  expect_true(caps$user_discovery)
})

# rack_tags ----------------------------------------------------------------

test_that("rack_tags strips session markers", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "tag-read", name = "Tag")

  tags <- rack_tags(new_rack_id_pins("tag-read"), backend)

  expect_false("blockr-session" %in% tags)
  expect_equal(tags, character(0))
})

test_that("rack_set_tags writes tags and preserves session marker", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "tag-write", name = "Tag")
  Sys.sleep(1.1)
  id <- new_rack_id_pins("tag-write")

  rack_set_tags(id, backend, tags = c("analysis", "production"))

  meta <- pins::pin_meta(backend, "tag-write")
  expect_true("blockr-session" %in% meta$tags)
  expect_true("analysis" %in% meta$tags)
  expect_true("production" %in% meta$tags)
})

test_that("rack_set_tags preserves the display name", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "tag-keep-name",
              name = "Keep Me")
  Sys.sleep(1.1)
  rack_set_tags(new_rack_id_pins("tag-keep-name"), backend, tags = "analysis")

  expect_equal(rack_name(new_rack_id_pins("tag-keep-name"), backend), "Keep Me")
})

test_that("rack_tags round-trip strips session marker from written tags", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "tag-roundtrip",
              name = "Tag")
  Sys.sleep(1.1)
  id <- new_rack_id_pins("tag-roundtrip")

  rack_set_tags(id, backend, tags = c("analysis", "production"))

  tags <- rack_tags(id, backend)

  expect_true("analysis" %in% tags)
  expect_true("production" %in% tags)
  expect_false("blockr-session" %in% tags)
})

# rack_acl -----------------------------------------------------------------

test_that("rack_acl on local pins returns public", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "acl-test", name = "ACL")

  acl <- rack_acl(new_rack_id_pins("acl-test"), backend)
  expect_equal(acl, "public")
})

# Unsupported operations ---------------------------------------------------

test_that("rack_set_acl on pins errors with rack_not_supported", {

  backend <- pins::board_temp()
  id <- new_rack_id_pins("test")

  expect_error(
    rack_set_acl(id, backend, "public"),
    class = "rack_not_supported"
  )
})

test_that("rack_share on pins errors with rack_not_supported", {

  backend <- pins::board_temp()
  id <- new_rack_id_pins("test")

  expect_error(
    rack_share(id, backend, "user1"),
    class = "rack_not_supported"
  )
})

test_that("rack_unshare on pins errors with rack_not_supported", {

  backend <- pins::board_temp()
  id <- new_rack_id_pins("test")

  expect_error(
    rack_unshare(id, backend, "user1"),
    class = "rack_not_supported"
  )
})

test_that("rack_shares on pins errors with rack_not_supported", {

  backend <- pins::board_temp()
  id <- new_rack_id_pins("test")

  expect_error(
    rack_shares(id, backend),
    class = "rack_not_supported"
  )
})

test_that("rack_find_users on pins errors with rack_not_supported", {

  backend <- pins::board_temp()

  expect_error(
    rack_find_users(backend, "alice"),
    class = "rack_not_supported"
  )
})

# rack_list tag filtering --------------------------------------------------

test_that("rack_list filters by user tags", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "board-a", name = "A")
  rack_create(backend, list(blocks = list()), id = "board-b", name = "B")
  Sys.sleep(1.1)

  rack_set_tags(
    new_rack_id_pins("board-b"), backend,
    tags = "production"
  )

  all_boards <- rack_list(backend)
  expect_length(all_boards, 2L)

  filtered <- rack_list(backend, tags = "production")
  expect_length(filtered, 1L)
  expect_equal(filtered[[1L]]$id, "board-b")
})

test_that("rack_list with no matching tags returns empty list", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list()), id = "board-no-match",
              name = "NM")

  result <- rack_list(backend, tags = "nonexistent")
  expect_length(result, 0L)
})

# Tag round-trip -----------------------------------------------------------

test_that("tag round-trip: create, tag, filter, load", {

  backend <- pins::board_temp(versioned = TRUE)

  rack_create(backend, list(blocks = list(a = 1)), id = "roundtrip",
              name = "RT")
  Sys.sleep(1.1)
  rack_set_tags(
    new_rack_id_pins("roundtrip"), backend,
    tags = "analysis"
  )

  filtered <- rack_list(backend, tags = "analysis")
  expect_length(filtered, 1L)

  result <- rack_load(new_rack_id_pins(filtered[[1L]]$id), backend)
  expect_equal(result$blocks$a, 1L)
})

# Connect: rack_acl -------------------------------------------------------

test_that("rack_acl on Connect returns access_type from API", {

  board <- mock_board_connect()

  record_content <- function() {
    rack_create(
      board_a, blockr_test_session,
      id = "blockr-fixture-acl", name = "blockr-fixture-acl"
    )
    qualified <- paste0(board_a$account, "/blockr-fixture-acl")
    content <- connect_api(
      board_a, "GET /content", query = list(name = "blockr-fixture-acl")
    )
    content[[1L]]
  }
  content <- connect_fixture(
    "content_find",
    record_content,
    pin_cleanup(board_a, "blockr-fixture-acl")
  )

  local_mocked_bindings(
    connect_content_find = function(board, name) content
  )

  id <- new_rack_id_pins_connect("user_a", "my_board")
  acl <- rack_acl(id, board)
  expect_type(acl, "character")
})

test_that("rack_set_acl on Connect calls API with access_type", {

  board <- mock_board_connect()
  id <- new_rack_id_pins_connect("user_a", "my_board")

  record_content <- function() {
    rack_create(
      board_a, blockr_test_session,
      id = "blockr-fixture-sharing", name = "blockr-fixture-sharing"
    )
    connect_api(
      board_a, "GET /content",
      query = list(name = "blockr-fixture-sharing")
    )[[1L]]
  }
  content <- connect_fixture(
    "content_for_sharing",
    record_content,
    pin_cleanup(board_a, "blockr-fixture-sharing")
  )

  patched_body <- NULL

  local_mocked_bindings(
    connect_content_find = function(board, name) content,
    connect_api = function(board, route, ..., body = NULL, query = NULL,
                           env = parent.frame()) {
      patched_body <<- body
      list()
    }
  )

  rack_set_acl(id, board, "all")
  expect_equal(patched_body$access_type, "all")
})

# Connect: rack_share / rack_unshare / rack_shares -------------------------

test_that("rack_shares on Connect returns permissions from API", {

  board <- mock_board_connect()

  record_content <- function() {
    rack_create(
      board_a, blockr_test_session,
      id = "blockr-fixture-sharing", name = "blockr-fixture-sharing"
    )
    content <- connect_api(
      board_a, "GET /content", query = list(name = "blockr-fixture-sharing")
    )
    content[[1L]]
  }
  content <- connect_fixture(
    "content_for_sharing",
    record_content,
    pin_cleanup(board_a, "blockr-fixture-sharing")
  )

  record_perms <- function() {
    me_b <- connect_api(board_b, "GET /user")
    connect_api(
      board_a, "POST /content/{content$guid}/permissions",
      body = list(
        principal_guid = me_b$guid,
        principal_type = "user",
        role = "viewer"
      )
    )
    connect_api(board_a, "GET /content/{content$guid}/permissions")
  }
  perms <- connect_fixture("content_permissions", record_perms)

  local_mocked_bindings(
    connect_content_find = function(board, name) content,
    connect_api = function(board, route, ...) perms
  )

  id <- new_rack_id_pins_connect("user_a", "my_board")
  result <- rack_shares(id, board)
  expect_type(result, "list")
  expect_length(result, 1L)
  expect_equal(result[[1L]]$principal_type, "user")
  expect_equal(result[[1L]]$role, "viewer")
})

test_that("rack_share on Connect posts permission", {

  board <- mock_board_connect()
  id <- new_rack_id_pins_connect("user_a", "my_board")

  record_content <- function() {
    rack_create(
      board_a, blockr_test_session,
      id = "blockr-fixture-sharing", name = "blockr-fixture-sharing"
    )
    connect_api(
      board_a, "GET /content",
      query = list(name = "blockr-fixture-sharing")
    )[[1L]]
  }
  content <- connect_fixture(
    "content_for_sharing",
    record_content,
    pin_cleanup(board_a, "blockr-fixture-sharing")
  )

  posted_body <- NULL

  local_mocked_bindings(
    connect_content_find = function(board, name) content,
    connect_api = function(board, route, ..., body = NULL, query = NULL,
                           env = parent.frame()) {
      posted_body <<- body
      list()
    }
  )

  rack_share(id, board, "user-guid-456")
  expect_equal(posted_body$principal_guid, "user-guid-456")
  expect_equal(posted_body$principal_type, "user")
  expect_equal(posted_body$role, "viewer")
})

test_that("rack_unshare on Connect deletes permission", {

  board <- mock_board_connect()
  id <- new_rack_id_pins_connect("user_a", "my_board")

  record_content <- function() {
    rack_create(
      board_a, blockr_test_session,
      id = "blockr-fixture-sharing", name = "blockr-fixture-sharing"
    )
    connect_api(
      board_a, "GET /content",
      query = list(name = "blockr-fixture-sharing")
    )[[1L]]
  }
  content <- connect_fixture(
    "content_for_sharing",
    record_content,
    pin_cleanup(board_a, "blockr-fixture-sharing")
  )

  record_perms <- function() {
    me_b <- connect_api(board_b, "GET /user")
    connect_api(
      board_a, "POST /content/{content$guid}/permissions",
      body = list(
        principal_guid = me_b$guid,
        principal_type = "user",
        role = "viewer"
      )
    )
    connect_api(board_a, "GET /content/{content$guid}/permissions")
  }
  perms <- connect_fixture("content_permissions", record_perms)

  deleted_route <- NULL

  local_mocked_bindings(
    connect_content_find = function(board, name) content,
    connect_api = function(board, route, ..., body = NULL, query = NULL,
                           env = parent.frame()) {
      resolved <- glue::glue(route, .envir = env)
      if (grepl("^GET", resolved)) return(perms)
      deleted_route <<- resolved
      list()
    }
  )

  target <- perms[[1L]]$principal_guid
  rack_unshare(id, board, target)

  expected <- paste0(
    "DELETE /content/", content$guid,
    "/permissions/", perms[[1L]]$id
  )
  expect_equal(deleted_route, expected)
})

test_that("rack_unshare on Connect errors for unknown principal", {

  board <- mock_board_connect()
  id <- new_rack_id_pins_connect("user_a", "my_board")

  record_content <- function() {
    rack_create(
      board_a, blockr_test_session,
      id = "blockr-fixture-sharing", name = "blockr-fixture-sharing"
    )
    connect_api(
      board_a, "GET /content",
      query = list(name = "blockr-fixture-sharing")
    )[[1L]]
  }
  content <- connect_fixture(
    "content_for_sharing",
    record_content,
    pin_cleanup(board_a, "blockr-fixture-sharing")
  )

  record_perms <- function() {
    me_b <- connect_api(board_b, "GET /user")
    connect_api(
      board_a, "POST /content/{content$guid}/permissions",
      body = list(
        principal_guid = me_b$guid,
        principal_type = "user",
        role = "viewer"
      )
    )
    connect_api(board_a, "GET /content/{content$guid}/permissions")
  }
  perms <- connect_fixture("content_permissions", record_perms)

  local_mocked_bindings(
    connect_content_find = function(board, name) content,
    connect_api = function(board, route, ...) perms
  )

  expect_error(
    rack_unshare(id, board, "nonexistent"),
    class = "rack_permission_not_found"
  )
})

# Connect: rack_find_users ------------------------------------------------

test_that("rack_find_users on Connect returns users from API", {

  board <- mock_board_connect()

  record_users <- function() {
    connect_api(
      board_a, "GET /users",
      query = list(prefix = board_a$account)
    )
  }
  user_response <- connect_fixture("user_search", record_users)

  local_mocked_bindings(
    connect_api = function(board, route, ...) user_response
  )

  users <- rack_find_users(board, "test")
  expect_type(users, "list")
  expect_length(users, 1L)
  expect_equal(users[[1L]]$username, "user_a")
  expect_equal(users[[1L]]$first_name, "Alice")
})
