test_that("board_folder creates directory if needed", {
  path <- file.path(tempdir(), paste0("blockr_test_", as.integer(Sys.time())))
  on.exit(unlink(path, recursive = TRUE))

  backend <- board_folder(path)
  expect_true(dir.exists(path))
  expect_s3_class(backend, "rack_board_folder")
})

test_that("new_rack_id_folder basic construction", {
  id <- new_rack_id_folder("my-workflow")
  expect_s3_class(id, "rack_id_folder")
  expect_s3_class(id, "rack_id")
  expect_equal(id$name, "my-workflow")
  expect_null(id$version)
})

test_that("new_rack_id_folder with version", {
  id <- new_rack_id_folder("my-workflow", "20260305T143022Z")
  expect_equal(id$version, "20260305T143022Z")
})

test_that("new_rack_id_folder rejects empty version", {
  expect_error(
    new_rack_id_folder("my-workflow", ""),
    class = "rack_id_folder_invalid_version"
  )
})

test_that("pin_name.rack_id_folder returns name", {
  id <- new_rack_id_folder("my-workflow")
  expect_equal(pin_name(id), "my-workflow")
})

test_that("display_name.rack_id_folder returns name", {
  id <- new_rack_id_folder("my-workflow")
  expect_equal(display_name(id), "my-workflow")
})

test_that("format.rack_id_folder", {
  id <- new_rack_id_folder("wf")
  expect_equal(format(id), "<rack_id_folder: wf>")

  id_v <- new_rack_id_folder("wf", "20260305T143022Z")
  expect_equal(format(id_v), "<rack_id_folder: wf@20260305T143022Z>")
})

test_that("rack_save persists and rack_list finds it", {
  backend <- board_folder(withr::local_tempdir())
  data <- list(blocks = list(), links = list())

  res <- rack_save(backend, data, name = "test-board")

  expect_s3_class(res, "rack_id_folder")
  expect_equal(res$name, "test-board")
  expect_true(not_null(res$version))

  boards <- rack_list(backend)
  expect_length(boards, 1L)
  expect_equal(display_name(boards[[1L]]), "test-board")
})

test_that("rack_list returns empty list on empty backend", {
  backend <- board_folder(withr::local_tempdir())
  expect_equal(rack_list(backend), list())
})

test_that("rack_list sorts by recency", {
  backend <- board_folder(withr::local_tempdir())

  rack_save(backend, list(x = 1), name = "older")
  Sys.sleep(1.1)
  rack_save(backend, list(x = 2), name = "newer")

  boards <- rack_list(backend)
  names <- vapply(boards, display_name, character(1L))
  expect_equal(names, c("newer", "older"))
})

test_that("rack_info returns version data.frame", {
  backend <- board_folder(withr::local_tempdir())
  data <- list(blocks = list())

  rack_save(backend, data, name = "info-test")
  Sys.sleep(1.1)
  data$v <- 2L
  rack_save(backend, data, name = "info-test")

  info <- rack_info(new_rack_id_folder("info-test"), backend)

  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), 2L)

  cols <- colnames(info)
  expect_true("version" %in% cols)
  expect_true("created" %in% cols)
  expect_true("hash" %in% cols)
})

test_that("rack_info returns empty data.frame for missing workflow", {
  backend <- board_folder(withr::local_tempdir())

  info <- rack_info(new_rack_id_folder("nonexistent"), backend)

  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), 0L)
})

test_that("rack_load succeeds with valid workflow", {
  backend <- board_folder(withr::local_tempdir())
  data <- list(blocks = list(a = 1), format = "test")

  rack_save(backend, data, name = "load-test")

  result <- rack_load(new_rack_id_folder("load-test"), backend)
  expect_type(result, "list")
  expect_equal(result$blocks$a, 1L)
})

test_that("rack_load with specific version", {
  backend <- board_folder(withr::local_tempdir())

  v1_data <- list(blocks = list(v = 1))
  v2_data <- list(blocks = list(v = 2))

  rack_save(backend, v1_data, name = "ver-load")
  Sys.sleep(1.1)
  rack_save(backend, v2_data, name = "ver-load")

  info <- rack_info(new_rack_id_folder("ver-load"), backend)
  older <- info$version[2L]

  result <- rack_load(new_rack_id_folder("ver-load", older), backend)
  expect_equal(result$blocks$v, 1L)
})

test_that("rack_load errors on missing workflow", {
  backend <- board_folder(withr::local_tempdir())

  expect_error(
    rack_load(new_rack_id_folder("does-not-exist"), backend),
    class = "rack_load_no_versions"
  )
})

test_that("rack_load errors on missing version", {
  backend <- board_folder(withr::local_tempdir())
  rack_save(backend, list(x = 1), name = "exists")

  expect_error(
    rack_load(new_rack_id_folder("exists", "99990101T000000Z"), backend),
    class = "rack_load_version_not_found"
  )
})

test_that("rack_delete removes specific version", {
  backend <- board_folder(withr::local_tempdir())
  data <- list(blocks = list())

  rack_save(backend, data, name = "del-ver")
  Sys.sleep(1.1)
  data$v <- 2L
  rack_save(backend, data, name = "del-ver")

  info <- rack_info(new_rack_id_folder("del-ver"), backend)
  expect_equal(nrow(info), 2L)

  older <- info$version[2L]
  rack_delete(new_rack_id_folder("del-ver", older), backend)

  remaining <- rack_info(new_rack_id_folder("del-ver"), backend)
  expect_equal(nrow(remaining), 1L)
  expect_false(older %in% remaining$version)
})

test_that("rack_delete errors on missing workflow", {
  backend <- board_folder(withr::local_tempdir())

  expect_error(
    rack_delete(new_rack_id_folder("nonexistent"), backend),
    class = "rack_delete_no_versions"
  )
})

test_that("rack_purge removes entire workflow directory", {
  backend <- board_folder(withr::local_tempdir())
  data <- list(blocks = list())

  rack_save(backend, data, name = "purge-me")
  expect_true(dir.exists(file.path(backend$path, "purge-me")))

  rack_purge(new_rack_id_folder("purge-me"), backend)
  expect_false(dir.exists(file.path(backend$path, "purge-me")))
})

test_that("last_saved returns timestamp", {
  backend <- board_folder(withr::local_tempdir())
  data <- list(blocks = list())

  rack_save(backend, data, name = "ts-test")

  ts <- last_saved(new_rack_id_folder("ts-test"), backend)
  expect_s3_class(ts, "POSIXct")
})

test_that("last_saved returns NULL for missing workflow", {
  backend <- board_folder(withr::local_tempdir())
  id <- new_rack_id_folder("no-such-workflow")

  ts <- last_saved(id, backend)
  expect_null(ts)
})

test_that("rack_file returns path to JSON", {
  backend <- board_folder(withr::local_tempdir())
  data <- list(blocks = list())

  res <- rack_save(backend, data, name = "file-test")
  path <- rack_file(res, backend)

  expect_true(file.exists(path))
  expect_true(grepl("\\.json$", path))
})

test_that("rack_id_from_input with folder backend", {
  backend <- board_folder(withr::local_tempdir())
  input <- list(name = "wf", user = "")

  id <- rack_id_from_input(input, backend)
  expect_s3_class(id, "rack_id_folder")
  expect_equal(id$name, "wf")
})

test_that("rack_id_for_board with folder backend", {
  backend <- board_folder(withr::local_tempdir())
  id <- rack_id_for_board("my-wf", backend)

  expect_s3_class(id, "rack_id_folder")
  expect_equal(id$name, "my-wf")
})

test_that("get_session_backend accepts rack_board_folder", {
  backend <- board_folder(withr::local_tempdir())
  withr::local_options(blockr.session_mgmt_backend = backend)

  result <- get_session_backend()
  expect_s3_class(result, "rack_board_folder")
})

test_that("get_session_backend accepts folder thunk", {
  path <- withr::local_tempdir()
  withr::local_options(
    blockr.session_mgmt_backend = function() board_folder(path)
  )

  result <- get_session_backend()
  expect_s3_class(result, "rack_board_folder")
})
