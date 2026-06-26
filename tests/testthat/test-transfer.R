# rack_download / rack_upload -----------------------------------------------

test_that("rack_upload stores file and rack_download retrieves it", {

  backend <- pins::board_temp(versioned = TRUE)

  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(blocks = list(a = 1)), tmp, null = "null")

  id <- rack_upload(backend, tmp, new_rack_id_pins("upload-test"))

  expect_s3_class(id, "rack_id_pins")
  expect_equal(id$id, "upload-test")
  expect_true(not_null(id$version))

  path <- rack_download(id, backend)
  expect_true(file.exists(path))

  data <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
  expect_equal(data$blocks$a, 1L)
})

test_that("rack_upload keys on the id, never the display name", {

  backend <- pins::board_temp(versioned = TRUE)

  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(x = 1), tmp, null = "null")

  id <- rack_upload(
    backend, tmp, new_rack_id_pins("stable-slug"),
    name = "Rebel eyas"
  )

  expect_equal(id$id, "stable-slug")
  expect_true("stable-slug" %in% pins::pin_list(backend))
  expect_equal(rack_name(id, backend), "Rebel eyas")
})

test_that("rack_download errors on missing pin", {

  backend <- pins::board_temp(versioned = TRUE)

  suppressWarnings(
    expect_error(
      rack_download(new_rack_id_pins("does-not-exist"), backend),
      class = "rack_load_no_versions"
    )
  )
})

test_that("rack_download errors on pin without blockr tags", {

  backend <- pins::board_temp(versioned = TRUE)

  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(x = 1), tmp)

  pins::pin_upload(
    backend, tmp, "no-tags",
    metadata = list(format = "v1")
  )

  expect_error(
    rack_download(new_rack_id_pins("no-tags"), backend),
    class = "rack_load_invalid_tags"
  )
})

test_that("rack_download with specific version", {

  backend <- pins::board_temp(versioned = TRUE)

  id1 <- rack_create(backend, list(v = 1), id = "ver-dl", name = "Ver DL")
  rack_append(new_rack_id_pins("ver-dl"), backend, list(v = 2))

  path <- rack_download(id1, backend)
  data <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
  expect_equal(data$v, 1L)
})

test_that("rack_load and rack_create compose download/upload correctly", {

  backend <- pins::board_temp(versioned = TRUE)
  data <- list(blocks = list(x = "hello"), links = list())

  id <- rack_create(backend, data, id = "roundtrip", name = "Round Trip")
  result <- rack_load(id, backend)

  expect_equal(result$blocks$x, "hello")
  expect_equal(result$links, list())
})

# prepare_download ---------------------------------------------------------

test_that("prepare_download returns JSON path for single workflow", {

  backend <- pins::board_temp(versioned = TRUE)
  rack_create(backend, list(blocks = list(a = 1)), id = "single-dl",
              name = "Single DL")

  sel <- list(list(id = "single-dl", name = "single-dl", user = ""))
  path <- prepare_download(sel, backend)

  expect_true(file.exists(path))
  expect_match(path, "\\.json$")

  data <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
  expect_equal(data$blocks$a, 1L)
})

test_that("prepare_download returns ZIP for multiple workflows", {

  backend <- pins::board_temp(versioned = TRUE)
  rack_create(backend, list(v = 1), id = "multi-a", name = "Multi A")
  rack_create(backend, list(v = 2), id = "multi-b", name = "Multi B")

  sel <- list(
    list(id = "multi-a", name = "multi-a", user = ""),
    list(id = "multi-b", name = "multi-b", user = "")
  )
  path <- prepare_download(sel, backend)

  expect_true(file.exists(path))
  expect_match(path, "\\.zip$")

  tmp_dir <- tempfile("unzip_")
  dir.create(tmp_dir)
  zip::unzip(path, exdir = tmp_dir)

  json_files <- list.files(tmp_dir, pattern = "\\.json$")
  expect_length(json_files, 2L)
  expect_true("multi-a.json" %in% json_files)
  expect_true("multi-b.json" %in% json_files)
})

test_that("prepare_download skips missing workflows in multi", {

  backend <- pins::board_temp(versioned = TRUE)
  rack_create(backend, list(v = 1), id = "exists", name = "Exists")

  sel <- list(
    list(id = "exists", name = "exists", user = ""),
    list(id = "does-not-exist", name = "does-not-exist", user = "")
  )

  # Should not error — skips the missing one
  path <- suppressWarnings(prepare_download(sel, backend))
  expect_true(file.exists(path))

  tmp_dir <- tempfile("unzip_")
  dir.create(tmp_dir)
  zip::unzip(path, exdir = tmp_dir)

  json_files <- list.files(tmp_dir, pattern = "\\.json$")
  expect_length(json_files, 1L)
})

# upload_workflows ---------------------------------------------------------

write_board_json <- function(path, id) {
  board <- blockr.core::new_board()
  ser <- blockr.core::serialize_board(
    board, blockr.core::board_blocks(board), id = id
  )
  jsonlite::write_json(ser, path, auto_unbox = TRUE, null = "null")
}

test_that("upload_workflows keys a single file on its own board id", {

  backend <- pins::board_temp(versioned = TRUE)

  tmp <- withr::local_tempfile(fileext = ".json")
  write_board_json(tmp, "single-board-id")

  file_info <- data.frame(
    name = "my_workflow.json",
    size = file.size(tmp),
    type = "application/json",
    datapath = tmp,
    stringsAsFactors = FALSE
  )

  result <- upload_workflows(file_info, backend)

  expect_true(result$ok)
  expect_equal(result$uploaded, 1L)
  expect_length(result$errors, 0L)

  boards <- rack_list(backend)
  expect_length(boards, 1L)
  expect_equal(boards[[1L]]$id, "single-board-id")
  expect_equal(boards[[1L]]$name, "my_workflow")
})

test_that("upload_workflows handles multiple files", {

  backend <- pins::board_temp(versioned = TRUE)

  tmp1 <- withr::local_tempfile(fileext = ".json")
  tmp2 <- withr::local_tempfile(fileext = ".json")
  write_board_json(tmp1, "wf-one-id")
  write_board_json(tmp2, "wf-two-id")

  file_info <- data.frame(
    name = c("wf_one.json", "wf_two.json"),
    size = c(file.size(tmp1), file.size(tmp2)),
    type = c("application/json", "application/json"),
    datapath = c(tmp1, tmp2),
    stringsAsFactors = FALSE
  )

  result <- upload_workflows(file_info, backend)

  expect_true(result$ok)
  expect_equal(result$uploaded, 2L)

  boards <- rack_list(backend)
  names <- chr_xtr(boards, "name")
  expect_true("wf_one" %in% names)
  expect_true("wf_two" %in% names)
})

test_that("upload_workflows skips a board payload without an id", {

  backend <- pins::board_temp(versioned = TRUE)

  tmp <- withr::local_tempfile(fileext = ".json")
  ser <- blockr.core::blockr_ser(blockr.core::new_board())
  jsonlite::write_json(ser, tmp, auto_unbox = TRUE, null = "null")

  file_info <- data.frame(
    name = "no_id.json",
    size = file.size(tmp),
    type = "application/json",
    datapath = tmp,
    stringsAsFactors = FALSE
  )

  result <- upload_workflows(file_info, backend)

  expect_false(result$ok)
  expect_equal(result$uploaded, 0L)
  expect_match(result$errors, "no board id")
})

test_that("upload_workflows skips non-JSON files", {

  backend <- pins::board_temp(versioned = TRUE)

  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines("a,b\n1,2", tmp)

  file_info <- data.frame(
    name = "data.csv",
    size = file.size(tmp),
    type = "text/csv",
    datapath = tmp,
    stringsAsFactors = FALSE
  )

  result <- upload_workflows(file_info, backend)

  expect_false(result$ok)
  expect_equal(result$uploaded, 0L)
  expect_length(result$errors, 1L)
  expect_match(result$errors, "non-JSON")
})

test_that("upload_workflows rejects invalid JSON", {

  backend <- pins::board_temp(versioned = TRUE)

  tmp <- withr::local_tempfile(fileext = ".json")
  writeLines("not valid json {{{", tmp)

  file_info <- data.frame(
    name = "broken.json",
    size = file.size(tmp),
    type = "application/json",
    datapath = tmp,
    stringsAsFactors = FALSE
  )

  result <- upload_workflows(file_info, backend)
  expect_false(result$ok)
  expect_equal(result$uploaded, 0L)
  expect_length(result$errors, 1L)
  expect_match(result$errors, "not a valid board")
})

test_that("upload_workflows rejects valid JSON that is not a board", {

  backend <- pins::board_temp(versioned = TRUE)

  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(x = 1), tmp)

  file_info <- data.frame(
    name = "not_a_board.json",
    size = file.size(tmp),
    type = "application/json",
    datapath = tmp,
    stringsAsFactors = FALSE
  )

  result <- upload_workflows(file_info, backend)
  expect_false(result$ok)
  expect_equal(result$uploaded, 0L)
  expect_match(result$errors, "not a valid board")
})
