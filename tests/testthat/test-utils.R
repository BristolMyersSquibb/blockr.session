test_that("cnd_to_notif surfaces brace-bearing condition messages literally", {

  msg <- '{"code":5, "error":"server failed"}'

  session <- shiny:::MockShinySession$new()
  surfaced <- NULL
  session$sendNotification <- function(type, message) {
    surfaced <<- message
    invisible()
  }

  notif <- cnd_to_notif(return_val = FALSE, type = "warning", session = session)

  expect_identical(notif(simpleError(msg)), FALSE)
  expect_identical(as.character(surfaced$html), msg)
  expect_identical(surfaced$type, "warning")
})

test_that("record_time_ago queries the backend, not a stored field", {

  backend <- pins::board_temp(versioned = TRUE)
  rack_create(backend, list(blocks = list()), id = "timed", name = "Timed")

  rec <- new_rack_record(id = "timed", name = "Timed")

  expect_null(rec$created)
  expect_equal(record_time_ago(rec, backend), "Just now")
})
