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

test_that("record_time_ago formats the record's saved time", {

  saved <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  rec <- new_rack_record(id = "wf", name = "WF", saved = saved)

  expect_equal(record_time_ago(rec), format_time_ago(saved))
})

test_that("record_time_ago is empty when the record has no saved time", {
  expect_equal(record_time_ago(new_rack_record(id = "wf", name = "WF")), "")
})
