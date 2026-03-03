.blockr_reload_state <- new.env(parent = emptyenv())

get_and_clear_reload_url <- function() {
  url <- .blockr_reload_state$url
  .blockr_reload_state$url <- NULL
  url
}

set_reload_url <- function(url) {
  .blockr_reload_state$url <- url
}

get_session_backend <- function() {

  val <- blockr_option("session_mgmt_backend", pins::board_local)

  if (is.function(val)) {
    val <- val()
  }

  if (!inherits(val, "pins_board")) {
    blockr_abort(
      paste(
        "The `session_mgmt_backend` option must be a pins board or a",
        "function that returns one, got {class(val)[[1L]]}."
      ),
      class = "invalid_session_backend"
    )
  }

  val
}

format_time_ago <- function(time) {

  if (is.character(time)) {
    time <- as.POSIXct(time)
  }

  diff_secs <- as.numeric(difftime(Sys.time(), time, units = "secs"))

  if (diff_secs < 60) {
    "Just now"
  } else if (diff_secs < 3600) {
    mins <- floor(diff_secs / 60)
    paste0(mins, " min", if (mins > 1) "s" else "", " ago")
  } else if (diff_secs < 86400) {
    hours <- floor(diff_secs / 3600)
    paste0(hours, " hour", if (hours > 1) "s" else "", " ago")
  } else if (diff_secs < 604800) {
    days <- floor(diff_secs / 86400)
    paste0(days, " day", if (days > 1) "s" else "", " ago")
  } else if (diff_secs < 2592000) {
    weeks <- floor(diff_secs / 604800)
    paste0(weeks, " week", if (weeks > 1) "s" else "", " ago")
  } else {
    format(time, "%b %d, %Y")
  }
}

get_initials <- function(username) {

  if (is.null(username) || username == "") {
    return("U")
  }

  parts <- strsplit(username, "[._@ -]")[[1]]
  parts <- parts[parts != ""]

  if (length(parts) >= 2) {
    paste0(
      toupper(substr(parts[1], 1, 1)),
      toupper(substr(parts[2], 1, 1))
    )
  } else {
    toupper(substr(username, 1, min(2, nchar(username))))
  }
}

has_tags <- function(x, tags = blockr_session_tags()) {
  all(tags %in% x[["tags"]])
}

cnd_to_notif <- function(return_val = NULL, type = "warning",
                         session = get_session()) {

  function(cnd) {
    notify(
      HTML(cli::ansi_html(conditionMessage(cnd))),
      type = type,
      session = session
    )

    return_val
  }
}

blockr_session_tags <- function() "blockr-session"

# Normalize JS array-of-objects input from Shiny.setInputValue.
# Shiny may deliver as:
#   - list of lists (ideal) -> use as-is
#   - data.frame (multiple objects) -> split into list of rows
#   - named atomic vector (single object) -> wrap in list
#   - named atomic vector with repeated keys (multiple objects flattened)
#     e.g. c(name="a", user="", name="b", user="") -> split into list of objects
normalize_js_input <- function(x) {
  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, ])))
  }

  if (!is.atomic(x)) {
    return(x)
  }

  nms <- names(x)
  if (is.null(nms)) {
    return(list(as.list(x)))
  }

  unique_keys <- unique(nms)
  n_keys <- length(unique_keys)
  n_total <- length(x)

  if (n_keys > 0 && n_total > n_keys && n_total %% n_keys == 0) {
    n_objects <- n_total %/% n_keys
    return(lapply(seq_len(n_objects), function(i) {
      start <- (i - 1L) * n_keys + 1L
      end <- i * n_keys
      as.list(x[start:end])
    }))
  }

  list(as.list(x))
}

filter_traceback <- function(calls) {
  blockr_pkgs <- c("blockr.core", "blockr.dock", "blockr.session",
                    "blockr.extra", "blockr.dplyr", "blockr.md",
                    "blockr.ggplot", "blockr.io", "blockr")
  pattern <- paste0("^(", paste(blockr_pkgs, collapse = "|"), ")::")
  keep <- grep(pattern, calls)
  if (length(keep) == 0L) return(calls)
  # include from first blockr frame to end
  calls[seq(min(keep), length(calls))]
}

show_restore_error <- function(stage, name, version = NULL, error, traceback,
                               board_json = NULL,
                               session = shiny::getDefaultReactiveDomain()) {
  error_msg <- conditionMessage(error)
  tb_filtered <- filter_traceback(traceback)
  tb_text <- paste(tb_filtered, collapse = "\n")

  info <- paste0("Workflow: ", name)
  if (!is.null(version)) {
    info <- paste0(info, " (version: ", version, ")")
  }

  json_text <- if (!is.null(board_json)) {
    jsonlite::toJSON(board_json, null = "null", auto_unbox = TRUE, pretty = TRUE)
  }

  report_parts <- c(
    paste("Stage:", stage),
    info,
    paste("Error:", error_msg),
    "",
    "Traceback:",
    tb_text
  )
  if (!is.null(json_text)) {
    report_parts <- c(report_parts, "", "Workflow JSON:", json_text)
  }
  report <- paste(report_parts, collapse = "\n")

  message("--- Workflow restore error ---\n",
          paste(head(report_parts, 5), collapse = "\n"), "\n---")

  dl_id <- paste0("dl_error_", sample.int(1e6, 1))
  report_b64 <- gsub(
    "\n", "", base64enc::base64encode(charToRaw(enc2utf8(report)))
  )

  shiny::showNotification(
    shiny::tagList(
      shiny::tags$div(
        paste0("Failed to ", stage, " workflow: ", error_msg)
      ),
      shiny::tags$a(
        id = dl_id,
        href = paste0("data:text/plain;base64,", report_b64),
        download = paste0("error-report-", name, ".txt"),
        style = paste0(
          "color: #842029; border-color: #842029; ",
          "background: transparent; text-decoration: none;"
        ),
        class = "btn btn-sm mt-2",
        shiny::icon("download"),
        "Download error report"
      )
    ),
    type = "error",
    duration = NULL
  )
}

safe_restore_board <- function(board, board_ser, restore_result,
                               name, version = NULL, session) {
  tb <- NULL
  tryCatch(
    {
      withCallingHandlers(
        restore_board(board, board_ser, restore_result, session = session),
        error = function(e) {
          tb <<- format(sys.calls())
        }
      )
      TRUE
    },
    error = function(e) {
      show_restore_error("restore", name, version, e, tb,
                         board_json = board_ser, session = session)
      FALSE
    }
  )
}

board_query_string <- function(id, backend) {

  params <- list(board_name = display_name(id))

  if (not_null(id$user) && !identical(id$user, backend$account)) {
    params$user <- id$user
  }

  if (not_null(id$version)) {
    params$version <- id$version
  }

  paste0(
    "?",
    paste(names(params), params, sep = "=", collapse = "&")
  )
}
