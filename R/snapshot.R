#' @rdname rack-backend
#' @export
rack_snapshot <- function(backend, id, data, ...) UseMethod("rack_snapshot")

#' @rdname rack-backend
#' @export
rack_snapshot_list <- function(backend, ...) UseMethod("rack_snapshot_list")

snapshot_tags <- function() "blockr-session-snapshot"

snapshot_prefix <- function() "blockr-snapshot-"

snapshot_pin_name <- function(key, pool) {
  paste0(snapshot_prefix(), if (identical(pool, "rack")) "r-" else "s-", key)
}

is_snapshot_name <- function(x) {
  startsWith(sub("^.*/", "", x), snapshot_prefix())
}

snapshot_pool <- function(name) {
  if (startsWith(sub("^.*/", "", name), paste0(snapshot_prefix(), "r-"))) {
    "rack"
  } else {
    "session"
  }
}

#' @export
rack_snapshot.pins_board <- function(backend, id, data, ..., meta = list()) {

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))

  jsonlite::write_json(data, tmp, null = "null")

  log_debug("Snapshot upload target {pin_name(id)}")

  pins::pin_upload(
    backend,
    tmp,
    pin_name(id),
    title = snapshot_title(meta),
    versioned = FALSE,
    metadata = c(list(content_hash = content_hash(data)), meta),
    tags = snapshot_tags()
  )

  invisible(id)
}

snapshot_title <- function(meta) {
  paste0(
    "blockr snapshot: ",
    coal(meta[["board_name"]], meta[["board_id"]], "unsaved", fail_all = FALSE)
  )
}

#' @export
rack_snapshot_list.pins_board <- function(backend, ...) {

  nms <- tryCatch(pins::pin_list(backend), error = function(e) character())

  snapshot_records(backend, nms[is_snapshot_name(nms)])
}

snapshot_records <- function(backend, names) {
  Filter(not_null, lapply(names, snapshot_record, backend))
}

snapshot_record <- function(name, backend) {

  id <- as_snapshot_id(name, backend)

  meta <- tryCatch(
    pins::pin_meta(backend, pin_name(id)),
    error = function(e) NULL
  )

  if (is.null(meta)) {
    return(NULL)
  }

  usr <- coal(meta$user, list(), fail_all = FALSE)

  new_rack_record(
    id = id$id,
    name = coal(usr$board_name, id$id, fail_all = FALSE),
    pool = snapshot_pool(id$id),
    board_id = coal(usr$board_id, "", fail_all = FALSE),
    rack_id = coal(usr$rack_id, "", fail_all = FALSE),
    content_hash = coal(usr$content_hash, "", fail_all = FALSE),
    ended = parse_stamp(usr$ended_at),
    saved = as.POSIXct(meta$created),
    class = "snapshot_record"
  )
}

as_snapshot_id <- function(name, backend) {
  as_rack_id(list(id = sub("^.*/", "", name)), backend)
}

parse_stamp <- function(x) {

  if (is.null(x) || !is_string(x) || !nzchar(x)) {
    return(NULL)
  }

  tryCatch(
    as.POSIXct(x, tz = "UTC"),
    error = function(e) NULL
  )
}

format_stamp <- function(x) {
  format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

snapshot_interval <- function() {

  val <- blockr_option("session_autosave", NULL)

  if (is.null(val)) {
    return(NULL)
  }

  val <- suppressWarnings(as.numeric(val))

  if (is.na(val) || val <= 0) {
    return(NULL)
  }

  val
}

snapshot_ttl_days <- function() {

  val <- suppressWarnings(
    as.numeric(blockr_option("session_snapshot_ttl", 7))
  )

  if (is.na(val) || val <= 0) 7 else val
}

# A session that ends cleanly is a weak signal that the user is done: it also
# fires on a plain reload and on a dropped connection, so it stamps rather than
# deletes and the sweep collects only once the grace has passed.
snapshot_grace_secs <- function() 15 * 60

snapshot_capable <- function(backend) {

  caps <- tryCatch(rack_capabilities(backend), error = function(e) list())

  isTRUE(caps[["snapshot"]])
}

snapshot_enabled <- function(backend) {
  not_null(snapshot_interval()) && snapshot_capable(backend)
}

snapshot_writer <- function(board, backend, current_id, saved, serialize_now,
                            current_query, session = get_session()) {

  if (!snapshot_enabled(backend)) {
    return(invisible(NULL))
  }

  interval <- snapshot_interval()

  state <- new.env(parent = emptyenv())
  state$key <- rand_names()
  state$held <- NULL
  state$slot <- NULL
  state$data <- NULL
  state$meta <- NULL

  observeEvent(
    saved(),
    {
      drop_session_slot(state, backend)
      state$held <- snapshot_hash(serialize_now)
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  observe(
    {
      invalidateLater(interval * 1000, session)

      isolate(
        snapshot_tick(
          state, board, backend, current_id(), serialize_now, current_query()
        )
      )
    }
  )

  session$onSessionEnded(function() stamp_session_end(state, backend))

  invisible(state)
}

snapshot_hash <- function(serialize_now) {
  tryCatch(content_hash(serialize_now()), error = function(e) NULL)
}

# The first tick runs at the session's first flush, before the user can have
# touched anything, so it is what establishes the baseline: whatever was loaded
# is what the record already holds, and only a later divergence is unsaved.
snapshot_tick <- function(state, board, backend, rid, serialize_now, query) {

  if (is.null(state$held)) {

    state$held <- snapshot_hash(serialize_now)

    return(adopt_snapshot(state, backend, recovery_key(query)))
  }

  take_snapshot(state, board, backend, rid, serialize_now)
}

# A recovered session takes over the slot it was offered rather than minting a
# fresh key and moving the content: the `recover` handle stays in the URL, so
# adopting keeps a reload of that URL resolvable, where a move would strand it
# on a purged snapshot until the next tick wrote the new slot.
adopt_snapshot <- function(state, backend, key) {

  if (is.null(key) || !identical(snapshot_pool(key), "session")) {
    return(invisible(NULL))
  }

  state$key <- sub(paste0("^", snapshot_prefix(), "s-"), "", key)
  state$slot <- as_snapshot_id(key, backend)

  invisible(NULL)
}

load_snapshot <- function(key, backend) {

  payload <- tryCatch(
    rack_load(as_snapshot_id(key, backend), backend),
    error = function(e) NULL
  )

  if (is.null(payload)) {
    return(NULL)
  }

  tryCatch(blockr_deser(payload), error = function(e) NULL)
}

recovery_key <- function(query) {

  key <- parseQueryString(coal(query, ""))[["recover"]]

  if (is.null(key) || !nzchar(key)) {
    return(NULL)
  }

  key
}

take_snapshot <- function(state, board, backend, rid, serialize_now) {

  if (!has_content(board$board)) {
    return(invisible(NULL))
  }

  data <- tryCatch(serialize_now(), error = function(e) NULL)

  if (is.null(data)) {
    return(invisible(NULL))
  }

  hash <- content_hash(data)

  if (identical(hash, state$held)) {
    return(invisible(NULL))
  }

  pool <- if (is.null(rid)) "session" else "rack"
  key <- if (is.null(rid)) state$key else rid$id
  slot <- as_snapshot_id(snapshot_pin_name(key, pool), backend)

  meta <- list(
    pool = pool,
    board_id = coal(board$board_id, "", fail_all = FALSE),
    board_name = snapshot_board_name(board),
    rack_id = if (is.null(rid)) "" else rid$id
  )

  ok <- tryCatch(
    {
      rack_snapshot(backend, slot, data, meta = meta)
      TRUE
    },
    error = function(e) {
      log_debug("Snapshot write failed: {conditionMessage(e)}")
      FALSE
    }
  )

  if (!ok) {
    return(invisible(NULL))
  }

  if (identical(pool, "rack")) {
    drop_session_slot(state, backend)
  }

  state$held <- hash
  state$slot <- slot
  state$data <- data
  state$meta <- meta

  invisible(slot)
}

has_content <- function(board) {
  has_length(board_block_ids(board))
}

snapshot_board_name <- function(board) {
  coal(
    get_board_option_or_null("board_name"),
    board$board_id,
    "",
    fail_all = FALSE
  )
}

drop_session_slot <- function(state, backend) {

  if (is.null(state$slot)) {
    return(invisible(NULL))
  }

  if (!identical(snapshot_pool(state$slot$id), "session")) {
    return(invisible(NULL))
  }

  discard_snapshot(state$slot, backend)

  state$slot <- NULL
  state$data <- NULL
  state$meta <- NULL

  invisible(NULL)
}

# The pins package carries no metadata-only edit, so the end stamp re-uploads
# the payload the loop last wrote, which is held in memory for exactly this.
stamp_session_end <- function(state, backend) {

  if (is.null(state$slot) || is.null(state$data)) {
    return(invisible(NULL))
  }

  meta <- c(state$meta, list(ended_at = format_stamp(Sys.time())))

  tryCatch(
    rack_snapshot(backend, state$slot, state$data, meta = meta),
    error = function(e) log_debug("End stamp failed: {conditionMessage(e)}")
  )

  invisible(NULL)
}

discard_snapshot <- function(id, backend) {
  tryCatch(
    rack_purge(id, backend),
    error = function(e) {
      log_debug("Snapshot purge failed: {conditionMessage(e)}")
    }
  )
}

snapshot_sweep <- function(backend) {

  if (!snapshot_capable(backend)) {
    return(list())
  }

  records <- tryCatch(rack_snapshot_list(backend), error = function(e) list())

  keep <- !lgl_ply(records, snapshot_expired)

  for (rec in records[!keep]) {
    discard_snapshot(as_snapshot_id(rec$id, backend), backend)
  }

  records[keep]
}

snapshot_expired <- function(rec) {

  now <- Sys.time()

  if (not_null(rec$ended)) {

    ended <- as.numeric(difftime(now, rec$ended, units = "secs"))

    if (isTRUE(ended > snapshot_grace_secs())) {
      return(TRUE)
    }
  }

  age <- as.numeric(difftime(now, rec$saved, units = "days"))

  isTRUE(age > snapshot_ttl_days())
}

snapshot_offers <- function(records, rid, backend, skip = NULL) {

  keep <- lgl_ply(records, snapshot_offerable, rid, backend, skip)

  records[keep]
}

snapshot_recovery <- function(input, output, backend, current_id,
                              current_query, session = get_session()) {

  # nothing parks snapshots while autosave is off, so a deployment that never
  # turns it on pays no listing on load either
  if (!snapshot_enabled(backend)) {
    return(invisible(NULL))
  }

  offers <- reactiveVal(NULL)

  observeEvent(
    TRUE,
    offers(
      snapshot_offers(
        snapshot_sweep(backend),
        current_id(),
        backend,
        recovery_key(current_query())
      )
    ),
    once = TRUE
  )

  observeEvent(
    offers(),
    if (length(offers())) showModal(recovery_modal(session$ns), session),
    once = TRUE
  )

  output$snapshot_offers <- renderUI(
    tags$table(
      class = "blockr-workflow-table",
      tags$tbody(lapply(offers(), snapshot_offer_row, session$ns))
    )
  )

  observeEvent(
    input$snapshot_discard,
    {
      rec <- offer_by_id(offers(), input$snapshot_discard)

      if (not_null(rec)) {
        discard_snapshot(as_snapshot_id(rec$id, backend), backend)
      }

      rest <- drop_offer(offers(), input$snapshot_discard)
      offers(rest)

      if (!length(rest)) {
        removeModal(session)
      }
    }
  )

  observeEvent(
    input$snapshot_restore,
    {
      rec <- offer_by_id(offers(), input$snapshot_restore)

      if (is.null(rec)) {
        return()
      }

      removeModal(session)

      reload_with_query(recovery_query(rec, current_query()), session)
    }
  )

  invisible(NULL)
}

offer_by_id <- function(offers, id) {

  hit <- match(id, chr_xtr(offers, "id"))

  if (is.na(hit)) NULL else offers[[hit]]
}

drop_offer <- function(offers, id) {
  offers[!chr_xtr(offers, "id") %in% id]
}

recovery_query <- function(rec, keep) {

  params <- list(recover = rec$id)

  if (identical(rec$pool, "rack") && nzchar(rec$rack_id)) {
    params$id <- rec$rack_id
  }

  build_query_string(c(params, drop_session_query(keep)))
}

recovery_modal <- function(ns) {
  modalDialog(
    title = "Unsaved work found",
    tags$p(
      class = "blockr-workflow-empty",
      "These boards hold changes that were never saved. Restoring reloads ",
      "one of them; anything left alone stays available."
    ),
    uiOutput(ns("snapshot_offers")),
    footer = modalButton("Close"),
    easyClose = TRUE
  )
}

snapshot_offer_row <- function(rec, ns) {
  tags$tr(
    class = "blockr-workflow-row",
    tags$td(class = "blockr-wf-name", rec$name),
    tags$td(class = "blockr-wf-time", record_time_ago(rec)),
    tags$td(
      class = "blockr-wf-action",
      tags$button(
        type = "button",
        class = "btn btn-sm btn-primary",
        onclick = shiny_input_js(ns("snapshot_restore"), rec$id),
        "Restore"
      ),
      tags$button(
        type = "button",
        class = "btn btn-sm",
        onclick = shiny_input_js(ns("snapshot_discard"), rec$id),
        "Discard"
      )
    )
  )
}

snapshot_offerable <- function(rec, rid, backend, skip) {

  if (identical(rec$id, skip)) {
    return(FALSE)
  }

  if (identical(rec$pool, "session")) {
    return(TRUE)
  }

  if (is.null(rid) || !identical(rec$rack_id, rid$id)) {
    return(FALSE)
  }

  !identical(
    rec$content_hash,
    tryCatch(rack_content_hash(rid, backend), error = function(e) NULL)
  )
}
