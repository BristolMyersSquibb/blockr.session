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

snapshot_enabled <- function() {
  not_null(snapshot_interval())
}

snapshot_writer <- function(board, backend, current_id, save_event,
                            serialize_now, current_query, status,
                            session = get_session()) {

  if (!snapshot_enabled()) {
    return(invisible(NULL))
  }

  interval <- snapshot_interval()

  state <- new.env(parent = emptyenv())
  state$key <- rand_names()
  state$held <- NULL
  state$slot <- NULL
  state$status <- status

  observeEvent(
    save_event(),
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

    return(adopt_draft(state, backend, recovery_key(query)))
  }

  take_snapshot(state, board, backend, rid, serialize_now)
}

# A recovered session takes over the draft it was offered rather than minting a
# fresh key and moving the content: the `recover` handle stays in the URL, so
# adopting keeps a reload of that URL resolvable, where a move would strand it
# on a purged draft until the next tick wrote the new one.
adopt_draft <- function(state, backend, key) {

  if (is.null(key) || !is_draft_record(key, "session")) {
    return(invisible(NULL))
  }

  state$key <- draft_record_key(key)
  state$slot <- as_rack_id(list(id = key), backend)

  invisible(NULL)
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

  draft <- if (is.null(rid)) "session" else "record"
  key <- if (is.null(rid)) state$key else rid$id

  slot <- tryCatch(
    rack_create(
      backend,
      data,
      id = key,
      name = snapshot_board_name(board),
      draft = draft
    ),
    error = function(e) {
      log_debug("Draft write failed: {conditionMessage(e)}")
      NULL
    }
  )

  # The status text carries this rather than a notification: a tick that keeps
  # failing would otherwise raise one toast per interval. Setting a reactiveVal
  # to the value it already holds is a no-op, so the message lands once.
  set_snapshot_status(
    state, if (is.null(slot)) "Autosave failed" else "Saved as draft"
  )

  if (is.null(slot)) {
    return(invisible(NULL))
  }

  if (identical(draft, "record")) {
    drop_session_slot(state, backend)
  }

  state$held <- hash
  state$slot <- slot

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

set_snapshot_status <- function(state, text) {

  if (is.null(state$status)) {
    return(invisible(NULL))
  }

  state$status(text)

  invisible(NULL)
}

drop_session_slot <- function(state, backend) {

  if (is.null(state$slot)) {
    return(invisible(NULL))
  }

  if (!is_draft_record(state$slot$id, "session")) {
    return(invisible(NULL))
  }

  discard_draft(state$slot, backend)

  state$slot <- NULL

  invisible(NULL)
}

discard_draft <- function(id, backend) {
  tryCatch(
    rack_purge(id, backend),
    error = function(e) {
      log_debug("Draft purge failed: {conditionMessage(e)}")
    }
  )
}

load_snapshot <- function(key, backend) {

  payload <- tryCatch(
    rack_load(as_rack_id(list(id = key), backend), backend),
    error = function(e) NULL
  )

  if (is.null(payload)) {
    return(NULL)
  }

  tryCatch(blockr_deser(payload), error = function(e) NULL)
}

# A `recover` handle carrying a key restores that draft; the bare parameter
# opens the list instead, so the prompt is somewhere the user navigates to
# rather than a modal that greets every load until the drafts are cleared.
recovery_key <- function(query) {

  key <- parseQueryString(coal(query, ""))[["recover"]]

  if (is.null(key) || !nzchar(key)) {
    return(NULL)
  }

  key
}

recovery_menu_requested <- function(query) {

  parsed <- parseQueryString(coal(query, ""))

  "recover" %in% names(parsed) && !nzchar(coal(parsed$recover, ""))
}

snapshot_sweep <- function(backend) {

  records <- tryCatch(
    rack_records(backend, draft = TRUE),
    error = function(e) list()
  )

  keep <- !lgl_ply(records, snapshot_expired)

  for (rec in records[!keep]) {
    discard_draft(as_rack_id(rec, backend), backend)
  }

  records[keep]
}

snapshot_expired <- function(rec) {

  age <- as.numeric(difftime(Sys.time(), rec$saved, units = "days"))

  isTRUE(age > snapshot_ttl_days())
}

snapshot_offers <- function(records, rid, backend, skip = NULL) {

  keep <- lgl_ply(records, snapshot_offerable, rid, backend, skip)

  records[keep]
}

snapshot_offerable <- function(rec, rid, backend, skip) {

  if (identical(rec$id, skip)) {
    return(FALSE)
  }

  if (is_draft_record(rec$id, "session")) {
    return(TRUE)
  }

  if (is.null(rid) || !identical(draft_record_key(rec$id), rid$id)) {
    return(FALSE)
  }

  stored <- tryCatch(rack_content_hash(rid, backend), error = function(e) NULL)
  held <- tryCatch(
    rack_content_hash(as_rack_id(rec, backend), backend),
    error = function(e) NULL
  )

  not_null(held) && !identical(held, stored)
}

snapshot_recovery <- function(input, output, backend, current_id,
                              current_query, session = get_session()) {

  # Nothing parks drafts while autosave is off, so a deployment that never
  # turns it on pays no listing on load either
  if (!snapshot_enabled()) {
    return(invisible(NULL))
  }

  offers <- reactiveVal(NULL)

  observeEvent(
    TRUE,
    {
      found <- snapshot_offers(
        snapshot_sweep(backend),
        current_id(),
        backend,
        recovery_key(current_query())
      )

      offers(found)

      if (has_length(found) && recovery_menu_requested(current_query())) {
        showModal(recovery_modal(session$ns), session)
      }
    },
    once = TRUE
  )

  output$recovery_notice <- renderUI(
    recovery_notice(session$ns, offers())
  )

  observeEvent(
    input$snapshot_menu,
    showModal(recovery_modal(session$ns), session)
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
        discard_draft(as_rack_id(rec, backend), backend)
      }

      rest <- drop_offer(offers(), input$snapshot_discard)
      offers(rest)

      if (!has_length(rest)) {
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

  if (is_draft_record(rec$id, "record")) {
    params$id <- draft_record_key(rec$id)
  }

  build_query_string(c(params, drop_session_query(keep)))
}

recovery_notice <- function(ns, offers) {

  if (!has_length(offers)) {
    return(NULL)
  }

  tags$button(
    type = "button",
    class = "btn btn-sm blockr-snapshot-notice",
    title = "Unsaved work from an earlier session",
    onclick = shiny_input_js(ns("snapshot_menu"), "open"),
    bsicons::bs_icon("clock-history"),
    sprintf(" %d draft%s", length(offers), if (length(offers) > 1L) "s" else "")
  )
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
