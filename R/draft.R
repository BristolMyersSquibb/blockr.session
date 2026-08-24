draft_interval <- function() {

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

draft_ttl_days <- function() {

  val <- suppressWarnings(
    as.numeric(blockr_option("session_draft_ttl", 7))
  )

  if (is.na(val) || val <= 0) 7 else val
}

draft_enabled <- function() {
  not_null(draft_interval())
}

draft_writer <- function(board, backend, current_id, save_event,
                         serialize_now, current_query, status,
                         session = get_session()) {

  if (!draft_enabled()) {
    return(invisible(NULL))
  }

  interval <- draft_interval()

  state <- new.env(parent = emptyenv())
  state$key <- rand_names()
  state$held <- NULL
  state$slot <- NULL
  state$slot_key <- NULL
  state$slot_name <- NULL
  state$recovered <- NULL
  state$status <- status

  observeEvent(
    save_event(),
    {
      drop_slot(state, backend)
      state$held <- draft_hash(serialize_now)
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  observe(
    {
      invalidateLater(interval * 1000, session)

      isolate(
        draft_tick(
          state, board, backend, current_id(), serialize_now, current_query()
        )
      )
    }
  )

  invisible(state)
}

draft_hash <- function(serialize_now) {
  tryCatch(content_hash(serialize_now()), error = function(e) NULL)
}

# The first tick runs at the session's first flush, before the user can have
# touched anything, so it is what establishes the baseline: whatever was loaded
# is what the record already holds, and only a later divergence is unsaved.
draft_tick <- function(state, board, backend, rid, serialize_now, query) {

  if (is.null(state$held)) {

    state$held <- draft_hash(serialize_now)
    state$recovered <- recovery_key(query)

    return(invisible(NULL))
  }

  write_draft(state, board, backend, rid, serialize_now)
}

write_draft <- function(state, board, backend, rid, serialize_now) {

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

  kind <- if (is.null(rid)) "session" else "record"
  key <- if (is.null(rid)) state$key else rid$id

  name <- draft_board_name(board)

  slot <- tryCatch(
    rack_create(
      backend,
      data,
      id = draft_slot_id(state, kind, key),
      name = name,
      draft = kind
    ),
    error = function(e) {
      log_debug("Draft write failed: {conditionMessage(e)}")
      NULL
    }
  )

  # The status text carries this rather than a notification: a tick that keeps
  # failing would otherwise raise one toast per interval. Setting a reactiveVal
  # to the value it already holds is a no-op, so the message lands once.
  set_draft_status(
    state, if (is.null(slot)) "Autosave failed" else "Saved as draft"
  )

  if (is.null(slot)) {
    return(invisible(NULL))
  }

  state$held <- hash
  state$slot <- slot
  state$slot_key <- key
  state$slot_name <- name

  drop_recovered(state, backend)

  invisible(slot)
}

# A session that recovered a draft owns a fresh slot like any other, and the
# one it was handed is dropped only once that slot holds the work. Purging
# first would leave the `recover` handle in the URL pointing at nothing
# until the next tick, so a reload in that window lands on an empty board.
drop_recovered <- function(state, backend) {

  if (is.null(state$recovered)) {
    return(invisible(NULL))
  }

  discard_draft(as_rack_id(list(id = state$recovered), backend), backend)

  state$recovered <- NULL

  invisible(NULL)
}

# A record draft's id is a pure function of the workflow, so it needs no
# carrying forward. A session draft's carries a mint time, so re-minting each
# tick would strew a new record per tick instead of overwriting one slot.
draft_slot_id <- function(state, kind, key) {

  if (identical(kind, "record") || is.null(state$slot)) {
    return(key)
  }

  if (!is_draft_record(state$slot$id, "session")) {
    return(key)
  }

  state$slot$id
}

has_content <- function(board) {
  has_length(board_block_ids(board))
}

draft_board_name <- function(board) {
  coal(
    get_board_option_or_null("board_name"),
    board$board_id,
    "",
    fail_all = FALSE
  )
}

set_draft_status <- function(state, text) {
  state$status(text)
}

# A save supersedes whatever the session had parked, so the draft goes with it.
# Without that a record draft, which never expires, would sit holding content
# the record already has for as long as the workflow exists.
drop_slot <- function(state, backend) {

  if (is.null(state$slot)) {
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

load_draft <- function(key, backend) {

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

sweep_drafts <- function(backend, now = Sys.time()) {

  records <- tryCatch(
    rack_records(backend, draft = TRUE),
    error = function(e) list()
  )

  keep <- !lgl_ply(records, draft_expired, now)

  for (rec in records[!keep]) {
    discard_draft(as_rack_id(rec, backend), backend)
  }

  records[keep]
}

# Both clocks come off the id: `saved` is not part of `new_rack_record()` and a
# backend that omits it would otherwise expire nothing, silently.
draft_expired <- function(rec, now = Sys.time()) {

  if (identical(draft_record_kind(rec$id), "record")) {
    return(FALSE)
  }

  age <- as.numeric(now) - draft_record_epoch(rec$id)

  isTRUE(age > draft_ttl_days() * 86400)
}

draft_offers <- function(records, rid, backend, skip = NULL) {

  keep <- lgl_ply(records, draft_offerable, rid, backend, skip)

  records[keep]
}

draft_offerable <- function(rec, rid, backend, skip) {

  if (identical(rec$id, skip)) {
    return(FALSE)
  }

  if (is_draft_record(rec$id, "session")) {
    return(TRUE)
  }

  # A save drops the draft it shadowed, so one still standing for this record
  # is unsaved work by construction -- no content comparison needed
  not_null(rid) &&
    identical(draft_record_hash(rec$id), draft_key_hash(rid$id))
}

draft_recovery <- function(input, output, backend, current_id,
                           current_query, session = get_session()) {

  # Nothing parks drafts while autosave is off, so a deployment that never
  # turns it on pays no listing on load either
  if (!draft_enabled()) {
    return(invisible(NULL))
  }

  offers <- reactiveVal(NULL)

  observeEvent(
    TRUE,
    {
      found <- draft_offers(
        sweep_drafts(backend),
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
    input$draft_menu,
    showModal(recovery_modal(session$ns), session)
  )

  output$draft_offers <- renderUI(
    tags$table(
      class = "blockr-workflow-table",
      tags$tbody(lapply(offers(), draft_offer_row, session$ns))
    )
  )

  observeEvent(
    input$draft_discard,
    {
      rec <- offer_by_id(offers(), input$draft_discard)

      if (not_null(rec)) {
        discard_draft(as_rack_id(rec, backend), backend)
      }

      rest <- drop_offer(offers(), input$draft_discard)
      offers(rest)

      if (!has_length(rest)) {
        removeModal(session)
      }
    }
  )

  observeEvent(
    input$draft_restore,
    {
      rec <- offer_by_id(offers(), input$draft_restore)

      if (is.null(rec)) {
        return()
      }

      removeModal(session)

      reload_with_query(
        recovery_query(rec, current_query(), current_id()), session
      )
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

recovery_query <- function(rec, keep, rid = NULL) {

  params <- list(recover = rec$id)

  if (is_draft_record(rec$id, "record") && not_null(rid)) {
    params$id <- rid$id
  }

  build_query_string(c(params, drop_session_query(keep)))
}

recovery_notice <- function(ns, offers) {

  if (!has_length(offers)) {
    return(NULL)
  }

  tags$button(
    type = "button",
    class = "btn btn-sm blockr-draft-notice",
    title = "Unsaved work from an earlier session",
    onclick = shiny_input_js(ns("draft_menu"), "open"),
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
    uiOutput(ns("draft_offers")),
    footer = modalButton("Close"),
    easyClose = TRUE
  )
}

draft_offer_row <- function(rec, ns) {
  tags$tr(
    class = "blockr-workflow-row",
    tags$td(class = "blockr-wf-name", rec$name),
    tags$td(class = "blockr-wf-time", record_time_ago(rec)),
    tags$td(
      class = "blockr-wf-action",
      tags$button(
        type = "button",
        class = "btn btn-sm btn-primary",
        onclick = shiny_input_js(ns("draft_restore"), rec$id),
        "Restore"
      ),
      tags$button(
        type = "button",
        class = "btn btn-sm",
        onclick = shiny_input_js(ns("draft_discard"), rec$id),
        "Discard"
      )
    )
  )
}
