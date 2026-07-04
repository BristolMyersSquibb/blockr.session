# Workflow index -------------------------------------------------------------
#
# A single pin caches the {id, name, created} of every workflow on a file-based
# pins board, so rack_list() reads one file instead of scanning each pin's
# version directories (an O(pins x versions) walk). The index is a *derived
# cache*: the pins are the source of truth, mutations patch a single row, and
# rebuild_index() re-derives the whole thing by a full scan whenever the cached
# file is missing or fails validation. It is never authoritative for deletion,
# so a stale or corrupt index can only briefly hide a workflow, never lose one.
#
# Connect boards keep their own rack_list() (they list via the Connect API), so
# the index is scoped to folder/file boards via board_uses_index().

index_pin_name <- function() "blockr-workflow-index"

# Kept deliberately OUT of the session tag set: pins_session_search() only keeps
# blockr-session-tagged pins, so the index never surfaces as a workflow.
index_pin_tags <- function() "blockr-session-index"

index_schema_version <- function() 1L

board_uses_index <- function(backend) {
  inherits(backend, "pins_board") && !inherits(backend, "pins_board_connect")
}

# Upper bound on retained versions per workflow, pruned on every save so a
# single pin's version directory (and its pin_versions() listing) stays small.
max_versions <- function() {
  as.integer(blockr_option("session_max_versions", 20L))
}

empty_records <- function() {
  data.frame(
    id = character(),
    name = character(),
    created = as.POSIXct(character()),
    stringsAsFactors = FALSE
  )
}

sort_records <- function(records) {
  if (nrow(records) <= 1L) {
    return(records)
  }
  ord <- order(records$created, decreasing = TRUE, na.last = TRUE)
  records[ord, , drop = FALSE]
}

valid_index <- function(obj) {

  if (is.null(obj) || !is.list(obj)) {
    return(FALSE)
  }

  if (!identical(obj$schema_version, index_schema_version())) {
    return(FALSE)
  }

  df <- obj$records

  if (!is.data.frame(df) || !all(c("id", "name", "created") %in% names(df))) {
    return(FALSE)
  }

  if (nrow(df) && (!is.character(df$id) || !is.character(df$name))) {
    return(FALSE)
  }

  TRUE
}

# Returns the cached records data frame, or NULL if the index pin is missing or
# fails validation (either way the caller rebuilds).
read_index <- function(backend) {

  obj <- tryCatch(
    pins::pin_read(backend, index_pin_name()),
    error = function(e) NULL
  )

  if (!valid_index(obj)) {
    return(NULL)
  }

  obj$records
}

write_index <- function(backend, records) {

  obj <- list(
    schema_version = index_schema_version(),
    records = records
  )

  # versioned = FALSE: the index is a live snapshot, not history, so it never
  # accumulates the version bloat it exists to avoid.
  suppressMessages(
    pins::pin_write(
      backend,
      obj,
      name = index_pin_name(),
      type = "rds",
      versioned = FALSE,
      tags = index_pin_tags()
    )
  )

  invisible(records)
}

# Full scan fallback: derive the record set directly from the board. This is the
# old rack_list() cost, paid only on a cache miss / corruption / first run.
scan_records <- function(backend, tags = NULL) {

  df <- pins_session_search(backend, tags)

  if (nrow(df) == 0L) {
    return(empty_records())
  }

  # pins_session_search() already drops the index pin (different tag); guard by
  # name too in case tag filtering ever changes.
  df <- df[df$name != index_pin_name(), , drop = FALSE]

  if (nrow(df) == 0L) {
    return(empty_records())
  }

  data.frame(
    id = df$name,
    name = vapply(
      seq_len(nrow(df)),
      function(i) {
        coal(df$meta[[i]]$user$name, df$name[i], fail_all = FALSE)
      },
      character(1L)
    ),
    created = as.POSIXct(df$created),
    stringsAsFactors = FALSE
  )
}

rebuild_index <- function(backend) {

  records <- sort_records(scan_records(backend))

  tryCatch(
    write_index(backend, records),
    error = function(e) {
      blockr_warn(
        "Could not persist workflow index: {conditionMessage(e)}",
        class = "index_write_failed"
      )
    }
  )

  records
}

# Read-or-rebuild entry point used by rack_list().
index_records <- function(backend) {

  records <- read_index(backend)

  if (is.null(records)) {
    log_debug("Workflow index missing or invalid; rebuilding from scan")
    records <- rebuild_index(backend)
  }

  records
}

records_to_list <- function(records) {

  if (nrow(records) == 0L) {
    return(list())
  }

  lapply(
    seq_len(nrow(records)),
    function(i) {
      new_rack_record(
        id = records$id[i],
        name = records$name[i],
        created = records$created[i]
      )
    }
  )
}

# Insert or update one row. A missing cache triggers a full rebuild (which picks
# up this workflow anyway); all writes are best-effort so index maintenance can
# never break the underlying save.
index_upsert <- function(backend, id, name, created) {

  if (!board_uses_index(backend)) {
    return(invisible())
  }

  records <- read_index(backend)

  if (is.null(records)) {
    rebuild_index(backend)
    return(invisible())
  }

  row <- data.frame(
    id = id,
    name = coal(name, id, fail_all = FALSE),
    created = as.POSIXct(created),
    stringsAsFactors = FALSE
  )

  updated <- sort_records(
    rbind(records[records$id != id, , drop = FALSE], row)
  )

  tryCatch(
    write_index(backend, updated),
    error = function(e) {
      blockr_warn(
        "Could not update workflow index: {conditionMessage(e)}",
        class = "index_write_failed"
      )
    }
  )

  invisible()
}

index_remove <- function(backend, id) {

  if (!board_uses_index(backend)) {
    return(invisible())
  }

  records <- read_index(backend)

  if (is.null(records)) {
    # nothing cached; the next read rebuilds from scan
    return(invisible())
  }

  updated <- records[records$id != id, , drop = FALSE]

  tryCatch(
    write_index(backend, updated),
    error = function(e) {
      blockr_warn(
        "Could not update workflow index: {conditionMessage(e)}",
        class = "index_write_failed"
      )
    }
  )

  invisible()
}
