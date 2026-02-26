# rack_id_pins -----------------------------------------------------------------

rack_id_pins <- function(name, version = NULL, created = NULL) {
  structure(
    list(name = name, version = version, created = created),
    class = c("rack_id_pins", "rack_id")
  )
}

# Accessor generics ------------------------------------------------------------

display_name <- function(id, ...) UseMethod("display_name")

#' @export
display_name.rack_id_pins <- function(id, ...) id$name

last_saved <- function(id, ...) UseMethod("last_saved")

#' @export
last_saved.rack_id_pins <- function(id, ...) id$created

#' @export
format.rack_id_pins <- function(x, ...) {
  v <- if (!is.null(x$version)) paste0("@", x$version) else ""
  paste0("<rack_id_pins: ", x$name, v, ">")
}

#' @export
print.rack_id_pins <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

# rack_list --------------------------------------------------------------------

rack_list <- function(backend, ...) UseMethod("rack_list")

#' @export
rack_list.pins_board <- function(backend, ...) {

  df <- pins::pin_search(backend)

  if (nrow(df) == 0L) {
    return(list())
  }

  # Filter by blockr-session tags when the column is available
  if ("tags" %in% colnames(df)) {
    tags <- blockr_session_tags()
    keep <- vapply(df$tags, function(t) all(tags %in% t), logical(1))
    df <- df[keep, , drop = FALSE]
  }

  if (nrow(df) == 0L) {
    return(list())
  }

  df <- df[order(df$created, decreasing = TRUE, na.last = TRUE), ]

  lapply(seq_len(nrow(df)), function(i) {
    rack_id_pins(name = df$name[i], created = df$created[i])
  })
}

# rack_info --------------------------------------------------------------------

rack_info <- function(id, backend, ...) UseMethod("rack_info")

#' @export
rack_info.rack_id_pins <- function(id, backend, ...) {

  versions <- tryCatch(
    pins::pin_versions(backend, id$name),
    error = function(e) {
      blockr_warn(
        "Could not retrieve versions for {id$name}: {conditionMessage(e)}",
        class = "rack_info_failed"
      )
      NULL
    }
  )

  if (is.null(versions) || nrow(versions) == 0L) {
    return(data.frame(
      version = character(),
      created = as.POSIXct(character()),
      hash = character(),
      stringsAsFactors = FALSE
    ))
  }

  versions <- versions[order(versions$created, decreasing = TRUE), ]

  if (!"hash" %in% colnames(versions)) {
    versions$hash <- NA_character_
  }

  rownames(versions) <- NULL
  versions[, c("version", "created", "hash"), drop = FALSE]
}

# rack_load --------------------------------------------------------------------

rack_load <- function(id, backend, ...) UseMethod("rack_load")

#' @export
rack_load.rack_id_pins <- function(id, backend, ...) {

  version <- id$version

  if (is.null(version)) {
    versions <- pins::pin_versions(backend, id$name)

    if (nrow(versions) == 0L) {
      blockr_abort(
        "No versions found for pin {id$name}.",
        class = "rack_load_no_versions"
      )
    }

    versions <- versions[order(versions$created, decreasing = TRUE), ]
    version <- versions$version[1L]
  }

  meta <- pins::pin_meta(backend, id$name, version)

  if (!has_tags(meta)) {
    blockr_warn(
      "Pin {id$name} is not compatible with blockr (missing session tags).",
      class = "rack_load_invalid_tags"
    )
    return(NULL)
  }

  dat <- pins::pin_download(backend, id$name, version, meta$pin_hash)

  switch(
    meta$user$format,
    v1 = jsonlite::fromJSON(
      dat,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    ),
    blockr_abort(
      "Unrecognized file format {meta$user$format}.",
      class = "unknown_file_format"
    )
  )
}

# rack_save --------------------------------------------------------------------

rack_save <- function(backend, data, ...) UseMethod("rack_save")

#' @export
rack_save.pins_board <- function(backend, data, ..., name) {

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))

  jsonlite::write_json(data, tmp, null = "null")

  pins::pin_upload(
    backend,
    tmp,
    name,
    versioned = TRUE,
    metadata = list(format = "v1"),
    tags = blockr_session_tags()
  )

  versions <- pins::pin_versions(backend, name)
  versions <- versions[order(versions$created, decreasing = TRUE), ]

  rack_id_pins(
    name = name,
    version = versions$version[1L],
    created = versions$created[1L]
  )
}

# rack_delete ------------------------------------------------------------------

rack_delete <- function(id, backend, ...) UseMethod("rack_delete")

#' @export
rack_delete.rack_id_pins <- function(id, backend, ...) {
  if (!is.null(id$version)) {
    pins::pin_version_delete(backend, id$name, id$version)
  } else {
    pins::pin_delete(backend, id$name)
  }
  invisible(TRUE)
}
