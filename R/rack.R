# new_rack_id ---------------------------------------------------------------

new_rack_id <- function(name, ..., class = character()) {

  if (!is_string(name) || !nzchar(name)) {
    blockr_abort(
      "rack id name must be a non-empty string, got {class(name)[[1L]]}.",
      class = "rack_id_invalid_name"
    )
  }

  structure(list(name = name, ...), class = c(class, "rack_id"))
}

# Accessor generics ---------------------------------------------------------

display_name <- function(id, ...) UseMethod("display_name")

#' @export
display_name.rack_id <- function(id, ...) id$name

last_saved <- function(id, ...) UseMethod("last_saved")

pin_name <- function(id, ...) UseMethod("pin_name")

# Format / print ------------------------------------------------------------

#' @export
format.rack_id <- function(x, ...) {
  paste0("<rack_id: ", x$name, ">")
}

#' @export
print.rack_id <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

# rack_list -----------------------------------------------------------------

rack_list <- function(backend, ...) UseMethod("rack_list")

# rack_info -----------------------------------------------------------------

rack_info <- function(id, backend, ...) UseMethod("rack_info")

# rack_load -----------------------------------------------------------------

rack_load <- function(id, backend, ...) UseMethod("rack_load")

# rack_save -----------------------------------------------------------------

rack_save <- function(backend, data, ...) UseMethod("rack_save")

# rack_delete ---------------------------------------------------------------

rack_delete <- function(id, backend, ...) UseMethod("rack_delete")

rack_purge <- function(id, backend, ...) UseMethod("rack_purge")
