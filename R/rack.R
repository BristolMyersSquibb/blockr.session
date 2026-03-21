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

rack_list <- function(backend, tags = NULL, ...) UseMethod("rack_list")

# rack_info -----------------------------------------------------------------

rack_info <- function(id, backend, ...) UseMethod("rack_info")

# rack_load -----------------------------------------------------------------

rack_load <- function(id, backend, ...) UseMethod("rack_load")

# rack_save -----------------------------------------------------------------

rack_save <- function(backend, data, ...) UseMethod("rack_save")

# rack_delete ---------------------------------------------------------------

rack_delete <- function(id, backend, ...) UseMethod("rack_delete")

rack_purge <- function(id, backend, ...) UseMethod("rack_purge")

# rack_capabilities --------------------------------------------------------

rack_capabilities <- function(backend, ...) UseMethod("rack_capabilities")

# rack_tags ----------------------------------------------------------------

rack_tags <- function(id, backend, ...) UseMethod("rack_tags")

rack_set_tags <- function(id, backend, tags, ...) UseMethod("rack_set_tags")

# rack_acl -----------------------------------------------------------------

rack_acl <- function(id, backend, ...) UseMethod("rack_acl")

rack_set_acl <- function(id, backend, acl_type, ...) UseMethod("rack_set_acl")

# rack_share ---------------------------------------------------------------

rack_share <- function(id, backend, with_sub, ...) UseMethod("rack_share")

rack_unshare <- function(id, backend, with_sub, ...) UseMethod("rack_unshare")

rack_shares <- function(id, backend, ...) UseMethod("rack_shares")

# rack_find_users ----------------------------------------------------------

rack_find_users <- function(backend, query, ...) UseMethod("rack_find_users")
