#' Session navbar right slot UI
#'
#' Creates the right side navbar content for blockr.dock integration:
#' - User avatar with initials
#'
#' @param id Namespace ID
#'
#' @return Shiny UI tagList
#' @export
session_navbar_right_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("user_avatar"), inline = TRUE)
  )
}

#' User avatar UI helper
#'
#' @param initials User initials (1-2 characters)
#' @return Shiny UI element
#' @keywords internal
user_avatar_ui <- function(initials = "U") {
  tags$div(
    class = "blockr-navbar-avatar",
    initials
  )
}
