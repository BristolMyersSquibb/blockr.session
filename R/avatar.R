#' Navbar avatar
#'
#' An item for the navbar of a blockr.dock board that shows the initials of the
#' user signed in to the app, from `session$user`. A server that signs its
#' visitors in, such as Posit Connect, sets it, and for a visitor who is not
#' signed in the item draws nothing. Appended with
#' [blockr.dock::custom_navbar()], the avatar sits last on the bar.
#'
#' @return A [blockr.dock::navbar_item()] with id `"avatar"`.
#'
#' @examplesIf requireNamespace("blockr.dock", quietly = TRUE)
#' blockr.dock::is_navbar_item(avatar_navbar_item())
#'
#' if (interactive()) {
#'   blockr.core::serve(
#'     blockr.dock::new_dock_board(),
#'     plugins = blockr.core::custom_plugins(manage_project()),
#'     navbar = blockr.dock::custom_navbar(avatar_navbar_item())
#'   )
#' }
#'
#' @export
avatar_navbar_item <- function() {

  rlang::check_installed("blockr.dock")

  blockr.dock::navbar_item("avatar", avatar_navbar_ui, avatar_navbar_server)
}

avatar_navbar_ui <- function(id, board) {
  tagList(
    htmltools::htmlDependency(
      "avatar-navbar",
      as.character(utils::packageVersion("blockr.session")),
      src = system.file("assets", package = "blockr.session"),
      stylesheet = "css/avatar-navbar.css"
    ),
    uiOutput(NS(id, "initials"), inline = TRUE)
  )
}

avatar_navbar_server <- function(id, board) {
  moduleServer(
    id,
    function(input, output, session) {
      output$initials <- renderUI(
        if (not_null(session$user)) {
          tags$div(class = "blockr-navbar-avatar", get_initials(session$user))
        }
      )
    }
  )
}

get_initials <- function(username) {

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
