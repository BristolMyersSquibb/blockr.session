#' Session navbar right slot server
#'
#' Renders user avatar with initials.
#'
#' @param id Namespace ID
#' @param board Reactive values object (unused, for interface consistency)
#' @param dock Dock object (unused, for interface consistency)
#' @param session Shiny session
#'
#' @export
session_navbar_right_server <- function(id, board = NULL, dock = NULL,
                                        session = shiny::getDefaultReactiveDomain()) {

  moduleServer(id, function(input, output, session) {

    output$user_avatar <- renderUI({
      username <- coal(
        session$user,
        Sys.getenv("USER"),
        Sys.getenv("USERNAME"),
        "User"
      )

      initials <- get_initials(username)
      user_avatar_ui(initials)
    })
  })
}

#' Get initials from username
#' @param username Character string
#' @return 1-2 character initials
#' @keywords internal
get_initials <- function(username) {
  if (is.null(username) || username == "") return("U")

  parts <- strsplit(username, "[._@ -]")[[1]]
  parts <- parts[parts != ""]

  if (length(parts) >= 2) {
    paste0(toupper(substr(parts[1], 1, 1)), toupper(substr(parts[2], 1, 1)))
  } else {
    toupper(substr(username, 1, min(2, nchar(username))))
  }
}
