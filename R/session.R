#' Session management
#'
#' Provides a [blockr.core::preserve_board()] using a [pins::board_local()] as
#' storage backend. The [blockr_option()] `session_mgmt_backend` can be set
#' to swap out this default pin board for another.
#'
#' @inheritParams blockr.core::preserve_board
#'
#' @return See [blockr.core::preserve_board()].
#'
#' @export
manage_session <- function(server = manage_session_server,
                           ui = manage_session_ui) {
  preserve_board(server, ui)
}

#' @param id Namespace ID
#' @param board Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @rdname manage_session
#' @export
manage_session_server <- function(id, board, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      backend <- blockr_option("session_mgmt_backend", pins::board_local())

      observeEvent(
        input$save,
        {
          json <- board_to_json(board$board, board$blocks, session)
          pins::pin_write(backend, json, board$board_id, type = "json",
                          versioned = TRUE)
        }
      )

      res <- reactiveVal()

      observeEvent(
        input$restore,
        {
          json <- tryCatch(
            pins::pin_read(backend, board$board_id),
            pins_pin_missing = function(e) {
              showNotification(
                HTML(cli::ansi_html(conditionMessage(e))),
                type = "warning",
                session = session
              )
              NULL
            }
          )

          if (not_null(json)) {
            res(from_json(json))
          }
        }
      )

      res
    }
  )
}

#' @param board The initial `board` object
#' @rdname manage_session
#' @export
manage_session_ui <- function(id, board) {
  tagList(
    actionButton(
      NS(id, "save"),
      "Save"
    ),
    actionButton(
      NS(id, "restore"),
      "Restore"
    )
  )
}

board_to_json <- function(board, blocks, session) {

  blocks <- lapply(
    lst_xtr(blocks, "server", "state"),
    lapply,
    reval_if
  )

  opts <- lapply(
    set_names(nm = names(as_board_options(board))),
    get_board_option_or_null,
    session
  )

  jsonlite::prettify(
    to_json(board, blocks = blocks, options = opts)
  )
}
