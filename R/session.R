#' Session management
#'
#' Provides a [blockr.core::preserve_board()] using a [pins::board_local()] as
#' storage backend. The [blockr.core::blockr_option()] `session_mgmt_backend`
#' can be set to swap out this default pin board for another.
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

  dot_args <- list(...)

  moduleServer(
    id,
    function(input, output, session) {

      backend <- blockr_option("session_mgmt_backend", pins::board_local())

      observeEvent(
        input$save,
        {
          res <- tryCatch(
            pins::pin_write(
              backend,
              do.call(
                board_to_json,
                c(list(board), dot_args, list(session = session))
              ),
              coal(
                get_board_option_or_null("board_name", session),
                board$board_id
              ),
              type = "json",
              versioned = TRUE,
              tags = blockr_session_tags()
            ),
            error = cnd_to_notif(type = "error")
          )
          if (not_null(res)) {
            showNotification(
              paste("Successfully saved", res),
              type = "message",
              session = session
            )
          }
        }
      )

      res <- reactiveVal()

      meta <- reactiveVal("")

      observeEvent(
        input$pin_version,
        {
          req(input$pin_name, input$pin_version)
          out <- tryCatch(
            pins::pin_meta(backend, input$pin_name, input$pin_version),
            error = cnd_to_notif()
          )
          req(out)
          meta(out)
        }
      )

      output$pin_meta <- renderPrint(print(req(meta())))

      observeEvent(
        input$browse,
        showModal(
          pins_modal(session$ns, board, input, backend)
        )
      )

      observeEvent(
        input$pin_name,
        {
          meta("")
          update_versions(input$pin_name, backend, session)
        }
      )

      observeEvent(
        input$delete_board,
        {
          req(input$pin_name)
          res <- tryCatch(
            pins::pin_delete(backend, input$pin_name),
            error = cnd_to_notif()
          )
          if (not_null(res)) {
            showNotification(
              "Successfully removed board",
              type = "message",
              session = session
            )
            meta("")
            update_pins(backend, session)
            reset_versions(session)
          }
        }
      )

      observeEvent(
        input$delete_version,
        {
          req(input$pin_name, input$pin_version)
          res <- tryCatch(
            pins::pin_version_delete(
              backend,
              input$pin_name,
              input$pin_version
            ),
            error = cnd_to_notif()
          )
          if (not_null(res)) {
            showNotification(
              "Successfully removed version",
              type = "message",
              session = session
            )
            meta("")
            update_versions(input$pin_name, backend, session)
          }
        }
      )

      observeEvent(
        input$restore,
        {
          req(input$pin_name, input$pin_version)
          json <- tryCatch(
            pins::pin_read(backend, input$pin_name, input$pin_version),
            error = cnd_to_notif()
          )

          if (not_null(json)) {
            do.call(
              restore_board,
              c(list(board$board, json, res), dot_args, list(session = session))
            )
            removeModal(session)
          }
        }
      )

      observeEvent(
        input$cancel,
        removeModal(session)
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
      NS(id, "browse"),
      "Browse"
    )
  )
}

board_to_json <- function(rv, ..., session) {
  jsonlite::prettify(
    serialize_board(rv$board, rv$blocks, ..., session = session)
  )
}

pins_modal <- function(ns, board, input, backend) {
  modalDialog(
    title = "Browse boards",
    selectInput(
      ns("pin_name"),
      "Boards",
      choices = pin_list(backend),
      selected = board$board_id
    ),
    selectInput(
      ns("pin_version"),
      "Versions",
      choices = c(`Select board` = "")
    ),
    verbatimTextOutput(ns("pin_meta")),
    footer = tagList(
      actionButton(
        ns("restore"),
        "Restore",
        class = "btn-success"
      ),
      actionButton(
        ns("delete_board"),
        "Delete board",
        class = "btn-danger"
      ),
      actionButton(
        ns("delete_version"),
        "Delete version",
        class = "btn-danger"
      ),
      actionButton(
        ns("cancel"),
        "Cancel"
      )
    ),
    size = "l"
  )
}
