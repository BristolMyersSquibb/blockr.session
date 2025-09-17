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
          tryCatch(
            pins::pin_write(
              backend,
              board_to_json(board$board, board$blocks, session),
              board$board_id,
              type = "json",
              versioned = TRUE,
              tags = "blockr"
            ),
            error = cnd_to_notif(type = "error")
          )
        }
      )

      res <- reactiveVal()

      output$pin_meta <- renderPrint(
        {
          req(input$pin_name, input$pin_version)
          meta <- tryCatch(
            pins::pin_meta(backend, input$pin_name, input$pin_version),
            error = cnd_to_notif()
          )
          req(meta)
          print(meta)
        }
      )

      observeEvent(
        input$browse,
        showModal(
          pins_modal(session$ns, board, input, backend)
        )
      )

      observeEvent(
        input$pin_name,
        {
          updateSelectInput(
            session,
            "pin_version",
            choices = pin_versions(input$pin_name, backend),
            selected = character(0)
          )
        }
      )

      observeEvent(
        input$delete_board,
        {
          req(input$pin_name)
          tryCatch(
            pins::pin_delete(backend, input$pin_name),
            error = cnd_to_notif()
          )
        }
      )

      observeEvent(
        input$delete_version,
        {
          req(input$pin_name, input$pin_version)
          tryCatch(
            pins::pin_version_delete(
              backend,
              input$pin_name,
              input$pin_version
            ),
            error = cnd_to_notif()
          )
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
            res(from_json(json))
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

pins_modal <- function(ns, board, input, backend) {
  modalDialog(
    title = "Browse boards",
    selectInput(
      ns("pin_name"),
      "Boards",
      choices = pins::pin_list(backend),
      selected = board$board_id
    ),
    selectInput(
      ns("pin_version"),
      "Versions",
      choices = character(0)
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
