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
            do.call(
              upload_board,
              c(list(backend, board), dot_args, list(session = session))
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

          if (has_tags(out)) {

            shinyjs::enable("restore")
            shinyjs::enable("delete_board")
            shinyjs::enable("delete_version")

            meta(out)

          } else {

            shinyjs::disable("restore")
            shinyjs::disable("delete_board")
            shinyjs::disable("delete_version")

            meta("Pin not compatible with blockr")
          }
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

          meda <- meta()

          if (!is.list(meda) && "tags" %in% names(meda) && has_tags(meda)) {

            showNotification(
              "Pin not compatible with blockr",
              type = "warning",
              session = session
            )

          } else {
            board_ser <- tryCatch(
              download_board(
                backend,
                input$pin_name,
                input$pin_version,
                meda$pin_hash,
                meda$user$format
              ),
              error = cnd_to_notif()
            )

            if (not_null(board_ser)) {
              do.call(
                restore_board,
                c(
                  list(board$board, board_ser, res),
                  dot_args,
                  list(session = session)
                )
              )

              removeModal(session)
            }
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

upload_board <- function(backend, rv, ..., session) {

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))

  jsonlite::write_json(
    serialize_board(rv$board, rv$blocks, rv$board_id, ..., session = session),
    tmp,
    null = "null"
  )

  name <- coal(
    get_board_option_or_null("board_name", session),
    rv$board_id
  )

  pins::pin_upload(
    backend,
    tmp,
    name,
    versioned = TRUE,
    metadata = list(format = "v1"),
    tags = blockr_session_tags()
  )
}

download_board <- function(backend, name, version, hash, format) {

  dat <- pins::pin_download(backend, name, version, hash)

  switch(
    format,
    v1 = jsonlite::fromJSON(
      dat,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    ),
    blockr_abort(
      "Unrecognized file format {format}.",
      class = "unknown_file_format"
    )
  )
}

pins_modal <- function(ns, board, input, backend) {
  modalDialog(
    shinyjs::useShinyjs(),
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
      shinyjs::disabled(
        actionButton(
          ns("restore"),
          "Restore",
          class = "btn-success"
        )
      ),
      shinyjs::disabled(
        actionButton(
          ns("delete_board"),
          "Delete board",
          class = "btn-danger"
        )
      ),
      shinyjs::disabled(
        actionButton(
          ns("delete_version"),
          "Delete version",
          class = "btn-danger"
        )
      ),
      actionButton(
        ns("cancel"),
        "Cancel"
      )
    ),
    size = "l"
  )
}
