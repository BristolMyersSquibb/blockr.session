#' @param board Reactive values object containing board state
#' @param ... Extra arguments (may include dock object)
#'
#' @rdname manage_project
#' @export
manage_project_server <- function(id, board, ...) {

  dot_args <- list(...)

  moduleServer(
    id,
    function(input, output, session) {

      backend <- blockr_option("session_mgmt_backend", pins::board_local())
      restore_result <- reactiveVal()

      refresh_trigger <- reactiveVal(0)
      save_status <- reactiveVal("Not saved")

      board_name <- reactive(
        coal(
          get_board_option_or_null("board_name", session),
          board$board_id
        )
      )

      prev_board_name <- reactiveVal(NULL)

      observeEvent(
        req(!identical(board_name(), prev_board_name())),
        {
          name <- board_name()

          if (!is.null(name) && nchar(name) > 0) {

            prev_board_name(name)

            meta <- tryCatch(
              pins::pin_meta(backend, name),
              error = function(e) NULL
            )

            if (!is.null(meta) && !is.null(meta$created)) {
              save_status(format_time_ago(meta$created))
            } else {
              save_status("Not saved")
            }
          }
        }
      )

      observeEvent(
        input$save_btn,
        {
          res <- tryCatch(
            do.call(
              upload_board,
              c(list(backend, board), dot_args, list(session = session))
            ),
            error = cnd_to_notif(type = "error")
          )
          if (not_null(res)) {
            notify(
              paste("Successfully saved", res),
              type = "message",
              session = session
            )
            save_status("Just now")
            refresh_trigger(refresh_trigger() + 1)
          }
        }
      )

      observeEvent(
        input$new_btn,
        {
          new <- clear_board(board$board)
          attr(new, "id") <- rand_names()
          restore_result(new)
        }
      )

      output$recent_workflows <- renderUI(
        {
          refresh_trigger()

          workflows <- list_workflows(backend)

          if (nrow(workflows) == 0) {
            return(
              tags$div(class = "blockr-workflow-empty", "No saved workflows")
            )
          }

          tagList(
            lapply(
              seq_len(min(nrow(workflows), 4)),
              function(i) {
                info <- workflows[i, ]
                tags$div(
                  class = "blockr-workflow-item",
                  onclick = shiny_input_js(
                    session$ns("load_workflow"),
                    info$name
                  ),
                  tags$div(class = "blockr-workflow-name", info$name),
                  tags$div(class = "blockr-workflow-meta", info$time_ago)
                )
              }
            )
          )
        }
      )

      # Save status output
      output$save_status <- renderText(
        save_status()
      )

      # LOAD workflow
      observeEvent(
        input$load_workflow,
        {
          name <- input$load_workflow
          versions <- pin_versions(name, backend)

          if (length(versions) == 0) {
            return()
          }

          meta <- pins::pin_meta(backend, name, versions[1])

          board_ser <- download_board(
            backend,
            name,
            versions[1],
            meta$pin_hash,
            meta$user$format
          )

          restore_board(
            board$board,
            board_ser,
            restore_result,
            session = session
          )
        }
      )

      # VIEW ALL WORKFLOWS modal
      observeEvent(
        input$view_all_workflows,
        {
          workflows <- list_workflows(backend)

          if (nrow(workflows) == 0) {
            notify("No saved workflows", type = "message")
            return()
          }

          show_workflows_modal(workflows, session)
        }
      )

      observeEvent(
        input$delete_workflows,
        {
          req(input$delete_workflows)
          deleted <- 0
          for (name in input$delete_workflows) {
            res <- tryCatch(
              {
                pins::pin_delete(backend, name)
                deleted <- deleted + 1
                TRUE
              },
              error = function(e) {
                notify(
                  paste("Failed to delete:", name),
                  type = "error"
                )
                FALSE
              }
            )
          }
          if (deleted > 0) {
            notify(
              paste("Deleted", deleted, "workflow(s)"),
              type = "message"
            )
            removeModal()
            refresh_trigger(refresh_trigger() + 1)
          }
        }
      )

      output$history_title <- renderUI(
        tags$span(board_name())
      )

      output$version_history <- renderUI(
        {
          refresh_trigger()

          name <- board_name()
          if (is.null(name) || name == "") {
            return(
              tags$div(
                class = "blockr-history-empty",
                "Save workflow to see history"
              )
            )
          }

          versions <- tryCatch(
            pins::pin_versions(backend, name),
            error = function(e) NULL
          )

          if (is.null(versions) || nrow(versions) == 0) {
            return(
              tags$div(class = "blockr-history-empty", "No versions found")
            )
          }

          versions <- versions[order(versions$created, decreasing = TRUE), ]

          items <- lapply(
            seq_len(min(nrow(versions), 4)),
            function(i) {
              v <- versions[i, ]
              time_ago <- format_time_ago(v$created)
              is_current <- i == 1

              tags$div(
                class = paste(
                  "blockr-workflow-item",
                  if (is_current) "current" else ""
                ),
                onclick = if (!is_current) {
                  shiny_input_obj_js(
                    session$ns("load_version"),
                    name = name,
                    version = v$version
                  )
                },
                tags$div(class = "blockr-workflow-name", time_ago),
                if (is_current) {
                  tags$div(class = "blockr-workflow-meta", "(Current)")
                }
              )
            }
          )

          tagList(items)
        }
      )

      # Load specific version
      observeEvent(
        input$load_version,
        {
          req(input$load_version$name, input$load_version$version)
          name <- input$load_version$name
          version <- input$load_version$version

          meta <- tryCatch(
            pins::pin_meta(backend, name, version),
            error = function(e) {
              notify(e$message, type = "error")
              NULL
            }
          )
          if (is.null(meta)) {
            return()
          }

          board_ser <- download_board(
            backend,
            name,
            version,
            meta$pin_hash,
            meta$user$format
          )
          restore_board(
            board$board,
            board_ser,
            restore_result,
            session = session
          )
        }
      )

      # VIEW ALL VERSIONS modal
      observeEvent(
        input$view_all_versions,
        {
          name <- board_name()
          if (is.null(name) || name == "") {
            notify(
              "Save workflow first to see versions",
              type = "message"
            )
            return()
          }

          versions <- tryCatch(
            pins::pin_versions(backend, name),
            error = function(e) NULL
          )

          if (is.null(versions) || nrow(versions) == 0) {
            notify("No versions found", type = "message")
            return()
          }

          versions <- versions[order(versions$created, decreasing = TRUE), ]

          show_versions_modal(name, versions, session)
        }
      )

      # Delete versions
      observeEvent(
        input$delete_versions,
        {
          req(input$delete_versions)
          name <- board_name()
          deleted <- 0
          for (version in input$delete_versions) {
            res <- tryCatch(
              {
                pins::pin_version_delete(backend, name, version)
                deleted <- deleted + 1
                TRUE
              },
              error = function(e) {
                notify(
                  paste("Failed to delete version:", version),
                  type = "error"
                )
                FALSE
              }
            )
          }
          if (deleted > 0) {
            notify(
              paste("Deleted", deleted, "version(s)"),
              type = "message"
            )
            removeModal()
            refresh_trigger(refresh_trigger() + 1)
          }
        }
      )

      # Title edit
      observeEvent(
        input$title_edit,
        set_board_option_value("board_name", input$title_edit, session)
      )

      # Initialize title
      observe(
        session$sendCustomMessage("blockr-update-navbar-title", board_name())
      )

      # User avatar
      output$user_avatar <- renderUI(
        {
          username <- coal(
            session$user,
            Sys.getenv("USER"),
            Sys.getenv("USERNAME"),
            "User"
          )
          initials <- get_initials(username)
          tags$div(class = "blockr-navbar-avatar", initials)
        }
      )

      # Return the reactiveVal for preserve_board
      restore_result
    }
  )
}

shiny_input_js <- function(ns_id, value) {
  sprintf(
    "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
    ns_id,
    value
  )
}

shiny_input_obj_js <- function(ns_id, ...) {
  props <- list(...)
  obj_parts <- vapply(
    names(props),
    function(k) sprintf("%s: '%s'", k, props[[k]]),
    character(1)
  )
  sprintf(
    "Shiny.setInputValue('%s', {%s}, {priority: 'event'})",
    ns_id,
    paste(obj_parts, collapse = ", ")
  )
}

hide_modal_js <- function(modal_id) {
  sprintf(
    "bootstrap.Modal.getInstance(document.getElementById('%s')).hide()",
    modal_id
  )
}

show_workflows_modal <- function(workflows, session) {

  rows <- lapply(
    seq_len(nrow(workflows)),
    function(i) {
      info <- workflows[i, ]
      tags$tr(
        class = "blockr-workflow-row",
        `data-name` = tolower(info$name),
        tags$td(
          class = "blockr-wf-checkbox",
          tags$input(
            type = "checkbox",
            class = "blockr-wf-select",
            value = info$name
          )
        ),
        tags$td(class = "blockr-wf-name", info$name),
        tags$td(class = "blockr-wf-time", info$time_ago),
        tags$td(
          class = "blockr-wf-action",
          tags$button(
            class = "btn btn-sm btn-primary",
            onclick = paste0(
              shiny_input_js(
                session$ns("load_workflow"),
                info$name
              ),
              "\n",
              hide_modal_js(session$ns("workflows_modal"))
            ),
            "Load"
          )
        )
      )
    }
  )

  modal_js <- modal_table_js(
    select_all_id = session$ns("select_all"),
    checkbox_class = "blockr-wf-select",
    delete_btn_id = session$ns("delete_workflows_btn"),
    delete_input_id = session$ns("delete_workflows"),
    item_type = "workflow",
    search_id = session$ns("workflow_search")
  )

  showModal(
    modalDialog(
      title = NULL,
      size = "l",
      easyClose = TRUE,
      footer = NULL,
      tags$div(
        id = session$ns("workflows_modal"),
        class = "blockr-workflows-modal",
        tags$div(
          class = "blockr-wf-header",
          tags$h5("All Workflows"),
          tags$div(
            class = "blockr-wf-header-actions",
            tags$button(
              id = session$ns("delete_workflows_btn"),
              class = "btn btn-sm btn-outline-danger",
              style = "display: none;",
              "Delete"
            ),
            tags$input(
              type = "text",
              id = session$ns("workflow_search"),
              class = "blockr-wf-search",
              placeholder = "Search workflows..."
            )
          )
        ),
        tags$div(
          class = "blockr-wf-table-container",
          tags$table(
            class = "blockr-wf-table",
            tags$thead(
              tags$tr(
                tags$th(
                  class = "blockr-wf-checkbox",
                  tags$input(
                    type = "checkbox",
                    id = session$ns("select_all")
                  )
                ),
                tags$th("Name"),
                tags$th("Last Modified"),
                tags$th("")
              )
            ),
            tags$tbody(rows)
          )
        ),
        tags$script(HTML(modal_js))
      )
    )
  )
}

show_versions_modal <- function(name, versions, session) {

  rows <- lapply(
    seq_len(nrow(versions)),
    function(i) {
      v <- versions[i, ]
      time_ago <- format_time_ago(v$created)
      is_current <- i == 1

      tags$tr(
        class = "blockr-workflow-row",
        tags$td(
          class = "blockr-wf-checkbox",
          tags$input(
            type = "checkbox",
            class = "blockr-version-select",
            value = v$version,
            disabled = if (is_current) "disabled" else NULL
          )
        ),
        tags$td(
          class = "blockr-wf-name",
          time_ago,
          if (is_current) {
            tags$span(class = "blockr-version-badge", "(Current)")
          }
        ),
        tags$td(
          class = "blockr-wf-action",
          if (!is_current) {
            tags$button(
              class = "btn btn-sm btn-primary",
              onclick = paste0(
                shiny_input_obj_js(
                  session$ns("load_version"),
                  name = name,
                  version = v$version
                ),
                "\n",
                hide_modal_js(session$ns("versions_modal"))
              ),
              "Load"
            )
          }
        )
      )
    }
  )

  modal_js <- modal_table_js(
    select_all_id = session$ns("select_all_versions"),
    checkbox_class = "blockr-version-select",
    delete_btn_id = session$ns("delete_versions_btn"),
    delete_input_id = session$ns("delete_versions"),
    item_type = "version",
    has_disabled = TRUE
  )

  showModal(
    modalDialog(
      title = NULL,
      size = "l",
      easyClose = TRUE,
      footer = NULL,
      tags$div(
        id = session$ns("versions_modal"),
        class = "blockr-workflows-modal",
        tags$div(
          class = "blockr-wf-header",
          tags$h5(paste("Version History:", name)),
          tags$div(
            class = "blockr-wf-header-actions",
            tags$button(
              id = session$ns("delete_versions_btn"),
              class = "btn btn-sm btn-outline-danger",
              style = "display: none;",
              "Delete"
            )
          )
        ),
        tags$div(
          class = "blockr-wf-table-container",
          tags$table(
            class = "blockr-wf-table",
            tags$thead(
              tags$tr(
                tags$th(
                  class = "blockr-wf-checkbox",
                  tags$input(
                    type = "checkbox",
                    id = session$ns("select_all_versions")
                  )
                ),
                tags$th("Version"),
                tags$th("")
              )
            ),
            tags$tbody(rows)
          )
        ),
        tags$script(HTML(modal_js))
      )
    )
  )
}

modal_table_js <- function(select_all_id, checkbox_class, delete_btn_id,
                           delete_input_id, item_type = "item",
                           search_id = NULL, has_disabled = FALSE) {

  if (!is.null(search_id)) {
    search_block <- sprintf(
      "document.getElementById('%s').addEventListener('input', function(e) {
        var filter = e.target.value.toLowerCase();
        var rows = document.querySelectorAll('.blockr-workflow-row');
        rows.forEach(function(row) {
          var name = row.getAttribute('data-name');
          row.style.display = name.includes(filter) ? '' : 'none';
        });
      });",
      search_id
    )
  } else {
    search_block <- ""
  }

  if (has_disabled) {
    disabled_filter <- ":not(:disabled)"
  } else {
    disabled_filter <- ""
  }

  sprintf(
    "%s
    document.getElementById('%s').addEventListener('change', function(e) {
      var checkboxes = document.querySelectorAll('.%s%s');
      checkboxes.forEach(function(cb) { cb.checked = e.target.checked });
      updateDeleteBtn();
    });

    function updateDeleteBtn() {
      var selected = document.querySelectorAll('.%s:checked');
      var btn = document.getElementById('%s');
      btn.style.display = selected.length > 0 ? '' : 'none';
      btn.textContent = 'Delete (' + selected.length + ')';
    }

    document.querySelectorAll('.%s').forEach(function(cb) {
      cb.addEventListener('change', updateDeleteBtn);
    });

    document.getElementById('%s').addEventListener('click', function() {
      var selected = [];
      document.querySelectorAll('.%s:checked').forEach(function(cb) {
        selected.push(cb.value);
      });
      if (selected.length > 0 &&
          confirm('Delete ' + selected.length + ' %s(s)?')) {
        Shiny.setInputValue('%s', selected, {priority: 'event'});
      }
    });

    updateDeleteBtn();",
    search_block,
    select_all_id,
    checkbox_class,
    disabled_filter,
    checkbox_class,
    delete_btn_id,
    checkbox_class,
    delete_btn_id,
    checkbox_class,
    item_type,
    delete_input_id
  )
}
