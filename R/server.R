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

      backend <- get_session_backend()
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

            info <- tryCatch(
              rack_info(rack_id_for_board(name, backend), backend),
              error = function(e) NULL
            )

            if (!is.null(info) && nrow(info) > 0L) {
              save_status(format_time_ago(info$created[1L]))
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
            {
              data <- do.call(
                serialize_board,
                c(
                  list(board$board, board$blocks, board$board_id),
                  dot_args,
                  list(session = session)
                )
              )
              rack_save(backend, data, name = board_name())
            },
            error = cnd_to_notif(type = "error")
          )
          if (not_null(res)) {
            notify(
              paste("Successfully saved", display_name(res)),
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

          workflows <- tryCatch(
            rack_list(backend),
            error = function(e) list()
          )

          if (length(workflows) == 0L) {
            return(
              tags$div(class = "blockr-workflow-empty", "No saved workflows")
            )
          }

          tagList(
            lapply(
              seq_len(min(length(workflows), 4L)),
              function(i) {
                wf <- workflows[[i]]
                wf_time <- format_time_ago(last_saved(wf, backend))
                tags$div(
                  class = "blockr-workflow-item",
                  onclick = shiny_input_obj_js(
                    session$ns("load_workflow"),
                    name = display_name(wf),
                    user = coal(wf$user, "")
                  ),
                  tags$div(
                    class = "blockr-workflow-name",
                    display_name(wf)
                  ),
                  tags$div(class = "blockr-workflow-meta", wf_time)
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
          id <- rack_id_from_input(input$load_workflow)

          board_ser <- tryCatch(
            rack_load(id, backend),
            error = cnd_to_notif(type = "error")
          )

          if (is.null(board_ser)) {
            return()
          }

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
          workflows <- tryCatch(
            rack_list(backend),
            error = function(e) list()
          )

          if (length(workflows) == 0L) {
            notify("No saved workflows", type = "message")
            return()
          }

          show_workflows_modal(workflows, backend, session)
        }
      )

      observeEvent(
        input$delete_workflows,
        {
          req(input$delete_workflows)
          deleted <- 0
          for (wf in input$delete_workflows) {
            id <- rack_id_from_input(wf)
            res <- tryCatch(
              {
                rack_purge(id, backend)
                deleted <- deleted + 1
                TRUE
              },
              error = function(e) {
                notify(
                  paste("Failed to delete:", display_name(id)),
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

          id <- rack_id_for_board(name, backend)

          versions <- tryCatch(
            rack_info(id, backend),
            error = function(e) NULL
          )

          if (is.null(versions) || nrow(versions) == 0L) {
            return(
              tags$div(class = "blockr-history-empty", "No versions found")
            )
          }

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
                    user = coal(id$user, ""),
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

          id <- rack_id_from_input(input$load_version)

          board_ser <- tryCatch(
            rack_load(id, backend),
            error = cnd_to_notif(type = "error")
          )

          if (is.null(board_ser)) {
            return()
          }

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

          id <- rack_id_for_board(name, backend)

          versions <- tryCatch(
            rack_info(id, backend),
            error = function(e) NULL
          )

          if (is.null(versions) || nrow(versions) == 0L) {
            notify("No versions found", type = "message")
            return()
          }

          show_versions_modal(id, versions, session)
        }
      )

      # Delete versions
      observeEvent(
        input$delete_versions,
        {
          req(input$delete_versions)
          id <- rack_id_for_board(board_name(), backend)
          deleted <- 0
          for (version in input$delete_versions) {
            ver_id <- rack_id_from_input(
              list(name = id$name, user = id$user, version = version)
            )
            res <- tryCatch(
              {
                rack_delete(ver_id, backend)
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
  obj_parts <- chr_ply(
    names(props),
    function(k) sprintf("%s: '%s'", k, props[[k]])
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

show_workflows_modal <- function(workflows, backend, session) {

  rows <- lapply(
    seq_along(workflows),
    function(i) {
      wf <- workflows[[i]]
      wf_time <- format_time_ago(last_saved(wf, backend))
      tags$tr(
        class = "blockr-workflow-row",
        `data-name` = tolower(display_name(wf)),
        `data-user` = coal(wf$user, ""),
        tags$td(
          class = "blockr-wf-checkbox",
          tags$input(
            type = "checkbox",
            class = "blockr-wf-select",
            `data-name` = display_name(wf),
            `data-user` = coal(wf$user, "")
          )
        ),
        tags$td(class = "blockr-wf-name", display_name(wf)),
        tags$td(class = "blockr-wf-time", wf_time),
        tags$td(
          class = "blockr-wf-action",
          tags$button(
            class = "btn btn-sm btn-primary",
            onclick = paste0(
              shiny_input_obj_js(
                session$ns("load_workflow"),
                name = display_name(wf),
                user = coal(wf$user, "")
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

show_versions_modal <- function(id, versions, session) {

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
                  name = id$name,
                  user = coal(id$user, ""),
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
          tags$h5(paste("Version History:", display_name(id))),
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
        var item = cb.dataset && cb.dataset.name
          ? {name: cb.dataset.name, user: cb.dataset.user || ''}
          : cb.value;
        selected.push(item);
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
