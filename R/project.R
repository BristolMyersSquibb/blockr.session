#' Project management
#'
#' Enhanced session management with navbar-compatible UI. Provides a
#' [blockr.core::preserve_board()] plugin with full navbar layout including
#' workflows, version history, and editable title.
#'
#' @inheritParams blockr.core::preserve_board
#'
#' @return See [blockr.core::preserve_board()].
#'
#' @export
manage_project <- function(
  server = manage_project_server,
  ui = manage_project_ui
) {
  preserve_board(server, ui)
}

#' @param id Namespace ID
#' @param x Board object
#'
#' @rdname manage_project
#' @export
manage_project_ui <- function(id, x) {
  ns <- NS(id)

  tagList(
    # CSS and JS dependencies
    htmltools::htmlDependency(
      "project-navbar",
      as.character(utils::packageVersion("blockr.session")),
      src = system.file("assets", package = "blockr.session"),
      stylesheet = "css/project-navbar.css",
      script = "js/project-navbar.js"
    ),
    # Full-width navbar container - single flex row with spacer
    tags$div(
      class = "manage-project-navbar",
      # Hamburger menu with tabbed dropdown
      tags$div(
        class = "dropdown",
        tags$button(
          class = "blockr-navbar-icon-btn",
          type = "button",
          `data-bs-toggle` = "dropdown",
          `aria-expanded` = "false",
          bsicons::bs_icon("layers", size = "1.4em")
        ),
        # Tabbed dropdown content
        tags$div(
          id = ns("tabbed_dropdown"),
          class = "dropdown-menu blockr-tabbed-dropdown",
          # Tab bar
          tags$div(
            class = "blockr-tab-bar",
            tags$button(
              id = ns("tab_workflows"),
              class = "blockr-tab active",
              type = "button",
              onclick = sprintf(
                "event.stopPropagation();
                document.querySelectorAll(
                  '#%s .blockr-tab'
                ).forEach(
                  t => t.classList.remove('active')
                );
                this.classList.add('active');
                document.getElementById(
                  '%s'
                ).classList.remove(
                  'blockr-tab-panel-hidden'
                );
                document.getElementById(
                  '%s'
                ).classList.add(
                  'blockr-tab-panel-hidden'
                );",
                ns("tabbed_dropdown"),
                ns("panel_workflows"),
                ns("panel_history")
              ),
              bsicons::bs_icon("layers"),
              "Workflows"
            ),
            tags$button(
              id = ns("tab_history"),
              class = "blockr-tab",
              type = "button",
              onclick = sprintf(
                "event.stopPropagation();
                document.querySelectorAll(
                  '#%s .blockr-tab'
                ).forEach(
                  t => t.classList.remove('active')
                );
                this.classList.add('active');
                document.getElementById(
                  '%s'
                ).classList.add(
                  'blockr-tab-panel-hidden'
                );
                document.getElementById(
                  '%s'
                ).classList.remove(
                  'blockr-tab-panel-hidden'
                );",
                ns("tabbed_dropdown"),
                ns("panel_workflows"),
                ns("panel_history")
              ),
              bsicons::bs_icon("clock-history"),
              "History"
            )
          ),
          # Workflows panel
          tags$div(
            id = ns("panel_workflows"),
            class = "blockr-tab-panel",
            tags$div(class = "blockr-workflows-section", "RECENT"),
            tags$div(
              class = "blockr-workflows-list",
              uiOutput(ns("recent_workflows"))
            ),
            tags$div(
              class = "blockr-tab-footer",
              tags$a(
                href = "#",
                class = "blockr-workflows-link",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', Date.now(), {priority: 'event'});
                  return false;",
                  ns("view_all_workflows")
                ),
                "View all workflows ",
                bsicons::bs_icon("arrow-right")
              )
            )
          ),
          # History panel
          tags$div(
            id = ns("panel_history"),
            class = "blockr-tab-panel blockr-tab-panel-hidden",
            tags$div(
              class = "blockr-history-title",
              uiOutput(ns("history_title"), inline = TRUE)
            ),
            uiOutput(ns("version_history")),
            tags$div(
              class = "blockr-tab-footer",
              tags$a(
                href = "#",
                class = "blockr-workflows-link",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', Date.now(), {priority: 'event'});
                  return false;",
                  ns("view_all_versions")
                ),
                "View all versions ",
                bsicons::bs_icon("arrow-right")
              )
            )
          )
        )
      ),
      # Editable workflow title
      tags$div(
        id = ns("title_wrapper"),
        class = "blockr-navbar-title-wrapper",
        tags$span(
          id = ns("title_display"),
          class = "blockr-navbar-title",
          onclick = sprintf(
            "document.getElementById('%s').classList.add('editing');
            document.getElementById('%s').focus();
            document.getElementById('%s').select();",
            ns("title_wrapper"),
            ns("title_input"),
            ns("title_input")
          ),
          ""
        ),
        tags$input(
          id = ns("title_input"),
          class = "blockr-navbar-title-input shiny-bound-input",
          type = "text",
          value = "",
          onblur = sprintf(
            "Shiny.setInputValue('%s', this.value, {priority: 'event'});
            document.getElementById('%s').classList.remove('editing');
            document.getElementById('%s').textContent = this.value;",
            ns("title_edit"),
            ns("title_wrapper"),
            ns("title_display")
          ),
          onkeydown = sprintf(
            "if(event.key === 'Enter') {
              this.blur();
            }
            if(event.key === 'Escape') {
              document.getElementById('%s').classList.remove('editing');
              this.value = document.getElementById('%s').textContent;
            }",
            ns("title_wrapper"),
            ns("title_display")
          )
        )
      ),
      # Divider
      tags$span(class = "blockr-navbar-divider"),
      # Save status
      tags$div(
        class = "blockr-navbar-save-section",
        textOutput(
          ns("save_status"),
          container = tags$span,
          inline = TRUE
        ) |>
          tagAppendAttributes(class = "blockr-navbar-meta"),
        tags$button(
          id = ns("save_btn"),
          class = "blockr-navbar-save-btn shiny-bound-input",
          type = "button",
          onclick = sprintf(
            "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
            ns("save_btn")
          ),
          bsicons::bs_icon("floppy", size = "1em")
        )
      ),
      # New button
      tags$button(
        id = ns("new_btn"),
        class = "blockr-navbar-btn-new shiny-bound-input",
        type = "button",
        onclick = sprintf(
          "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
          ns("new_btn")
        ),
        bsicons::bs_icon("plus"),
        "New"
      ),
      # Spacer to push avatar to the right
      tags$span(class = "manage-project-spacer"),
      # User avatar
      uiOutput(ns("user_avatar"), inline = TRUE)
    )
  )
}

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

      # Track last known board name to detect changes
      last_board_name <- reactiveVal(NULL)

      # Initialize save status
      observe({
        name <- coal(
          get_board_option_or_null("board_name", session),
          board$board_id
        )

        if (!identical(name, last_board_name())) {
          last_board_name(name)
          if (!is.null(name) && nchar(name) > 0) {
            meta <- tryCatch(
              pins::pin_meta(backend, name),
              error = function(e) NULL
            )
            if (!is.null(meta) && !is.null(meta$created)) {
              time_ago <- format_time_ago(meta$created)
              save_status(time_ago)
            } else {
              save_status("Not saved")
            }
          }
        }
      })

      # SAVE button
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
            showNotification(
              paste("Successfully saved", res),
              type = "message",
              session = session
            )
            save_status("Just now")
            refresh_trigger(refresh_trigger() + 1)
          }
        }
      )

      # NEW button
      observeEvent(
        input$new_btn,
        {
          new <- clear_board(board$board)
          attr(new, "id") <- rand_names()
          restore_result(new)
        }
      )

      # Recent workflows list (uses efficient pin_search via list_workflows)
      output$recent_workflows <- renderUI({
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
                onclick = sprintf(
                  "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                  session$ns("load_workflow"),
                  info$name
                ),
                tags$div(class = "blockr-workflow-name", info$name),
                tags$div(class = "blockr-workflow-meta", info$time_ago)
              )
            }
          )
        )
      })

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
            showNotification("No saved workflows", type = "message")
            return()
          }

          rows <- lapply(seq_len(nrow(workflows)), function(i) {
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
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'});
                    bootstrap.Modal.getInstance(
                      document.getElementById('%s')
                    ).hide();",
                    session$ns("load_workflow"),
                    info$name,
                    session$ns("workflows_modal")
                  ),
                  "Load"
                )
              )
            )
          })

          modal_js <- sprintf(
            "document.getElementById('%s').addEventListener(
              'input',
              function(e) {
                var filter = e.target.value.toLowerCase();
                var rows = document.querySelectorAll('.blockr-workflow-row');
                rows.forEach(
                  function(row) {
                    var name = row.getAttribute('data-name');
                    row.style.display = name.includes(filter) ? '' : 'none';
                  }
                );
              }
            );

            document.getElementById('%s').addEventListener(
              'change',
              function(e) {
                var checkboxes = document.querySelectorAll('.blockr-wf-select');
                checkboxes.forEach(
                  function(cb) {
                    cb.checked = e.target.checked
                  }
                )
                updateDeleteButton();
              }
            );

            function updateDeleteButton() {
              var selected = document.querySelectorAll(
                '.blockr-wf-select:checked'
              );
              var btn = document.getElementById('%s');
              btn.style.display = selected.length > 0 ? '' : 'none';
              btn.textContent = 'Delete (' + selected.length + ')';
            }

            document.querySelectorAll('.blockr-wf-select').forEach(
              function(cb) {
                cb.addEventListener('change', updateDeleteButton);
              }
            );

            document.getElementById('%s').addEventListener(
              'click',
              function() {
                var selected = [];
                document.querySelectorAll(
                  '.blockr-wf-select:checked'
                ).forEach(
                  function(cb) {
                    selected.push(cb.value);
                  }
                );
                if (selected.length > 0 &&
                      confirm('Delete ' + selected.length + ' workflow(s)?')) {
                  Shiny.setInputValue('%s', selected, {priority: 'event'});
                }
              }
            );

            updateDeleteButton();",
            session$ns("workflow_search"),
            session$ns("select_all"),
            session$ns("delete_workflows_btn"),
            session$ns("delete_workflows_btn"),
            session$ns("delete_workflows")
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
      )

      # DELETE workflows
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
                showNotification(
                  paste("Failed to delete:", name),
                  type = "error"
                )
                FALSE
              }
            )
          }
          if (deleted > 0) {
            showNotification(
              paste("Deleted", deleted, "workflow(s)"),
              type = "message"
            )
            removeModal()
            refresh_trigger(refresh_trigger() + 1)
          }
        }
      )

      # History - show current workflow name
      output$history_title <- renderUI({
        tags$span(
          coal(
            get_board_option_or_null("board_name", session),
            board$board_id
          )
        )
      })

      # Version history list
      output$version_history <- renderUI({
        refresh_trigger()

        name <- coal(
          get_board_option_or_null("board_name", session),
          board$board_id
        )
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
          return(tags$div(class = "blockr-history-empty", "No versions found"))
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
                sprintf(
                  "Shiny.setInputValue(
                      '%s',
                      {
                        name: '%s',
                        version: '%s'
                      },
                      {
                        priority: 'event'
                      }
                    )",
                  session$ns("load_version"),
                  name,
                  v$version
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
      })

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
              showNotification(e$message, type = "error")
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
          name <- coal(
            get_board_option_or_null("board_name", session),
            board$board_id
          )
          if (is.null(name) || name == "") {
            showNotification(
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
            showNotification("No versions found", type = "message")
            return()
          }

          versions <- versions[order(versions$created, decreasing = TRUE), ]

          rows <- lapply(seq_len(nrow(versions)), function(i) {
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
                    onclick = sprintf(
                      "Shiny.setInputValue(
                        '%s',
                        {
                          name: '%s',
                          version: '%s'
                        },
                        {
                          priority: 'event'
                        }
                      );
                      bootstrap.Modal.getInstance(
                        document.getElementById('%s')
                      ).hide();",
                      session$ns("load_version"),
                      name,
                      v$version,
                      session$ns("versions_modal")
                    ),
                    "Load"
                  )
                }
              )
            )
          })

          modal_js <- sprintf(
            "document.getElementById('%s').addEventListener(
              'change',
              function(e) {
                var checkboxes = document.querySelectorAll(
                  '.blockr-version-select:not(:disabled)'
                );
                checkboxes.forEach(
                  function(cb) {
                    cb.checked = e.target.checked
                  }
                )
                updateVersionDeleteButton();
              }
            );

            function updateVersionDeleteButton() {
              var selected = document.querySelectorAll(
                '.blockr-version-select:checked'
              );
              var btn = document.getElementById('%s');
              btn.style.display = selected.length > 0 ? '' : 'none';
              btn.textContent = 'Delete (' + selected.length + ')';
            }

            document.querySelectorAll(
              '.blockr-version-select'
            ).forEach(
              function(cb) {
                cb.addEventListener('change', updateVersionDeleteButton);
              }
            );

            document.getElementById('%s').addEventListener(
              'click',
              function() {
                var selected = [];
                document.querySelectorAll(
                  '.blockr-version-select:checked'
                ).forEach(
                  function(cb) {
                    selected.push(cb.value);
                  }
                );
                if (selected.length > 0 &&
                    confirm('Delete ' + selected.length + ' version(s)?')) {
                  Shiny.setInputValue('%s', selected, {priority: 'event'});
                }
              }
            );

            updateVersionDeleteButton();",
            session$ns("select_all_versions"),
            session$ns("delete_versions_btn"),
            session$ns("delete_versions_btn"),
            session$ns("delete_versions")
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
      )

      # Delete versions
      observeEvent(
        input$delete_versions,
        {
          req(input$delete_versions)
          name <- coal(
            get_board_option_or_null("board_name", session),
            board$board_id
          )
          deleted <- 0
          for (version in input$delete_versions) {
            res <- tryCatch(
              {
                pins::pin_version_delete(backend, name, version)
                deleted <- deleted + 1
                TRUE
              },
              error = function(e) {
                showNotification(
                  paste("Failed to delete version:", version),
                  type = "error"
                )
                FALSE
              }
            )
          }
          if (deleted > 0) {
            showNotification(
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
      observe({
        title <- coal(
          get_board_option_or_null("board_name", session),
          board$board_id,
          "Untitled"
        )
        session$sendCustomMessage("blockr-update-navbar-title", title)
      })

      # User avatar
      output$user_avatar <- renderUI({
        username <- coal(
          session$user,
          Sys.getenv("USER"),
          Sys.getenv("USERNAME"),
          "User"
        )
        initials <- get_initials(username)
        tags$div(class = "blockr-navbar-avatar", initials)
      })

      observe({
        query <- shiny::parseQueryString(session$clientData$url_search)
        workflow_id <- query$id
        if (!is.null(workflow_id) && nzchar(workflow_id)) {
          wf_name <- gsub("-", "/", workflow_id, fixed = TRUE)
          versions <- tryCatch(
            pins::pin_versions(backend, wf_name),
            error = function(e) NULL
          )
          version <- NULL
          if (!is.null(versions) && nrow(versions) > 0) {
            version <- versions$version[1]
          }
          meta <- tryCatch(
            pins::pin_meta(backend, wf_name, version),
            error = function(e) NULL
          )
          if (!is.null(meta)) {
            board_ser <- download_board(
              backend,
              wf_name,
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
        }
      })

      # Return the reactiveVal for preserve_board
      restore_result
    }
  )
}
