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

      refresh_trigger <- reactiveVal(0)
      save_status <- reactiveVal("Not saved")

      serialize_now <- function() {
        do.call(
          serialize_board,
          c(
            list(board$board, board$blocks, board$board_id),
            dot_args,
            list(session = session)
          )
        )
      }

      board_name <- reactive(
        coal(
          get_board_option_or_null("board_name", session),
          board$board_id
        )
      )

      prev_query <- reactiveVal(NULL)

      current_query <- reactive(
        coal(prev_query(), session$clientData$url_search, "")
      )

      current_id <- reactive({

        query <- parseQueryString(current_query())

        qid <- coal(query$id, query$board_name, fail_all = FALSE)

        if (is.null(qid) || !nzchar(qid)) {
          return(NULL)
        }

        rack_id_from_input(
          backend,
          list(id = qid, user = query$user)
        )
      })

      observeEvent(
        current_id(),
        {
          id <- current_id()

          if (is.null(id)) {
            save_status("Not saved")
            return()
          }

          # the backend's native name field is authoritative for a persisted
          # record, so seed the in-session board name from it on load (a rename
          # not followed by a content save would otherwise revert on reload).
          nm <- tryCatch(
            rack_name(id, backend),
            error = function(e) NULL
          )
          if (not_null(nm)) {
            tryCatch(
              set_board_option_value(
                "board_name", nm, board$board, session
              ),
              error = function(e) NULL
            )
          }

          info <- tryCatch(rack_info(id, backend), error = function(e) NULL)

          if (not_null(info) && nrow(info) > 0L) {
            save_status(format_time_ago(info$created[1L]))
          } else {
            save_status("Not saved")
          }
        },
        ignoreNULL = FALSE
      )

      observeEvent(
        input$save_btn,
        {
          # Save keys on the board's stable id: a loaded record (current_id),
          # else the board id. It appends a version to that record if it exists,
          # else creates it under the board id -- so the record id and board id
          # match. Only Save As mints a fresh board id to fork.
          target <- coal(
            current_id(),
            rack_id_from_input(backend, list(id = board$board_id))
          )

          exists <- isTRUE(
            tryCatch(rack_exists(target, backend), error = function(e) FALSE)
          )

          data <- tryCatch(
            serialize_now(),
            error = cnd_to_notif(type = "error")
          )

          if (is.null(data)) {
            return()
          }

          if (exists && !rack_content_changed(target, backend, data)) {
            notify("No changes to save", type = "message", session = session)
            return()
          }

          res <- tryCatch(
            if (exists) {
              rack_append(target, backend, data)
            } else {
              rack_create(
                backend, data,
                id = board$board_id, name = board_name()
              )
            },
            error = cnd_to_notif(type = "error")
          )

          if (is.null(res)) {
            return()
          }

          notify(
            paste("Successfully saved", board_name()),
            type = "message",
            session = session
          )
          save_status("Just now")
          refresh_trigger(refresh_trigger() + 1)

          saved <- rack_id_from_input(
            backend,
            list(id = res$id, user = res$user)
          )
          new_url <- board_query_string(saved, backend)
          prev_query(new_url)
          updateQueryString(new_url, mode = "replace", session = session)
        }
      )

      observeEvent(
        input$new_btn,
        {
          updateQueryString("?", mode = "replace", session = session)
          session$reload()
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
                wf_time <- format_time_ago(wf$created)
                tags$div(
                  class = "blockr-workflow-item",
                  onclick = shiny_input_obj_js(
                    session$ns("load_workflow"),
                    id = wf$id,
                    user = coal(wf$user, "")
                  ),
                  tags$div(
                    class = "blockr-workflow-item-content",
                    tags$div(
                      class = "blockr-workflow-name",
                      wf$name
                    ),
                    tags$div(class = "blockr-workflow-meta", wf_time)
                  ),
                  tags$a(
                    class = "blockr-open-newtab",
                    href = board_query_string(wf, backend),
                    target = "_blank",
                    onclick = "event.stopPropagation();",
                    bsicons::bs_icon("box-arrow-up-right", size = "0.75em")
                  )
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

      observeEvent(
        input$load_workflow,
        navigate_to_board(
          rack_id_from_input(backend, input$load_workflow),
          backend,
          session
        )
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
          sel <- normalize_js_input(input$delete_workflows)
          deleted <- 0
          for (wf in sel) {
            id <- rack_id_from_input(backend, wf)
            res <- tryCatch(
              {
                rack_purge(id, backend)
                deleted <- deleted + 1
                TRUE
              },
              error = function(e) {
                notify(
                  paste("Failed to delete:", coal(wf$name, wf$id,
                                                  fail_all = FALSE)),
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

      # DOWNLOAD workflows
      output$download_workflows <- downloadHandler(
        filename = function() {
          sel <- normalize_js_input(input$wf_selection)
          if (length(sel) == 1L) {
            paste0(sel[[1]]$name, ".json")
          } else {
            "workflows.zip"
          }
        },
        content = function(file) {
          sel <- normalize_js_input(input$wf_selection)
          req(length(sel) > 0L)
          tryCatch(
            file.copy(prepare_download(sel, backend), file),
            error = function(e) {
              notify(
                paste("Download failed:", conditionMessage(e)),
                type = "error",
                glue = FALSE,
                session = session
              )
            }
          )
        }
      )

      # DOWNLOAD versions
      output$download_versions <- downloadHandler(
        filename = function() {
          sel <- normalize_js_input(input$ver_selection)
          slug <- current_id()$id
          if (length(sel) == 1L) {
            paste0(slug, "_v", sel[[1]]$version, ".json")
          } else {
            paste0(slug, "_versions.zip")
          }
        },
        content = function(file) {
          sel <- normalize_js_input(input$ver_selection)
          req(length(sel) > 0L, current_id())
          slug <- current_id()$id
          dl_sel <- lapply(sel, function(v) {
            list(
              id = slug,
              name = paste0(slug, "_v", v$version),
              user = coal(v$user, ""),
              version = v$version
            )
          })
          tryCatch(
            file.copy(prepare_download(dl_sel, backend), file),
            error = function(e) {
              notify(
                paste("Download failed:", conditionMessage(e)),
                type = "error",
                glue = FALSE,
                session = session
              )
            }
          )
        }
      )

      # UPLOAD workflows
      observeEvent(
        input$upload_file,
        {
          req(input$upload_file)

          result <- tryCatch(
            upload_workflows(input$upload_file, backend),
            error = function(e) {
              list(
                ok = FALSE, uploaded = 0L,
                errors = paste("Upload failed:",
                               conditionMessage(e))
              )
            }
          )

          for (err in result$errors) {
            notify(err, type = "error", glue = FALSE, session = session)
          }

          if (result$ok) {
            notify(
              paste("Uploaded", result$uploaded, "workflow(s)"),
              type = "message",
              session = session
            )
            removeModal()
            refresh_trigger(refresh_trigger() + 1)
          }
        }
      )

      output$history_title <- renderUI(
        {
          id <- current_id()

          if (not_null(id)) {
            tags$span(board_name())
          }
        }
      )

      output$version_history <- renderUI(
        {
          refresh_trigger()

          id <- current_id()
          if (is.null(id)) {
            return(
              tags$div(
                class = "blockr-history-empty",
                "Save workflow to see history"
              )
            )
          }

          versions <- tryCatch(
            rack_info(id, backend),
            error = function(e) NULL
          )

          if (is.null(versions) || nrow(versions) == 0L) {
            return(
              tags$div(class = "blockr-history-empty", "No versions found")
            )
          }

          active_version <- parseQueryString(current_query())$version

          items <- lapply(
            seq_len(min(nrow(versions), 4)),
            function(i) {
              v <- versions[i, ]
              time_ago <- format_time_ago(v$created)
              is_current <- if (is.null(active_version)) {
                i == 1L
              } else {
                identical(v$version, active_version)
              }

              tags$div(
                class = paste(
                  "blockr-workflow-item",
                  if (is_current) "current" else ""
                ),
                onclick = if (!is_current) {
                  shiny_input_obj_js(
                    session$ns("load_version"),
                    id = id$id,
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

      observeEvent(
        input$load_version,
        {
          ver <- input$load_version
          req(coal(ver$id, ver$name, fail_all = FALSE), ver$version)

          navigate_to_board(
            rack_id_from_input(backend, ver),
            backend,
            session
          )
        }
      )

      # VIEW ALL VERSIONS modal
      observeEvent(
        input$view_all_versions,
        {
          id <- current_id()
          if (is.null(id)) {
            notify(
              "Save workflow first to see versions",
              type = "message"
            )
            return()
          }

          versions <- tryCatch(
            rack_info(id, backend),
            error = function(e) NULL
          )

          if (is.null(versions) || nrow(versions) == 0L) {
            notify("No versions found", type = "message")
            return()
          }

          show_versions_modal(id, versions, session, backend,
                              name = board_name())
        }
      )

      # Delete versions
      observeEvent(
        input$delete_versions,
        {
          req(input$delete_versions, current_id())
          id <- current_id()
          deleted <- 0
          for (version in input$delete_versions) {
            ver_id <- rack_id_from_input(
              backend,
              list(id = id$id, user = id$user, version = version)
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

      # Title edit drives the rename (it isn't coupled to a save): the
      # in-session option updates, and for a loaded record the backend's native
      # name field is written too. An unsaved board carries the name until
      # rack_create persists it on first save. `title_edit` fires on blur, so
      # it is already one event per edit (no per-keystroke debounce needed).
      observeEvent(
        input$title_edit,
        {
          name <- input$title_edit

          set_board_option_value("board_name", name, board$board, session)

          id <- current_id()

          if (not_null(id) && not_null(name) && nzchar(name)) {
            current <- tryCatch(
              rack_name(id, backend),
              error = function(e) NULL
            )
            if (!identical(current, name)) {
              tryCatch(
                rack_rename(id, backend, name),
                error = cnd_to_notif(type = "warning")
              )
            }
          }
        }
      )

      # Initialize title
      observe(
        session$sendCustomMessage("blockr-update-navbar-title", board_name())
      )

      # --- Sharing & visibility (capability-driven) ---

      capabilities <- reactive(rack_capabilities(backend))

      has_sharing <- reactive({
        caps <- capabilities()
        isTRUE(caps$sharing) || isTRUE(caps$visibility)
      })

      sharing_trigger <- reactiveVal(0)

      output$sharing_tab <- renderUI({
        req(has_sharing())
        tags$button(
          id = session$ns("tab_sharing"),
          class = "blockr-tab",
          type = "button",
          `data-panel` = session$ns("panel_sharing"),
          onclick = tab_switch_js(),
          bsicons::bs_icon("people"),
          "Sharing"
        )
      })

      output$sharing_panel <- renderUI({
        req(has_sharing())
        caps <- capabilities()

        tags$div(
          id = session$ns("panel_sharing"),
          class = paste(
            "blockr-tab-panel blockr-tab-panel-hidden",
            "blockr-sharing-panel"
          ),
          if (isTRUE(caps$visibility)) {
            tags$div(
              class = "blockr-sharing-section",
              tags$div(class = "blockr-sharing-label", "VISIBILITY"),
              uiOutput(session$ns("visibility_control"))
            )
          },
          uiOutput(session$ns("sharing_controls"))
        )
      })

      output$sharing_controls <- renderUI({
        req(identical(input$visibility_select, "acl"))
        caps <- capabilities()

        tagList(
          if (isTRUE(caps$sharing)) {
            tags$div(
              class = "blockr-sharing-section",
              tags$div(class = "blockr-sharing-label", "SHARED WITH"),
              uiOutput(session$ns("shared_users_list"))
            )
          },
          if (isTRUE(caps$user_discovery)) {
            tags$div(
              class = "blockr-sharing-section",
              tags$div(class = "blockr-sharing-label", "ADD PEOPLE"),
              tags$input(
                type = "text",
                id = session$ns("user_search_input"),
                class = "blockr-user-search",
                placeholder = "Search users...",
                oninput = sprintf(
                  "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
                  session$ns("user_search_input")
                )
              ),
              uiOutput(session$ns("user_search_results"))
            )
          }
        )
      })

      output$visibility_control <- renderUI({
        sharing_trigger()
        req(has_sharing())
        caps <- capabilities()
        req(isTRUE(caps$visibility))

        id <- current_id()
        if (is.null(id)) {
          return(
            tags$div(class = "blockr-sharing-hint", "Save workflow first")
          )
        }

        acl <- tryCatch(rack_acl(id, backend), error = function(e) "acl")

        selected <- if (identical(acl, "acl")) {
          shares <- tryCatch(
            rack_shares(id, backend),
            error = function(e) list()
          )
          if (length(shares) > 0L) "acl" else "private"
        } else {
          acl
        }

        selectInput(
          session$ns("visibility_select"),
          label = NULL,
          choices = c(
            "Private" = "private",
            "Restricted" = "acl",
            "Public" = "logged_in"
          ),
          selected = selected,
          width = "100%"
        )
      })

      output$shared_users_list <- renderUI({
        sharing_trigger()
        req(has_sharing())
        caps <- capabilities()
        req(isTRUE(caps$sharing))

        id <- current_id()
        if (is.null(id)) {
          return(
            tags$div(class = "blockr-sharing-hint", "Save workflow first")
          )
        }

        shares <- tryCatch(
          rack_shares(id, backend),
          error = function(e) list()
        )

        if (length(shares) == 0) {
          return(
            tags$div(
              class = "blockr-shared-empty",
              "Not shared with anyone"
            )
          )
        }

        tagList(lapply(shares, function(s) {
          tags$div(
            class = "blockr-shared-user",
            tags$div(
              class = "blockr-shared-user-info",
              tags$span(
                class = "blockr-shared-user-name",
                s$display_name
              )
            ),
            tags$button(
              class = "blockr-shared-user-remove",
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                session$ns("unshare_user"),
                s$principal_guid
              ),
              "Remove"
            )
          )
        }))
      })

      user_search_query <- debounce(reactive(input$user_search_input), 300)

      output$user_search_results <- renderUI({
        query <- user_search_query()
        req(query, nzchar(query))

        users <- tryCatch(
          rack_find_users(backend, query),
          error = function(e) list()
        )

        if (length(users) == 0) {
          return(
            tags$div(class = "blockr-search-empty", "No users found")
          )
        }

        tagList(lapply(users, function(u) {
          display <- paste(coal(u$first_name, ""), coal(u$last_name, ""))
          email <- coal(u$email, u$username, "")

          tags$div(
            class = "blockr-search-result",
            tags$div(
              class = "blockr-search-result-info",
              tags$span(class = "blockr-search-result-name", display),
              if (nzchar(email)) {
                tags$span(class = "blockr-search-result-email", email)
              }
            ),
            tags$button(
              class = "blockr-search-result-share",
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                session$ns("share_user"),
                u$guid
              ),
              "Share"
            )
          )
        }))
      })

      observeEvent(
        input$visibility_select,
        {
          req(current_id())
          id <- current_id()

          acl_value <- if (identical(input$visibility_select, "private")) {
            "acl"
          } else {
            input$visibility_select
          }

          tryCatch(
            {
              rack_set_acl(id, backend, acl_value)

              if (identical(input$visibility_select, "private")) {
                shares <- rack_shares(id, backend)
                for (s in shares) {
                  rack_unshare(id, backend, s$principal_guid)
                }
              }
            },
            error = cnd_to_notif(type = "error")
          )
        },
        ignoreInit = TRUE
      )

      observeEvent(
        input$share_user,
        {
          req(current_id())
          id <- current_id()
          tryCatch(
            {
              rack_share(id, backend, input$share_user)
              sharing_trigger(sharing_trigger() + 1)
              notify(
                "Shared successfully",
                type = "message",
                session = session
              )
            },
            error = cnd_to_notif(type = "error")
          )
        }
      )

      observeEvent(
        input$unshare_user,
        {
          req(current_id())
          id <- current_id()
          tryCatch(
            {
              rack_unshare(id, backend, input$unshare_user)
              sharing_trigger(sharing_trigger() + 1)
              notify(
                "Removed user",
                type = "message",
                session = session
              )
            },
            error = cnd_to_notif(type = "error")
          )
        }
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

      # core's preserve_board validator requires a reactive return; this plugin
      # resolves boards at the request phase, so nothing flows back through it.
      reactiveVal()
    }
  )
}

#' Rack-backed board loader
#'
#' A [blockr.core::board_loader()] for [blockr.core::serve()] that picks the
#' board to build from the request URL. The board named by the URL handle
#' (`board_name` / `user` / `version`) loads from the rack backend (the
#' `blockr.session_mgmt_backend` option); a request without such a handle (a
#' cold load, or a "New" board) gets a cleared copy of the served board. Pair
#' it with [manage_project()] when calling [blockr.core::serve()]:
#' `serve(board, plugins = c(.., manage_project()), loader = rack_loader())`.
#'
#' @return A [blockr.core::board_loader()] object.
#'
#' @export
rack_loader <- function() {

  resolve <- function(request, session, default) {

    # the page query is on the request at the GET (no session yet) but in the
    # session's client data at the WS connect
    search <- if (is.null(session)) {
      request[["QUERY_STRING"]]
    } else {
      isolate(session$clientData$url_search)
    }

    query <- parseQueryString(coal(search, ""))
    handle <- coal(query$id, query$board_name, fail_all = FALSE)

    if (is.null(handle) || !nzchar(handle)) {
      return(clear_board(default))
    }

    backend <- get_session_backend()

    id <- rack_id_from_input(
      backend,
      list(id = handle, user = query$user, version = query$version)
    )

    # resolve runs at both the GET (UI) and the WS connect (server), so a
    # rack-backed board is fetched and deserialized twice per page load. The
    # double fetch is deliberate: a shared cache keyed by the URL handle would
    # leak across sessions once backend credentials become visitor-scoped, and a
    # session-scoped cache cannot span the GET -> WS boundary.
    board_ser <- tryCatch(rack_load(id, backend), error = function(e) NULL)

    if (is.null(board_ser)) {
      return(clear_board(default))
    }

    loaded <- tryCatch(blockr_deser(board_ser), error = function(e) NULL)

    if (is.null(loaded)) {
      return(clear_board(default))
    }

    loaded
  }

  board_loader(resolve)
}

navigate_to_board <- function(id, backend, session) {

  updateQueryString(
    board_query_string(id, backend),
    mode = "replace",
    session = session
  )

  session$reload()
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
      wf_time <- format_time_ago(wf$created)
      tags$tr(
        class = "blockr-workflow-row",
        `data-name` = tolower(wf$name),
        `data-user` = coal(wf$user, ""),
        tags$td(
          class = "blockr-wf-checkbox",
          tags$input(
            type = "checkbox",
            class = "blockr-wf-select",
            `data-id` = wf$id,
            `data-name` = wf$name,
            `data-user` = coal(wf$user, "")
          )
        ),
        tags$td(class = "blockr-wf-name", wf$name),
        tags$td(class = "blockr-wf-time", wf_time),
        tags$td(
          class = "blockr-wf-action",
          tags$div(
            class = "blockr-wf-row-actions",
            tags$button(
              class = "btn btn-sm btn-primary",
              onclick = paste0(
                shiny_input_obj_js(
                  session$ns("load_workflow"),
                  id = wf$id,
                  user = coal(wf$user, "")
                ),
                "\n",
                hide_modal_js(session$ns("workflows_modal"))
              ),
              "Load"
            ),
            tags$a(
              class = "btn btn-sm btn-outline-secondary",
              href = board_query_string(wf, backend),
              target = "_blank",
              bsicons::bs_icon(
                "box-arrow-up-right",
                size = "0.85em"
              )
            ),
            tags$button(
              class = "btn btn-sm btn-outline-primary blockr-wf-row-btn",
              title = "Download",
              onclick = sprintf(
                "Shiny.setInputValue('%s', [{id: '%s', name: '%s',
                  user: '%s'}], {priority: 'event'});
                  setTimeout(function() {
                    document.getElementById('%s').click();
                  }, 100);",
                session$ns("wf_selection"),
                wf$id,
                wf$name,
                coal(wf$user, ""),
                session$ns("download_workflows")
              ),
              bsicons::bs_icon("download")
            ),
            tags$button(
              class = "btn btn-sm btn-outline-danger blockr-wf-row-btn",
              title = "Delete",
              onclick = sprintf(
                "if (confirm('Delete %s?')) {
                  Shiny.setInputValue('%s',
                    [{id: '%s', name: '%s', user: '%s'}],
                    {priority: 'event'});
                }",
                wf$name,
                session$ns("delete_workflows"),
                wf$id,
                wf$name,
                coal(wf$user, "")
              ),
              bsicons::bs_icon("trash")
            )
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
    search_id = session$ns("workflow_search"),
    download_btn_id = session$ns("download_workflows_btn"),
    selection_input_id = session$ns("wf_selection")
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
            tags$div(
              id = session$ns("download_workflows_btn"),
              downloadButton(
                session$ns("download_workflows"),
                label = "Download",
                class = "btn-sm btn-primary"
              )
            ),
            tags$div(
              class = "blockr-wf-upload-compact",
              fileInput(
                session$ns("upload_file"),
                label = NULL,
                accept = ".json",
                multiple = TRUE,
                buttonLabel = "Upload",
                placeholder = NULL
              )
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

show_versions_modal <- function(id, versions, session, backend, name = NULL) {

  active_version <- getQueryString(session)$version

  rows <- lapply(
    seq_len(nrow(versions)),
    function(i) {
      v <- versions[i, ]
      time_ago <- format_time_ago(v$created)
      is_current <- if (is.null(active_version)) {
        i == 1L
      } else {
        identical(v$version, active_version)
      }

      tags$tr(
        class = "blockr-workflow-row",
        tags$td(
          class = "blockr-wf-checkbox",
          tags$input(
            type = "checkbox",
            class = "blockr-version-select",
            value = v$version,
            `data-version` = v$version,
            `data-user` = coal(id$user, ""),
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
          tags$div(
            class = "blockr-wf-row-actions",
            if (!is_current) {
              tags$button(
                class = "btn btn-sm btn-primary",
                onclick = paste0(
                  shiny_input_obj_js(
                    session$ns("load_version"),
                    id = id$id,
                    user = coal(id$user, ""),
                    version = v$version
                  ),
                  "\n",
                  hide_modal_js(session$ns("versions_modal"))
                ),
                "Load"
              )
            },
            tags$a(
              class = "btn btn-sm btn-outline-secondary",
              href = board_query_string(
                list(
                  id = id$id,
                  user = id$user,
                  version = v$version
                ),
                backend
              ),
              target = "_blank",
              bsicons::bs_icon(
                "box-arrow-up-right",
                size = "0.85em"
              )
            ),
            tags$button(
              class = paste(
                "btn btn-sm btn-outline-primary blockr-wf-row-btn"
              ),
              title = "Download",
              onclick = sprintf(
                "Shiny.setInputValue('%s',
                  [{version: '%s', user: '%s'}],
                  {priority: 'event'});
                  setTimeout(function() {
                    document.getElementById('%s').click();
                  }, 100);",
                session$ns("ver_selection"),
                v$version,
                coal(id$user, ""),
                session$ns("download_versions")
              ),
              bsicons::bs_icon("download")
            ),
            if (!is_current) {
              tags$button(
                class = paste(
                  "btn btn-sm btn-outline-danger blockr-wf-row-btn"
                ),
                title = "Delete",
                onclick = sprintf(
                  "if (confirm('Delete this version?')) {
                    Shiny.setInputValue('%s',
                      ['%s'],
                      {priority: 'event'});
                  }",
                  session$ns("delete_versions"),
                  v$version
                ),
                bsicons::bs_icon("trash")
              )
            }
          )
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
    has_disabled = TRUE,
    download_btn_id = session$ns("download_versions_btn"),
    selection_input_id = session$ns("ver_selection")
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
          tags$h5(paste("Version History:", coal(name, id$id,
                                                 fail_all = FALSE))),
          tags$div(
            class = "blockr-wf-header-actions",
            tags$button(
              id = session$ns("delete_versions_btn"),
              class = "btn btn-sm btn-outline-danger",
              style = "display: none;",
              "Delete"
            ),
            tags$div(
              id = session$ns("download_versions_btn"),
              downloadButton(
                session$ns("download_versions"),
                label = "Download",
                class = "btn-sm btn-primary"
              )
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
                           search_id = NULL, has_disabled = FALSE,
                           download_btn_id = NULL,
                           selection_input_id = NULL) {

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

  # Download button wrapper visibility + selection sync
  if (!is.null(download_btn_id)) {
    download_visibility <- sprintf(
      "var dlWrap = document.getElementById('%s');
      if (selected.length > 0) {
        dlWrap.style.visibility = 'visible';
        dlWrap.style.position = '';
      } else {
        dlWrap.style.visibility = 'hidden';
        dlWrap.style.position = 'absolute';
      }
      var dlLink = dlWrap.querySelector('a');
      if (dlLink) dlLink.textContent =
        'Download (' + selected.length + ')';",
      download_btn_id
    )
  } else {
    download_visibility <- ""
  }

  # Sync selection to Shiny input for downloadHandler
  if (!is.null(selection_input_id)) {
    selection_sync <- sprintf(
      "var selData = [];
      selected.forEach(function(cb) {
        var item = {};
        for (var key in cb.dataset) {
          item[key] = cb.dataset[key];
        }
        selData.push(item);
      });
      Shiny.setInputValue('%s', selData, {priority: 'event'});",
      selection_input_id
    )
  } else {
    selection_sync <- ""
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
      %s
      %s
    }

    document.querySelectorAll('.%s').forEach(function(cb) {
      cb.addEventListener('change', updateDeleteBtn);
    });

    document.getElementById('%s').addEventListener('click', function() {
      var selected = [];
      document.querySelectorAll('.%s:checked').forEach(function(cb) {
        var item = cb.dataset && cb.dataset.name
          ? {
              id: cb.dataset.id,
              name: cb.dataset.name,
              user: cb.dataset.user || ''
            }
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
    download_visibility,
    selection_sync,
    checkbox_class,
    delete_btn_id,
    checkbox_class,
    item_type,
    delete_input_id
  )
}
