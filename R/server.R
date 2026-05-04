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

      use_url <- url_params_enabled()

      prev_query <- reactiveVal(isolate(board$reload_meta$url))

      log_info(
        "[RELOAD-DEBUG] manage_project_server init | ",
        "session: {substr(session$token, 1, 8)} | ",
        "use_url: {use_url} | ",
        "prev_query: {coal(isolate(board$reload_meta$url), '(null)')}"
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
        session$clientData$url_search,
        {
          log_info(
            "[RELOAD-DEBUG] URL observer fired | ",
            "session: {substr(session$token, 1, 8)} | ",
            "use_url: {use_url} | ",
            "url_search: {coal(session$clientData$url_search, '(empty)')}"
          )

          if (!use_url) {
            log_info(
              "[RELOAD-DEBUG] URL observer: use_url=FALSE, returning | ",
              "session: {substr(session$token, 1, 8)}"
            )
            return()
          }

          query <- getQueryString(session)

          if (is.null(query$board_name)) {
            log_info(
              "[RELOAD-DEBUG] URL observer: no board_name, returning | ",
              "session: {substr(session$token, 1, 8)}"
            )
            return()
          }

          id <- rack_id_from_input(
            list(
              name = query$board_name,
              user = query$user,
              version = query$version
            ),
            backend
          )

          new_url <- board_query_string(id, backend)

          # Skip if the URL matches what we last set ourselves. This prevents
          # re-triggering after our own updateQueryString calls and also handles
          # the post-session$reload() case (prev_query is initialized from the
          # pkg-level reload state that persists across session reloads).
          if (identical(new_url, prev_query())) {
            log_info(
              "[RELOAD-DEBUG] URL observer: guard MATCHED, skipping | ",
              "session: {substr(session$token, 1, 8)} | ",
              "url: {new_url}"
            )
            set_board_option_value("board_name", query$board_name, session)
            return()
          }

          log_info(
            "[RELOAD-DEBUG] URL observer: guard MISSED, will restore | ",
            "session: {substr(session$token, 1, 8)} | ",
            "new_url: {new_url} | ",
            "prev_query: {coal(prev_query(), '(null)')}"
          )

          board_ser <- tryCatch(
            rack_load(id, backend),
            error = cnd_to_notif(type = "error")
          )

          if (is.null(board_ser)) return()

          prev_query(new_url)

          ok <- safe_restore_board(
            board$board, board_ser, restore_result,
            meta = list(url = new_url), session = session
          )

          if (ok) {
            log_info(
              "[RELOAD-DEBUG] URL observer: restore OK, will reload | ",
              "session: {substr(session$token, 1, 8)}"
            )
            set_board_option_value("board_name", query$board_name, session)
            updateQueryString(new_url, mode = "replace", session = session)
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

            new_url <- board_query_string(
              rack_id_for_board(board_name(), backend),
              backend
            )
            prev_query(new_url)

            if (use_url) {
              updateQueryString(
                new_url, mode = "replace", session = session
              )
            }
          }
        }
      )

      observeEvent(
        input$new_btn,
        {
          new <- clear_board(board$board)
          new_id <- rand_names()
          attr(new, "id") <- new_id
          new <- reset_board_name(new, id_to_sentence_case(new_id))
          restore_result(new)

          if (use_url) {
            updateQueryString("?", mode = "replace", session = session)
          }
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
                    class = "blockr-workflow-item-content",
                    tags$div(
                      class = "blockr-workflow-name",
                      display_name(wf)
                    ),
                    tags$div(class = "blockr-workflow-meta", wf_time)
                  ),
                  if (use_url) {
                    tags$a(
                      class = "blockr-open-newtab",
                      href = board_query_string(wf, backend),
                      target = "_blank",
                      onclick = "event.stopPropagation();",
                      bsicons::bs_icon(
                        "box-arrow-up-right", size = "0.75em"
                      )
                    )
                  }
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

      # LOAD workflow — navigate to URL, preload handles the rest
      observeEvent(
        input$load_workflow,
        {
          id <- rack_id_from_input(input$load_workflow)
          new_url <- board_query_string(id, backend)
          log_info(
            "[RELOAD-DEBUG] load_workflow: navigating | ",
            "session: {substr(session$token, 1, 8)} | ",
            "url: {new_url}"
          )
          navigate_to(new_url, session)
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

          show_workflows_modal(workflows, backend, session, use_url)
        }
      )

      observeEvent(
        input$delete_workflows,
        {
          req(input$delete_workflows)
          sel <- normalize_js_input(input$delete_workflows)
          deleted <- 0
          for (wf in sel) {
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
          name <- board_name()
          if (length(sel) == 1L) {
            paste0(name, "_v", sel[[1]]$version, ".json")
          } else {
            paste0(name, "_versions.zip")
          }
        },
        content = function(file) {
          sel <- normalize_js_input(input$ver_selection)
          req(length(sel) > 0L)
          name <- board_name()
          dl_sel <- lapply(sel, function(v) {
            list(
              name = paste0(name, "_v", v$version),
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
            notify(err, type = "error", session = session)
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

          active_version <- parseQueryString(coal(prev_query(), ""))$version

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
                    name = id$name,
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

      # Load specific version — navigate to URL, preload handles the rest
      observeEvent(
        input$load_version,
        {
          req(input$load_version$name, input$load_version$version)

          id <- rack_id_from_input(input$load_version)
          new_url <- board_query_string(id, backend)
          log_info(
            "[RELOAD-DEBUG] load_version: navigating | ",
            "session: {substr(session$token, 1, 8)} | ",
            "url: {new_url}"
          )
          navigate_to(new_url, session)
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

          show_versions_modal(id, versions, session, backend, use_url)
        }
      )

      # VIEW VERSIONS for a workflow picked from the workflows list
      # (does not require the workflow to be loaded — supports recovering
      # from corrupted-latest by loading an older version directly)
      observeEvent(
        input$view_versions_for,
        {
          req(input$view_versions_for$name)

          id <- rack_id_from_input(input$view_versions_for, backend)

          versions <- tryCatch(
            rack_info(id, backend),
            error = function(e) NULL
          )

          if (is.null(versions) || nrow(versions) == 0L) {
            notify("No versions found", type = "message")
            return()
          }

          loaded <- board_name()
          match_active <- !is.null(loaded) &&
            nzchar(loaded) &&
            identical(sanitize_pin_name(loaded), id$name)

          show_versions_modal(
            id, versions, session, backend, use_url,
            match_active = match_active
          )
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

        name <- board_name()
        if (is.null(name) || !nzchar(name)) {
          return(
            tags$div(class = "blockr-sharing-hint", "Save workflow first")
          )
        }

        id <- rack_id_for_board(name, backend)
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

        name <- board_name()
        if (is.null(name) || !nzchar(name)) {
          return(
            tags$div(class = "blockr-sharing-hint", "Save workflow first")
          )
        }

        id <- rack_id_for_board(name, backend)
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
          req(board_name())
          id <- rack_id_for_board(board_name(), backend)

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
          req(board_name())
          id <- rack_id_for_board(board_name(), backend)
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
          req(board_name())
          id <- rack_id_for_board(board_name(), backend)
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

show_workflows_modal <- function(workflows, backend, session, use_url) {

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
          tags$div(
            class = "blockr-wf-row-actions",
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
            ),
            if (use_url) {
              tags$a(
                class = "btn btn-sm btn-outline-secondary",
                href = board_query_string(wf, backend),
                target = "_blank",
                bsicons::bs_icon(
                  "box-arrow-up-right",
                  size = "0.85em"
                )
              )
            },
            tags$button(
              class = "btn btn-sm btn-outline-secondary blockr-wf-row-btn",
              title = "Version history",
              onclick = paste0(
                shiny_input_obj_js(
                  session$ns("view_versions_for"),
                  name = display_name(wf),
                  user = coal(wf$user, "")
                ),
                "\n",
                hide_modal_js(session$ns("workflows_modal"))
              ),
              bsicons::bs_icon("clock-history")
            ),
            tags$button(
              class = "btn btn-sm btn-outline-primary blockr-wf-row-btn",
              title = "Download",
              onclick = sprintf(
                "Shiny.setInputValue('%s', [{name: '%s',
                  user: '%s'}], {priority: 'event'});
                  setTimeout(function() {
                    document.getElementById('%s').click();
                  }, 100);",
                session$ns("wf_selection"),
                display_name(wf),
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
                    [{name: '%s', user: '%s'}],
                    {priority: 'event'});
                }",
                display_name(wf),
                session$ns("delete_workflows"),
                display_name(wf),
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

show_versions_modal <- function(id, versions, session, backend, use_url,
                                match_active = TRUE) {

  active_version <- if (match_active) getQueryString(session)$version else NULL

  rows <- lapply(
    seq_len(nrow(versions)),
    function(i) {
      v <- versions[i, ]
      time_ago <- format_time_ago(v$created)
      is_current <- if (!match_active) {
        FALSE
      } else if (is.null(active_version)) {
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
                    name = id$name,
                    user = coal(id$user, ""),
                    version = v$version
                  ),
                  "\n",
                  hide_modal_js(session$ns("versions_modal"))
                ),
                "Load"
              )
            },
            if (use_url) {
              tags$a(
                class = "btn btn-sm btn-outline-secondary",
                href = board_query_string(
                  rack_id_from_input(
                    list(
                      name = id$name,
                      user = id$user,
                      version = v$version
                    ),
                    backend
                  ),
                  backend
                ),
                target = "_blank",
                bsicons::bs_icon(
                  "box-arrow-up-right",
                  size = "0.85em"
                )
              )
            },
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
          tags$h5(paste("Version History:", display_name(id))),
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
    download_visibility,
    selection_sync,
    checkbox_class,
    delete_btn_id,
    checkbox_class,
    item_type,
    delete_input_id
  )
}
