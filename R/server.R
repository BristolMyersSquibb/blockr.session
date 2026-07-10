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

      backend <- get_session_backend(session)

      refresh_trigger <- reactiveVal(0)
      save_status <- reactiveVal("Not saved")

      serialize_now <- function(id = board$board_id) {
        do.call(
          serialize_board,
          c(
            list(board$board, board$blocks, id),
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
            sprintf("Successfully saved %s (%s)", board_name(), res$id),
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
        input$save_as_btn,
        {
          new_id <- rand_names()

          data <- tryCatch(
            serialize_now(new_id),
            error = cnd_to_notif(type = "error")
          )

          if (is.null(data)) {
            return()
          }

          res <- tryCatch(
            rack_create(
              backend, data,
              id = new_id, name = board_name()
            ),
            error = cnd_to_notif(type = "error")
          )

          if (is.null(res)) {
            return()
          }

          forked <- rack_id_from_input(
            backend,
            list(id = res$id, user = res$user)
          )

          navigate_to_board(forked, backend, session)
        }
      )

      observeEvent(
        input$new_btn,
        {
          updateQueryString("?", mode = "replace", session = session)
          session$reload()
        }
      )

      all_workflows <- reactive({
        refresh_trigger()
        tryCatch(rack_list(backend), error = function(e) list())
      })

      workflow_query <- debounce(
        reactive(coal(input$workflow_filter, "")),
        250
      )

      filtered_workflows <- reactive(
        search_workflows(all_workflows(), workflow_query())
      )

      workflow_batch <- 10L
      n_shown <- reactiveVal(workflow_batch)

      observeEvent(
        list(workflow_query(), refresh_trigger()),
        n_shown(workflow_batch)
      )

      observeEvent(
        input$workflow_load_more,
        {
          total <- length(filtered_workflows())
          n_shown(min(n_shown() + workflow_batch, total))
        }
      )

      output$workflow_count <- renderText(
        {
          total <- length(all_workflows())

          if (total == 0L) {
            return("")
          }

          if (nzchar(trimws(workflow_query()))) {
            paste0(length(filtered_workflows()), " / ", total)
          } else {
            as.character(total)
          }
        }
      )

      output$recent_workflows <- renderUI(
        {
          if (length(all_workflows()) == 0L) {
            return(
              tags$div(class = "blockr-workflow-empty", "No saved workflows")
            )
          }

          matches <- filtered_workflows()

          if (length(matches) == 0L) {
            return(
              tags$div(
                class = "blockr-workflow-noresults",
                "No workflows match your search"
              )
            )
          }

          shown <- matches[seq_len(min(n_shown(), length(matches)))]

          tagList(
            lapply(shown, workflow_item, backend, session$ns),
            if (length(matches) > length(shown)) {
              tags$div(
                class = "blockr-workflow-sentinel",
                `data-input-id` = session$ns("workflow_load_more"),
                tags$div(class = "blockr-workflow-spinner")
              )
            }
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

      # MANAGE WORKFLOWS modal: a server-side window of the filtered list, with
      # selection tracked server-side so select-all and delete act over the
      # whole filtered set rather than the loaded rows.

      modal_query <- debounce(
        reactive(coal(input$modal_workflow_filter, "")),
        250
      )

      modal_filtered <- reactive(
        search_workflows(all_workflows(), modal_query())
      )

      modal_n_shown <- reactiveVal(workflow_batch)
      modal_selection <- reactiveVal(character())
      modal_open <- reactiveVal(FALSE)
      modal_expanded <- reactiveVal(character())
      modal_focus <- reactiveVal(NULL)

      # Version history for the expanded rows. Recomputes only when the expanded
      # set or the backend contents change -- not on scroll or filter -- so the
      # windowed table renders sub-rows without a backend hit per render.
      expanded_versions <- reactive({

        refresh_trigger()

        recs <- modal_selected_records(modal_expanded(), all_workflows())

        set_names(lapply(recs, record_versions, backend), chr_xtr(recs, "id"))
      })

      observeEvent(
        input$view_all_workflows,
        {
          if (length(all_workflows()) == 0L) {
            notify("No saved workflows", type = "message")
            return()
          }

          modal_n_shown(workflow_batch)
          modal_selection(character())
          modal_expanded(character())
          modal_focus(NULL)
          modal_open(TRUE)

          show_workflows_modal(session)
        }
      )

      observeEvent(input$modal_closed, modal_open(FALSE))

      observeEvent(
        modal_query(),
        {
          modal_n_shown(workflow_batch)
          modal_selection(character())
          modal_focus(NULL)
        }
      )

      observeEvent(
        input$modal_toggle_expand,
        {
          id <- input$modal_toggle_expand$id
          cur <- modal_expanded()

          if (id %in% cur) {
            modal_expanded(setdiff(cur, id))
          } else {
            modal_expanded(c(cur, id))
          }
        }
      )

      observeEvent(
        input$modal_load_more,
        {
          total <- length(modal_filtered())
          modal_n_shown(min(modal_n_shown() + workflow_batch, total))
        }
      )

      observeEvent(
        input$modal_toggle,
        {
          tog <- input$modal_toggle

          if (isTRUE(tog$checked)) {
            modal_selection(union(modal_selection(), tog$id))
          } else {
            modal_selection(setdiff(modal_selection(), tog$id))
          }
        }
      )

      observeEvent(
        input$modal_select_all,
        {
          if (isTRUE(input$modal_select_all$checked)) {
            modal_selection(chr_xtr(modal_filtered(), "id"))
          } else {
            modal_selection(character())
          }
        }
      )

      observe(
        {
          if (!isTRUE(modal_open())) {
            return()
          }

          session$sendCustomMessage(
            "blockr-modal-selection",
            list(
              count = length(
                modal_selected_records(modal_selection(), all_workflows())
              ),
              total = length(modal_filtered())
            )
          )
        }
      )

      observeEvent(
        input$modal_delete,
        {
          records <- modal_selected_records(modal_selection(), all_workflows())
          req(length(records) > 0L)

          deleted <- delete_rack_records(records, backend, session)

          if (deleted > 0L) {
            notify(paste("Deleted", deleted, "workflow(s)"), type = "message")
            modal_selection(character())
            refresh_trigger(refresh_trigger() + 1)
          }
        }
      )

      output$modal_workflow_count <- renderText(
        {
          total <- length(all_workflows())

          if (total == 0L) {
            return("")
          }

          if (nzchar(trimws(modal_query()))) {
            paste0(length(modal_filtered()), " / ", total)
          } else {
            as.character(total)
          }
        }
      )

      output$workflows_modal_rows <- renderUI(
        {
          if (length(all_workflows()) == 0L) {
            return(modal_message_row("No saved workflows"))
          }

          matches <- modal_filtered()

          if (length(matches) == 0L) {
            return(modal_message_row("No workflows match your search"))
          }

          matches <- float_to_front(matches, modal_focus())

          shown <- matches[seq_len(min(modal_n_shown(), length(matches)))]

          active_version <- parseQueryString(current_query())$version

          rows <- lapply(
            shown, modal_row_with_history,
            modal_expanded(), expanded_versions(), current_id(),
            active_version, isolate(modal_selection()), backend, session$ns
          )

          tagList(
            rows,
            if (length(matches) > length(shown)) {
              modal_sentinel_row(session$ns("modal_load_more"))
            }
          )
        }
      )

      output$download_selected <- downloadHandler(
        filename = function() {
          records <- modal_selected_records(modal_selection(), all_workflows())
          if (length(records) == 1L) {
            paste0(records[[1L]]$name, ".json")
          } else {
            "workflows.zip"
          }
        },
        content = function(file) {
          records <- modal_selected_records(modal_selection(), all_workflows())
          req(length(records) > 0L)

          sel <- lapply(
            records,
            function(wf) {
              list(id = wf$id, name = wf$name, user = coal(wf$user, ""))
            }
          )

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

      observeEvent(
        input$delete_workflows,
        {
          req(input$delete_workflows)

          records <- normalize_js_input(input$delete_workflows)
          deleted <- delete_rack_records(records, backend, session)

          if (deleted > 0) {
            notify(paste("Deleted", deleted, "workflow(s)"), type = "message")
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
          slug <- if (length(sel)) sel[[1]]$id else "workflow"
          if (length(sel) == 1L) {
            paste0(slug, "_v", sel[[1]]$version, ".json")
          } else {
            paste0(slug, "_versions.zip")
          }
        },
        content = function(file) {
          sel <- normalize_js_input(input$ver_selection)
          req(length(sel) > 0L)
          dl_sel <- lapply(sel, function(v) {
            list(
              id = v$id,
              name = paste0(v$id, "_v", v$version),
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
              is_current <- version_is_current(i, v$version, active_version)

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
                tags$div(
                  class = "blockr-workflow-meta",
                  tags$span(class = "blockr-wf-version-id", v$hash),
                  if (is_current) " (Current)"
                )
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

      # VIEW ALL VERSIONS: open the workflows overview with the loaded record
      # expanded in place. History lives inline in the one modal, so a corrupted
      # latest version stays recoverable without loading the record.
      observeEvent(
        input$view_all_versions,
        {
          id <- current_id()

          if (is.null(id)) {
            notify("Save workflow first to see versions", type = "message")
            return()
          }

          modal_n_shown(workflow_batch)
          modal_selection(character())
          modal_expanded(id$id)
          modal_focus(id$id)
          modal_open(TRUE)

          show_workflows_modal(session)
        }
      )

      # Delete a single version from the record named in the payload -- each
      # inline row carries its own id, since several can be open at once.
      observeEvent(
        input$delete_versions,
        {
          del <- input$delete_versions
          req(del$id, del$version)

          ver_id <- rack_id_from_input(
            backend,
            list(id = del$id, user = del$user, version = del$version)
          )

          ok <- tryCatch(
            {
              rack_delete(ver_id, backend)
              TRUE
            },
            error = function(e) {
              notify(paste("Failed to delete version:", del$version),
                     type = "error")
              FALSE
            }
          )

          if (isTRUE(ok)) {
            notify("Deleted version", type = "message")
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
#' (`board_name` / `user` / `version`) loads from the `session_mgmt_backend`
#' backend; a request without such a handle (a cold load, or a "New" board)
#' gets a cleared copy of the served board. The backend is resolved with the
#' loader's own request (at the GET, before any session) or session (at the WS
#' connect), so a user-scoped backend such as [user_pins_board()] resolves
#' under the *visitor's* own Posit Connect credentials at both phases; a board
#' the visitor may not read does not resolve, whatever the `user` / `board_name`
#' in the URL. Pair it with [manage_project()] when calling
#' [blockr.core::serve()]:
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

    backend <- get_session_backend(
      if (is.null(session)) list(request = request) else session
    )

    id <- rack_id_from_input(
      backend,
      list(id = handle, user = query$user, version = query$version)
    )

    # resolve runs at both the GET (UI) and the WS connect (server), so a
    # rack-backed board is fetched and deserialized twice per page load. The
    # double fetch is deliberate: a shared cache keyed by the URL handle would
    # leak across sessions when backend credentials are visitor-scoped, and a
    # session-scoped cache cannot span the GET -> WS boundary.
    board_ser <- tryCatch(
      rack_load(id, backend),
      rack_load_invalid_tags = function(e) {
        refuse_incompatible_load(e, session)
        NULL
      },
      error = function(e) NULL
    )

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

# A URL may point at a pin that is not a blockr workflow -- directly, or because
# the unconfigured listing shows every pin. Loading one aborts before the
# deserializer; warn the visitor at the WS phase (where a session exists) rather
# than silently handing back a cleared board.
refuse_incompatible_load <- function(cnd, session) {

  if (is.null(session)) {
    return(invisible())
  }

  notify(
    conditionMessage(cnd),
    type = "warning",
    glue = FALSE,
    session = session
  )
}

search_workflows <- function(workflows, query) {

  query <- tolower(trimws(query))

  if (!nzchar(query)) {
    return(workflows)
  }

  Filter(
    function(wf) grepl(query, tolower(coal(wf$name, "")), fixed = TRUE),
    workflows
  )
}

workflow_item <- function(wf, backend, ns) {

  tags$div(
    class = "blockr-workflow-item",
    onclick = shiny_input_obj_js(
      ns("load_workflow"),
      id = wf$id,
      user = coal(wf$user, "")
    ),
    tags$div(
      class = "blockr-workflow-item-content",
      tags$div(class = "blockr-workflow-name", wf$name),
      tags$div(class = "blockr-workflow-meta", record_time_ago(wf))
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

modal_selected_records <- function(ids, workflows) {
  Filter(function(wf) wf$id %in% ids, workflows)
}

delete_rack_records <- function(records, backend, session) {

  deleted <- 0L

  for (wf in records) {
    ok <- tryCatch(
      {
        rack_purge(rack_id_from_input(backend, wf), backend)
        TRUE
      },
      error = function(e) {
        notify(
          paste("Failed to delete:", coal(wf$name, wf$id, fail_all = FALSE)),
          type = "error",
          session = session
        )
        FALSE
      }
    )

    if (ok) {
      deleted <- deleted + 1L
    }
  }

  deleted
}

workflow_modal_row <- function(wf, selected, backend, ns, expanded = FALSE) {

  tags$tr(
    class = paste0("blockr-workflow-row", if (expanded) " expanded"),
    tags$td(
      class = "blockr-wf-expand-cell",
      tags$button(
        class = "btn btn-sm blockr-wf-expand",
        title = "Version history",
        `aria-expanded` = if (expanded) "true" else "false",
        onclick = shiny_input_obj_js(
          ns("modal_toggle_expand"),
          id = wf$id,
          user = coal(wf$user, "")
        ),
        bsicons::bs_icon("chevron-right")
      )
    ),
    tags$td(
      class = "blockr-wf-checkbox",
      tags$input(
        type = "checkbox",
        class = "blockr-wf-select",
        `data-id` = wf$id,
        checked = if (wf$id %in% selected) "checked"
      )
    ),
    tags$td(class = "blockr-wf-name", wf$name),
    tags$td(class = "blockr-wf-time", record_time_ago(wf)),
    tags$td(
      class = "blockr-wf-action",
      tags$div(
        class = "blockr-wf-row-actions",
        tags$button(
          class = "btn btn-sm btn-primary",
          onclick = paste0(
            shiny_input_obj_js(
              ns("load_workflow"),
              id = wf$id,
              user = coal(wf$user, "")
            ),
            "\n",
            hide_modal_js(ns("workflows_modal"))
          ),
          "Load"
        ),
        tags$a(
          class = "btn btn-sm btn-outline-secondary",
          href = board_query_string(wf, backend),
          target = "_blank",
          bsicons::bs_icon("box-arrow-up-right", size = "0.85em")
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
            ns("wf_selection"),
            wf$id,
            wf$name,
            coal(wf$user, ""),
            ns("download_workflows")
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
            ns("delete_workflows"),
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

modal_sentinel_row <- function(input_id) {
  tags$tr(
    class = "blockr-wf-modal-sentinel",
    `data-input-id` = input_id,
    tags$td(
      colspan = "5",
      tags$div(
        class = "blockr-wf-modal-loader",
        tags$div(class = "blockr-workflow-spinner")
      )
    )
  )
}

modal_message_row <- function(text) {
  tags$tr(
    tags$td(class = "blockr-wf-modal-message", colspan = "5", text)
  )
}

show_workflows_modal <- function(session) {

  ns <- session$ns

  showModal(
    modalDialog(
      title = NULL,
      size = "l",
      easyClose = TRUE,
      footer = NULL,
      tags$div(
        id = ns("workflows_modal"),
        class = "blockr-workflows-modal blockr-wf-manage-modal",
        `data-toggle-input` = ns("modal_toggle"),
        `data-select-all-input` = ns("modal_select_all"),
        `data-filter-input` = ns("modal_workflow_filter"),
        `data-delete-input` = ns("modal_delete"),
        `data-closed-input` = ns("modal_closed"),
        `data-delete-btn` = ns("delete_selected_btn"),
        `data-download-wrap` = ns("download_selected_wrap"),
        tags$div(
          class = "blockr-wf-header",
          tags$h5("Manage Workflows"),
          tags$div(
            class = "blockr-wf-header-actions",
            tags$button(
              id = ns("delete_selected_btn"),
              class = "btn btn-sm btn-outline-danger",
              style = "display: none;",
              "Delete"
            ),
            tags$div(
              id = ns("download_selected_wrap"),
              style = "visibility: hidden; position: absolute;",
              downloadButton(
                ns("download_selected"),
                label = "Download",
                class = "btn-sm btn-primary"
              )
            ),
            tags$div(
              class = "blockr-wf-upload-compact",
              fileInput(
                ns("upload_file"),
                label = NULL,
                accept = ".json",
                multiple = TRUE,
                buttonLabel = "Upload",
                placeholder = NULL
              )
            ),
            tags$div(
              class = "blockr-wf-search-wrap",
              tags$input(
                type = "text",
                id = ns("modal_workflow_filter"),
                class = "blockr-wf-search",
                placeholder = "Search workflows...",
                autocomplete = "off",
                oninput = sprintf(
                  "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
                  ns("modal_workflow_filter")
                )
              ),
              tags$span(
                class = "blockr-wf-count",
                textOutput(ns("modal_workflow_count"), inline = TRUE)
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
                tags$th(class = "blockr-wf-expand-cell"),
                tags$th(
                  class = "blockr-wf-checkbox",
                  tags$input(
                    type = "checkbox",
                    class = "blockr-wf-select-all"
                  )
                ),
                tags$th("Name"),
                tags$th("Last Modified"),
                tags$th("")
              )
            ),
            uiOutput(ns("workflows_modal_rows"), container = tags$tbody)
          )
        ),
        tags$div(
          style = "display: none;",
          downloadButton(ns("download_workflows"), label = NULL),
          downloadButton(ns("download_versions"), label = NULL)
        )
      )
    )
  )
}

same_record <- function(x, y) {

  if (is.null(x) || is.null(y)) {
    return(FALSE)
  }

  identical(x$id, y$id) &&
    identical(
      coal(x$user, "", fail_all = FALSE),
      coal(y$user, "", fail_all = FALSE)
    )
}

# When the history is not the loaded board, no row is "current": the URL's
# ?version= describes the loaded board, and "current" also disables Load/Delete.
version_is_current <- function(i, version, active_version, is_active = TRUE) {

  if (!is_active) {
    return(FALSE)
  }

  if (is.null(active_version)) {
    return(i == 1L)
  }

  identical(version, active_version)
}

record_versions <- function(wf, backend) {
  tryCatch(
    rack_info(rack_id_from_input(backend, wf), backend),
    error = function(e) NULL
  )
}

float_to_front <- function(workflows, id) {

  if (is.null(id)) {
    return(workflows)
  }

  is_focus <- chr_xtr(workflows, "id") == id

  c(workflows[is_focus], workflows[!is_focus])
}

modal_row_with_history <- function(wf, expanded, versions, loaded,
                                   active_version, selected, backend, ns) {

  row <- workflow_modal_row(
    wf, selected, backend, ns,
    expanded = wf$id %in% expanded
  )

  if (!(wf$id %in% expanded)) {
    return(row)
  }

  is_active <- same_record(rack_id_from_input(backend, wf), loaded)

  tagList(
    row,
    version_subrows(
      wf, versions[[wf$id]], is_active,
      if (is_active) active_version, backend, ns
    )
  )
}

version_subrows <- function(wf, versions, is_active, active_version, backend,
                            ns) {

  if (is.null(versions) || nrow(versions) == 0L) {
    return(
      tags$tr(
        class = "blockr-wf-subrow",
        tags$td(
          colspan = "5",
          tags$div(class = "blockr-wf-subrow-empty", "No versions found")
        )
      )
    )
  }

  lapply(
    seq_len(nrow(versions)),
    function(i) {
      version_subrow(wf, versions[i, ], i, is_active, active_version, backend,
                     ns)
    }
  )
}

version_subrow <- function(wf, v, i, is_active, active_version, backend, ns) {

  is_current <- version_is_current(i, v$version, active_version, is_active)
  user <- coal(wf$user, "")

  tags$tr(
    class = "blockr-workflow-row blockr-wf-subrow",
    tags$td(class = "blockr-wf-expand-cell"),
    tags$td(class = "blockr-wf-checkbox"),
    tags$td(
      class = "blockr-wf-name blockr-wf-subrow-name",
      tags$span(class = "blockr-wf-version-id", v$hash),
      if (is_current) {
        tags$span(class = "blockr-version-badge", "(Current)")
      }
    ),
    tags$td(class = "blockr-wf-time", format_time_ago(v$created)),
    tags$td(
      class = "blockr-wf-action",
      tags$div(
        class = "blockr-wf-row-actions",
        if (is_current) {
          tags$span(class = "btn btn-sm blockr-wf-slot-hidden", "Load")
        } else {
          tags$button(
            class = "btn btn-sm btn-primary",
            onclick = paste0(
              shiny_input_obj_js(
                ns("load_version"),
                id = wf$id, user = user, version = v$version
              ),
              "\n",
              hide_modal_js(ns("workflows_modal"))
            ),
            "Load"
          )
        },
        tags$a(
          class = "btn btn-sm btn-outline-secondary",
          href = board_query_string(
            list(id = wf$id, user = wf$user, version = v$version),
            backend
          ),
          target = "_blank",
          bsicons::bs_icon("box-arrow-up-right", size = "0.85em")
        ),
        tags$button(
          class = "btn btn-sm btn-outline-primary blockr-wf-row-btn",
          title = "Download",
          onclick = sprintf(
            "Shiny.setInputValue('%s',
              [{id: '%s', version: '%s', user: '%s'}],
              {priority: 'event'});
              setTimeout(function() {
                document.getElementById('%s').click();
              }, 100);",
            ns("ver_selection"), wf$id, v$version, user,
            ns("download_versions")
          ),
          bsicons::bs_icon("download")
        ),
        if (is_current) {
          tags$span(
            class = "btn btn-sm blockr-wf-row-btn blockr-wf-slot-hidden"
          )
        } else {
          tags$button(
            class = "btn btn-sm btn-outline-danger blockr-wf-row-btn",
            title = "Delete",
            onclick = sprintf(
              "if (confirm('Delete this version?')) {
                Shiny.setInputValue('%s',
                  {id: '%s', user: '%s', version: '%s'},
                  {priority: 'event'});
              }",
              ns("delete_versions"), wf$id, user, v$version
            ),
            bsicons::bs_icon("trash")
          )
        }
      )
    )
  )
}
