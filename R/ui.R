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
          `data-bs-auto-close` = "outside",
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
              `data-panel` = ns("panel_workflows"),
              onclick = tab_switch_js(),
              bsicons::bs_icon("layers"),
              "Workflows"
            ),
            tags$button(
              id = ns("tab_history"),
              class = "blockr-tab",
              type = "button",
              `data-panel` = ns("panel_history"),
              onclick = tab_switch_js(),
              bsicons::bs_icon("clock-history"),
              "History"
            ),
            uiOutput(ns("sharing_tab"))
          ),
          # Workflows panel
          tags$div(
            id = ns("panel_workflows"),
            class = "blockr-tab-panel",
            # Sticky search over the full workflow list. Typing filters
            # server-side; the list renders a window and materializes more as
            # you scroll (see project-navbar.js).
            tags$div(
              class = "blockr-workflow-search-wrap",
              tags$div(
                class = "blockr-workflow-search",
                bsicons::bs_icon("search", size = "0.9em"),
                tags$input(
                  id = ns("workflow_filter"),
                  type = "text",
                  class = "blockr-workflow-search-input",
                  placeholder = "Search workflows...",
                  autocomplete = "off",
                  oninput = sprintf(
                    "Shiny.setInputValue('%s', this.value,
                    {priority: 'event'})",
                    ns("workflow_filter")
                  ),
                  onkeydown = "blockrWorkflowSearchKey(event, this)"
                ),
                tags$span(
                  class = "blockr-workflow-count",
                  textOutput(ns("workflow_count"), inline = TRUE)
                )
              )
            ),
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
                "Manage workflows ",
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
          ),
          # Sharing panel (conditionally rendered from server)
          uiOutput(ns("sharing_panel"))
        )
      ),
      # Read-only workflow (rack) ID -- rendered only once the workflow has a
      # chosen id; an unsaved board has none, and "Not saved" carries that state
      tagAppendAttributes(
        uiOutput(ns("rack_id_area")),
        class = "blockr-navbar-id-area"
      ),
      # Save status
      tags$div(
        class = "blockr-navbar-save-section",
        tagAppendAttributes(
          textOutput(
            ns("save_status"),
            container = tags$span,
            inline = TRUE
          ),
          class = "blockr-navbar-meta"
        ),
        # Save on its own until the workflow is saved; the "Save as new
        # workflow" split only appears once there is a record to fork from
        tagAppendAttributes(
          uiOutput(ns("save_controls")),
          class = "btn-group blockr-navbar-save-group"
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

tab_switch_js <- function() {
  "event.stopPropagation();
  var dd = this.closest('.blockr-tabbed-dropdown');
  dd.querySelectorAll('.blockr-tab').forEach(
    t => t.classList.remove('active')
  );
  this.classList.add('active');
  dd.querySelectorAll('.blockr-tab-panel').forEach(
    p => p.classList.add('blockr-tab-panel-hidden')
  );
  var panel = document.getElementById(this.getAttribute('data-panel'));
  if (panel) panel.classList.remove('blockr-tab-panel-hidden');"
}
