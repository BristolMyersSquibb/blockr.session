#' @param id Namespace ID
#' @param x Board object
#'
#' @rdname manage_project
#' @export
manage_project_ui <- function(id, x) {

  ns <- NS(id)

  tagList(
    # blockr.ui: the design tokens and the menu classes the navbar's menus
    # take their look from.
    blockr.ui::controls_dep(),
    # CSS and JS dependencies
    htmltools::htmlDependency(
      "project-navbar",
      as.character(utils::packageVersion("blockr.session")),
      src = system.file("assets", package = "blockr.session"),
      stylesheet = "css/project-navbar.css",
      script = "js/project-navbar.js"
    ),
    # The navbar reads as a path: blockr.dock's mark (with this plugin's
    # workflows menu), the workflow's name (what you do to this workflow),
    # then blockr.dock's page menu. Each
    # piece carries `data-navbar-slot`; blockr.dock's navbar orders the slots
    # into one row, so this plugin's pieces and the dock's can interleave.
    tags$div(
      class = "manage-project-navbar",
      # The mark's menu: every workflow, a new one, and drafts from earlier
      # sessions. blockr.dock draws the mark; the class
      # `blockr-navbar-brand-menu` asks it to hang this menu under the mark.
      tags$div(
        id = ns("tabbed_dropdown"),
        class = paste(
          "dropdown-menu blockr-tabbed-dropdown blockr-mark-menu",
          "blockr-navbar-brand-menu"
        ),
        # Sticky search over the full workflow list. Typing filters
        # server-side; the list renders a window and materializes more as
        # you scroll (see project-navbar.js).
        tags$div(
          id = ns("panel_workflows"),
          class = "blockr-tab-panel",
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
              "All workflows ",
              bsicons::bs_icon("arrow-right")
            )
          )
        ),
        # New workflow, new in a new tab, and the drafts row when there are
        # drafts to recover
        tags$div(
          class = "blockr-menu blockr-mark-actions",
          uiOutput(ns("recovery_notice")),
          uiOutput(ns("new_controls"))
        )
      ),
      # The workflow's name: its menu holds what you do to this workflow
      tags$div(
        class = "dropdown blockr-navbar-name",
        `data-navbar-slot` = "title",
        tags$button(
          class = "blockr-navbar-name-btn",
          type = "button",
          `data-bs-toggle` = "dropdown",
          `data-bs-auto-close` = "outside",
          `aria-expanded` = "false",
          tagAppendAttributes(
            textOutput(ns("rack_id_area"), inline = TRUE),
            class = "blockr-navbar-title"
          ),
          bsicons::bs_icon("chevron-down", class = "blockr-navbar-chev")
        ),
        tags$div(
          class = "dropdown-menu blockr-tabbed-dropdown blockr-name-menu",
          tags$div(
            class = "blockr-menu blockr-name-actions",
            uiOutput(ns("save_as_item"))
          ),
          tags$div(
            class = "blockr-tab-bar",
            tags$button(
              id = ns("tab_history"),
              class = "blockr-tab active",
              type = "button",
              `data-panel` = ns("panel_history"),
              onclick = tab_switch_js(),
              bsicons::bs_icon("clock-history"),
              "History"
            ),
            uiOutput(ns("sharing_tab"))
          ),
          tags$div(
            id = ns("panel_history"),
            class = "blockr-tab-panel",
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
      # The save state lives on the button (Save..., Save); the status text
      # stays for screen readers and carries "Autosave failed" as a tooltip
      tagAppendAttributes(
        textOutput(ns("save_status"), container = tags$span, inline = TRUE),
        class = "visually-hidden"
      ),
      tagAppendAttributes(
        uiOutput(ns("save_controls")),
        class = "blockr-navbar-save-group",
        `data-navbar-slot` = "actions"
      ),
      tagAppendAttributes(
        uiOutput(ns("user_avatar"), inline = TRUE),
        `data-navbar-slot` = "account"
      )
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
