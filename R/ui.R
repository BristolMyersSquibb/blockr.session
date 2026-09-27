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
    # The navbar: the workflow on the left (its name, whose menu switches to
    # another workflow, and its save menu beside it), then blockr.dock's views
    # on the right. Each piece carries `data-navbar-slot`; blockr.dock's
    # navbar orders the slots into one row, so this plugin's pieces and the
    # dock's can interleave.
    tags$div(
      class = "manage-project-navbar",
      # The workflow's name: its menu lists every workflow, a new one, and
      # drafts from earlier sessions
      tags$div(
        class = "dropdown blockr-navbar-name",
        `data-navbar-slot` = "title",
        tags$button(
          class = "blockr-navbar-name-btn",
          type = "button",
          title = "Workflows",
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
          id = ns("tabbed_dropdown"),
          class = "dropdown-menu blockr-tabbed-dropdown blockr-name-menu",
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
            class = "blockr-menu blockr-name-actions",
            uiOutput(ns("recovery_notice")),
            uiOutput(ns("new_controls"))
          )
        )
      ),
      # Beside the name, everything about keeping this workflow: the disk
      # icon saves, the chevron opens Save, Save as, Download, the latest
      # versions and Share. Share swaps the menu for the sharing panel.
      tags$div(
        class = "dropdown blockr-navbar-save-group",
        `data-navbar-slot` = "save",
        uiOutput(ns("save_controls"), inline = TRUE),
        tags$button(
          class = "blockr-navbar-save-toggle",
          type = "button",
          title = "Save options",
          `aria-label` = "Save options",
          `data-bs-toggle` = "dropdown",
          `data-bs-auto-close` = "outside",
          `aria-expanded` = "false",
          bsicons::bs_icon("chevron-down", class = "blockr-navbar-chev")
        ),
        tags$div(
          class = "dropdown-menu blockr-tabbed-dropdown blockr-save-menu",
          tags$div(
            id = ns("panel_save"),
            class = "blockr-tab-panel blockr-save-panel",
            tags$div(
              class = "blockr-menu blockr-save-actions",
              uiOutput(ns("save_items"))
            ),
            tags$div(class = "blockr-save-rule"),
            tags$div(class = "blockr-history-title", "Versions"),
            uiOutput(ns("version_history")),
            tags$a(
              href = "#",
              class = "blockr-workflows-link",
              onclick = sprintf(
                "Shiny.setInputValue('%s', Date.now(), {priority: 'event'});
                return false;",
                ns("view_all_versions")
              ),
              "All versions ",
              bsicons::bs_icon("arrow-right")
            ),
            uiOutput(ns("share_item"))
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
        uiOutput(ns("user_avatar"), inline = TRUE),
        `data-navbar-slot` = "account"
      )
    )
  )
}

# Show one panel of a menu and hide the others: Share opens the sharing
# panel in place of the save menu, and its back row returns.
panel_switch_js <- function(panel) {
  sprintf(
    "event.stopPropagation();
    var dd = this.closest('.blockr-tabbed-dropdown');
    dd.querySelectorAll('.blockr-tab-panel').forEach(
      p => p.classList.toggle('blockr-tab-panel-hidden', p.id !== '%s')
    );",
    panel
  )
}
