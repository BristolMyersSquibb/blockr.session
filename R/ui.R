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
