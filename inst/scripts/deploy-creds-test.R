#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2L) {
  cat("Usage: Rscript deploy-creds-test.R <server_url> <api_key>\n")
  quit(status = 1L)
}

Sys.setenv(CONNECT_SERVER = args[[1L]])
Sys.setenv(CONNECT_API_KEY = args[[2L]])

# --- App (inlined so no local package install required) ---

app <- r"[
library(shiny)
library(connectapi)

ui <- fluidPage(
  h3("Connect credentials diagnostic"),

  h4("Process-level user (CONNECT_API_KEY env var)"),
  verbatimTextOutput("process_user"),

  h4("Connect-related session request headers"),
  verbatimTextOutput("session_headers"),

  h4("Viewer user after token exchange"),
  verbatimTextOutput("viewer_user")
)

server <- function(input, output, session) {

  output$process_user <- renderPrint({
    tryCatch(
      connectapi::connect()$GET("users/current/")[c("username", "email")],
      error = function(e) conditionMessage(e)
    )
  })

  output$session_headers <- renderPrint({
    hdrs <- as.list(session$request)
    hdrs[grepl("CONNECT|POSIT", names(hdrs), ignore.case = TRUE)]
  })

  output$viewer_user <- renderPrint({
    token <- session$request$HTTP_POSIT_CONNECT_USER_SESSION_TOKEN

    if (is.null(token) || !nzchar(token)) {
      return("No viewer session token in request headers")
    }

    tryCatch(
      connectapi::connect(token = token)$GET("users/current/")[c("username", "email")],
      error = function(e) conditionMessage(e)
    )
  })
}

shinyApp(ui, server)
]"

# --- Deploy ---

dir <- tempfile()
dir.create(dir)
on.exit(unlink(dir, recursive = TRUE))

writeLines(app, file.path(dir, "app.R"))
rsconnect::writeManifest(dir, appPrimaryDoc = "app.R", appMode = "shiny")

con <- connectapi::connect()

task <- con |>
  connectapi::deploy(connectapi::bundle_dir(dir), name = "blockr-creds-test") |>
  connectapi::poll_task()

connectapi::content_update_access_type(task, access_type = "logged_in")

cat("Deployed:", task$content$dashboard_url, "\n")
