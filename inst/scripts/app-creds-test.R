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
