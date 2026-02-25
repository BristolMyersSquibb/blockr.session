library(blockr)
library(blockr.session)
library(connectapi)

options(
  blockr.session_mgmt_backend = function() {
    session <- shiny::getDefaultReactiveDomain()
    token <- session$request$HTTP_POSIT_CONNECT_USER_SESSION_TOKEN

    if (is.null(token) || !nzchar(token)) {
      return(pins::board_local())
    }

    con <- connectapi::connect(token = token)
    pins::board_connect(
      auth = "manual",
      server = Sys.getenv("CONNECT_SERVER"),
      key = con$api_key
    )
  }
)

serve(
  new_dock_board(),
  plugins = custom_plugins(manage_project())
)
