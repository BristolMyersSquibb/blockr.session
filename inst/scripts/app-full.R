library(blockr.core)

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
  },
  g6R.layout_on_data_change = TRUE
)

serve(
  blockr.dock::new_dock_board(extensions = blockr.dag::new_dag_extension()),
  plugins = custom_plugins(blockr.session::manage_project())
)
