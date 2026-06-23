library(blockr.core)

options(
  blockr.session_mgmt_backend = blockr.session::user_pins_board,
  g6R.layout_on_data_change = TRUE
)

serve(
  blockr.dock::new_dock_board(extensions = blockr.dag::new_dag_extension()),
  plugins = custom_plugins(blockr.session::manage_project())
)
