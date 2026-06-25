library(blockr.core)

options(g6R.layout_on_data_change = TRUE)

serve(
  blockr.dock::new_dock_board(extensions = blockr.dag::new_dag_extension()),
  plugins = custom_plugins(blockr.session::manage_project())
)
