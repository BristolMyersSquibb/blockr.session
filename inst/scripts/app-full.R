library(blockr)
library(blockr.session)

options(
  blockr.session_mgmt_backend = pins::board_connect
)

serve(
  new_dock_board(),
  plugins = custom_plugins(manage_project())
)
