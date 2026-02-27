library(blockr.core)
library(blockr.session)

options(
  blockr.session_mgmt_backend = pins::board_connect
)

board <- new_board()
serve(board, plugins = c(board_plugins(board, -1), manage_project()))
