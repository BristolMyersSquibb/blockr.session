library(blockr.core)
library(blockr.session)

board <- new_board()
serve(board, plugins = c(board_plugins(board, -1), manage_project()))
