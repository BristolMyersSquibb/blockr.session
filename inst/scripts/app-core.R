library(blockr.core)
library(blockr.session)

board <- new_board()
serve(board, plugins = custom_plugins(manage_project()))
