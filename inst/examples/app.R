library(blockr.core)
library(blockr.dock)
library(blockr.dag)
library(blockr.session)
pkgload::load_all("blockr.session")

storage_dir <- file.path(tempdir(), "blockr-workflows")

options(
  blockr.session_mgmt_backend = blockr.session::board_folder(storage_dir)
)

board <- new_dock_board(
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)
serve(board, plugins = custom_plugins(manage_project()))
