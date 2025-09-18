library(blockr.md)
library(blockr.core)
library(blockr.ai)

options(
  blockr.session_mgmt_backend = pins::board_connect()
)

serve(new_md_board())
