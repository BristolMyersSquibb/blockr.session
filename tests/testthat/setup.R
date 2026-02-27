if (connect_recording()) {

  board_a <- pins::board_connect(
    server = Sys.getenv("CONNECT_SERVER"),
    key = Sys.getenv("CONNECT_API_KEY_A")
  )

  board_b <- pins::board_connect(
    server = Sys.getenv("CONNECT_SERVER"),
    key = Sys.getenv("CONNECT_API_KEY_B")
  )

  server_url <- board_a$url
  server_host <- gsub("^https?://", "", server_url)

  subs <- c(
    setNames("user_a", board_a$account),
    setNames("user_b", board_b$account),
    setNames("https://connect.example.com", server_url),
    setNames("connect.example.com", server_host)
  )

  options(blockr.connect_fixture_subs = subs)

  blockr_data <- list(
    blocks = list(
      a = list(type = "dataset_block"),
      b = list(type = "filter_block")
    ),
    links = list(),
    format = "test"
  )

  blockr_data_v2 <- blockr_data
  blockr_data_v2$blocks$c <- list(type = "plot_block")

  upload_blockr_json(
    board_a, blockr_data, "blockr-fixture-board",
    versioned = TRUE,
    metadata = list(format = "v1"),
    tags = "blockr-session"
  )

  Sys.sleep(1)

  upload_blockr_json(
    board_a, blockr_data_v2, "blockr-fixture-board",
    versioned = TRUE,
    metadata = list(format = "v1"),
    tags = "blockr-session"
  )

  upload_blockr_json(
    board_a, list(x = 1), "blockr-fixture-plain",
    versioned = TRUE,
    metadata = list(format = "v1")
  )

  upload_blockr_json(
    board_b, blockr_data, "blockr-fixture-board",
    versioned = TRUE,
    metadata = list(format = "v1"),
    tags = "blockr-session"
  )

  qualified_a_board <- paste0(board_a$account, "/blockr-fixture-board")
  qualified_a_plain <- paste0(board_a$account, "/blockr-fixture-plain")
  qualified_b_board <- paste0(board_b$account, "/blockr-fixture-board")

  withr::defer(
    {
      try(pins::pin_delete(board_a, qualified_a_board), silent = TRUE)
      try(pins::pin_delete(board_a, qualified_a_plain), silent = TRUE)
      try(pins::pin_delete(board_b, qualified_b_board), silent = TRUE)
    },
    envir = testthat::teardown_env()
  )
}
