connect_vars <- c("CONNECT_SERVER", "CONNECT_API_KEY_A", "CONNECT_API_KEY_B")
all_set <- all(nzchar(Sys.getenv(connect_vars)))

if (all_set) {
  tryCatch(
    {
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

      options(
        blockr.connect_fixture_subs = subs,
        blockr.connect_recording = TRUE
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
    },
    error = function(e) NULL
  )
}
