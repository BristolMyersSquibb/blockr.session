connect_vars <- c("CONNECT_SERVER", "CONNECT_API_KEY_A", "CONNECT_API_KEY_B")

if (all(nzchar(Sys.getenv(connect_vars)))) {
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

      server_host <- gsub("^https?://", "", board_a$url)

      subs <- set_names(
        c("user_a", "user_b", "connect.example.com"),
        c(board_a$account, board_b$account, server_host)
      )

      withr::local_options(
        blockr.connect_fixture_subs = subs,
        blockr.connect_recording = TRUE,
        .local_envir = testthat::teardown_env()
      )
    },
    error = function(e) message(conditionMessage(e))
  )
}
