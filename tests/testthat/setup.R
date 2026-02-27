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

      server_url <- board_a$url
      server_host <- gsub("^https?://", "", server_url)

      subs <- c(
        setNames("user_a", board_a$account),
        setNames("user_b", board_b$account),
        setNames("https://connect.example.com", server_url),
        setNames("connect.example.com", server_host)
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
