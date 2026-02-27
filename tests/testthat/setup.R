connect_vars <- Sys.getenv(
  c("CONNECT_SERVER", "CONNECT_API_KEY_A", "CONNECT_API_KEY_B")
)

if (all(nzchar(connect_vars))) {

  try_board <- function(key, label) {
    tryCatch(
      pins::board_connect(
        server = connect_vars["CONNECT_SERVER"],
        key = key
      ),
      error = function(e) {
        message("Connect ", label, ": ", conditionMessage(e))
        NULL
      }
    )
  }

  board_a <- try_board(connect_vars["CONNECT_API_KEY_A"], "user A")
  board_b <- try_board(connect_vars["CONNECT_API_KEY_B"], "user B")

  if (not_null(board_a) && not_null(board_b)) {

    server_host <- gsub("^https?://", "", board_a$url)

    connect_test_substitutions <- set_names(
      c("user_a", "user_b", "connect.example.com"),
      c(board_a$account, board_b$account, server_host)
    )

    connect_test_recording <- TRUE
  }
}
