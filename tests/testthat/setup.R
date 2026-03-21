connect_vars <- Sys.getenv(
  c("CONNECT_SERVER", "CONNECT_API_KEY_A", "CONNECT_API_KEY_B")
)

if (all(nzchar(connect_vars)) && nzchar(Sys.getenv("CONNECT_RECORD"))) {

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

    subs <- c(
      set_names("user_a", board_a$account),
      set_names("user_b", board_b$account),
      set_names("connect.example.com", server_host)
    )

    # Query authenticated user details to substitute GUIDs and emails
    tryCatch({
      me_a <- connect_api(board_a, "GET /user")
      me_b <- connect_api(board_b, "GET /user")

      if (not_null(me_a$guid)) {
        subs[me_a$guid] <- "00000000-0000-4000-a000-000000000001"
      }
      if (not_null(me_b$guid)) {
        subs[me_b$guid] <- "00000000-0000-4000-a000-000000000002"
      }
      if (not_null(me_a$email)) {
        subs[me_a$email] <- "user_a@example.com"
      }
      if (not_null(me_b$email)) {
        subs[me_b$email] <- "user_b@example.com"
      }
      if (not_null(me_a$first_name) && nzchar(me_a$first_name)) {
        subs[me_a$first_name] <- "Alice"
      }
      if (not_null(me_a$last_name) && nzchar(me_a$last_name)) {
        subs[me_a$last_name] <- "Test"
      }
      if (not_null(me_b$first_name) && nzchar(me_b$first_name)) {
        subs[me_b$first_name] <- "Bob"
      }
      if (not_null(me_b$last_name) && nzchar(me_b$last_name)) {
        subs[me_b$last_name] <- "Test"
      }
    }, error = function(e) {
      message("Could not fetch user details for substitutions: ", e$message)
    })

    connect_test_substitutions <- subs
    connect_test_recording <- TRUE
  }
}
