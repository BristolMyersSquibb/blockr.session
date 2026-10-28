test_that("no :has() sits above a subject without a class or an id (#145)", {

  # A `:has()` outside the subject makes Chrome restyle the whole page on every
  # DOM insertion unless the subject carries a class or an id. On a 40-block
  # dock board, `.manage-project-navbar:has(...) > *` cost 81ms per insertion,
  # against 3ms with the `:has()` on the piece itself.
  css <- list.files(
    system.file("assets", "css", package = "blockr.session"),
    pattern = "\\.css$",
    full.names = TRUE
  )

  sels <- unlist(
    lapply(css, function(file) {
      x <- paste(readLines(file, warn = FALSE), collapse = "\n")
      x <- gsub("(?s)/\\*.*?\\*/", "", x, perl = TRUE)
      x <- regmatches(x, gregexpr("[^{};]+(?=\\{)", x, perl = TRUE))[[1L]]
      x <- trimws(x)
      x <- x[!startsWith(x, "@")]
      # Empty every argument and attribute, so that the commas and spaces left
      # are the selectors' own
      x <- gsub("\\((?:[^()]++|(?R))*\\)", "()", x, perl = TRUE)
      x <- gsub("\\[[^]]*\\]", "[]", x)
      trimws(unlist(strsplit(x, ",")))
    })
  )

  slow <- lgl_ply(
    strsplit(sels, "\\s*[>+~]\\s*|\\s+"),
    function(x) {
      n <- length(x)
      n > 1L && any(grepl(":has(", x[-n], fixed = TRUE)) &&
        !grepl("[.#]", x[n])
    }
  )

  expect_true(any(grepl(":has(", sels, fixed = TRUE)))
  expect_identical(sels[slow], character())
})
