test_that("the avatar item draws a single output and brings its stylesheet", {

  skip_if_not_installed("blockr.dock")

  item <- avatar_navbar_item()

  expect_true(blockr.dock::is_navbar_item(item))
  expect_identical(item[["id"]], "avatar")

  ui <- item[["ui"]](NS("board", "avatar"), new_board())

  doc <- xml2::read_html(as.character(div(id = "wrapper", ui)))
  children <- xml2::xml_find_all(doc, "//div[@id='wrapper']/*")

  expect_length(children, 1L)
  expect_identical(xml2::xml_attr(children, "id"), "board-avatar-initials")
  expect_match(xml2::xml_attr(children, "class"), "shiny-html-output")

  deps <- htmltools::findDependencies(ui)
  expect_true("avatar-navbar" %in% chr_xtr(deps, "name"))
})

test_that("the avatar shows the signed-in user's initials", {

  skip_if_not_installed("blockr.dock")

  session <- MockShinySession$new()
  session$user <- "jane.doe"

  testServer(
    avatar_navbar_item()[["server"]],
    {
      avatar <- xml2::read_html(output$initials$html)
      expect_identical(
        xml2::xml_text(
          xml2::xml_find_all(avatar, "//div[@class='blockr-navbar-avatar']")
        ),
        "JD"
      )
    },
    args = list(board = reactiveValues()),
    session = session
  )
})

test_that("the avatar draws nothing when no one is signed in", {

  skip_if_not_installed("blockr.dock")

  testServer(
    avatar_navbar_item()[["server"]],
    expect_null(output$initials),
    args = list(board = reactiveValues())
  )
})

test_that("the plugin's navbar piece leaves the avatar to its own item", {

  doc <- xml2::read_html(
    as.character(manage_project_ui("project", new_board()))
  )

  expect_length(
    xml2::xml_find_all(doc, "//*[contains(@id, 'avatar')]"),
    0L
  )
})

test_that("get_initials takes the first letters of a split name", {
  expect_identical(get_initials("jane.doe@example.com"), "JD")
  expect_identical(get_initials("jane"), "JA")
})
