test_that("tt_render produces valid Typst markup", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |> tt_render()

  expect_true(grepl("#table\\(", code))
  expect_true(grepl("columns:", code))
})

test_that("tt_render includes column widths", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |> tt_widths(1, 2) |> tt_render()

  expect_true(grepl("1fr", code))
  expect_true(grepl("2fr", code))
})

test_that("tt_render includes stroke when set", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |> tt_style(stroke = TRUE) |> tt_render()

  expect_true(grepl("stroke:", code))
})

test_that("tt_render handles striped rows", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |> tt_style(striped = TRUE) |> tt_render()

  expect_true(grepl("fill:.*calc\\.odd", code))
})

test_that("tt_render escapes special characters", {
  df <- data.frame(a = c("test*bold", "normal"), b = 1:2)
  code <- tt(df) |> tt_render()

  expect_true(grepl("test\\\\\\*bold", code))
})

test_that("tt_render escapes characters in all content", {
  df <- data.frame(formula = c("$x^2$", "$y_i$"), value = 1:2)
  code <- tt(df, escape = TRUE) |> tt_render()

  # $ and ^ are not special Typst chars, so unchanged

  expect_true(grepl("\\$x\\^2\\$", code))
  # _ is escaped even inside $...$
  expect_true(grepl("\\$y\\\\_i\\$", code))
})

test_that("tt_render generates bold formatting", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |> tt_column(a, bold = TRUE) |> tt_render()

  expect_true(grepl("\\*.*\\*", code))
})

test_that("tt_render wraps in figure when caption provided", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df, caption = "Test table") |> tt_render()

  expect_true(grepl("#figure\\(", code))
  expect_true(grepl("caption:", code))
  expect_true(grepl("Test table", code))
})

test_that("tt_render adds label for cross-reference", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df, label = "tbl-test") |> tt_render()

  expect_true(grepl("<tbl-test>", code))
})

test_that("tt_render handles header_above", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |> tt_header_above(c("Group" = 2)) |> tt_render()

  expect_true(grepl("colspan: 2", code))
  expect_true(grepl("Group", code))
})

test_that("tt_render handles hlines", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |> tt_style(header_separate = TRUE) |> tt_render()

  expect_true(grepl("table\\.hline\\(\\)", code))
})

test_that("tt_render handles footnotes", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |> tt_footnote(general = "Source: test") |> tt_render()

  expect_true(grepl("Source: test", code))
})

test_that("tt_render handles cell background", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |> tt_cell(1, 1, background = "yellow") |> tt_render()

  expect_true(grepl("table\\.cell\\(", code))
  expect_true(grepl("fill:", code))
})
