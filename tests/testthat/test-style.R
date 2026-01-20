test_that("tt_style sets table-level properties", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_style(stroke = TRUE, striped = TRUE)
  expect_true(tbl$stroke)
  expect_true(tbl$striped)

  tbl2 <- tt(df) |> tt_style(inset = "5pt", fill = "gray")
  expect_equal(tbl2$inset, "5pt")
  expect_equal(tbl2$fill, "gray")
})

test_that("tt_style sets header_separate", {
  df <- data.frame(a = 1:3, b = 4:6)
  tbl <- tt(df) |> tt_style(header_separate = TRUE)
  expect_true(tbl$header_separate)
})

test_that("tt_column sets column styles", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_column(a, bold = TRUE, align = "right")
  expect_true(tbl$col_styles$a$bold)
  expect_equal(tbl$col_align[1], "right")
})

test_that("tt_column works with tidy-select", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  tbl <- tt(df) |> tt_column(c(a, c), color = "blue")
  expect_equal(tbl$col_styles$a$color, "blue")
  expect_equal(tbl$col_styles$c$color, "blue")
})

test_that("tt_row sets row styles", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_row(0, bold = TRUE, background = "gray")
  expect_true(tbl$row_styles[["0"]]$bold)
  expect_equal(tbl$row_styles[["0"]]$background, "gray")
})

test_that("tt_row handles hlines", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_row(1, hline_above = TRUE, hline_below = TRUE)
  expect_true(length(tbl$hlines) >= 2)
})

test_that("tt_cell sets individual cell styles", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_cell(1, 1, background = "yellow")
  expect_equal(tbl$cell_styles[["1_1"]]$background, "yellow")
})

test_that("tt_cell handles colspan", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  tbl <- tt(df) |> tt_cell(1, 1, colspan = 2)
  expect_equal(tbl$cell_styles[["1_1"]]$colspan, 2)
})

test_that("tt_header_above adds grouped header", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_header_above(c("Group" = 2))
  expect_length(tbl$headers_above, 1)
  expect_equal(tbl$headers_above[[1]]$header, c("Group" = 2))
})

test_that("tt_header_above validates span sum", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(tt(df) |> tt_header_above(c("Group" = 3)), "must sum to 2")
})

test_that("tt_pack_rows creates row groups", {
  df <- data.frame(a = 1:5, b = 6:10)

  tbl <- tt(df) |> tt_pack_rows("Group 1", 1, 3)
  expect_length(tbl$row_groups, 1)
  expect_equal(tbl$row_groups[[1]]$group_label, "Group 1")
  expect_equal(tbl$row_groups[[1]]$start_row, 1L)
  expect_equal(tbl$row_groups[[1]]$end_row, 3L)
})

test_that("tt_footnote adds footnotes", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_footnote(general = "Note 1")
  expect_equal(tbl$footnotes$general, "Note 1")

  tbl2 <- tt(df) |> tt_footnote(number = c("First", "Second"))
  expect_equal(tbl2$footnotes$number, c("First", "Second"))
})

test_that("tt_hline adds horizontal lines", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_hline(1, stroke = "red")
  expect_length(tbl$hlines, 1)
  expect_equal(tbl$hlines[[1]]$y, 1L)
})

test_that("tt_vline adds vertical lines", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_vline(1, stroke = "blue")
  expect_length(tbl$vlines, 1)
  expect_equal(tbl$vlines[[1]]$x, 1L)
})
