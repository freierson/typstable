test_that("tt_style sets table-level properties", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df, rownames = FALSE) |> tt_style(stroke = TRUE, striped = TRUE)
  expect_true(tbl$stroke)
  expect_true(tbl$striped)

  tbl2 <- tt(df, rownames = FALSE) |> tt_style(inset = "5pt", fill = "gray")
  expect_equal(tbl2$inset, "5pt")
  expect_equal(tbl2$fill, "gray")
})

test_that("tt_style sets header_separate", {
  df <- data.frame(a = 1:3, b = 4:6)
  tbl <- tt(df, rownames = FALSE) |> tt_style(header_separate = TRUE)
  expect_true(tbl$header_separate)
})

test_that("tt_column sets column styles", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df, rownames = FALSE) |> tt_column(a, bold = TRUE, align = "right")
  expect_true(tbl$col_styles$a$bold)
  expect_equal(tbl$col_align[1], "right")
})

test_that("tt_column works with tidy-select", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  tbl <- tt(df, rownames = FALSE) |> tt_column(c(a, c), color = "blue")
  expect_equal(tbl$col_styles$a$color, "blue")
  expect_equal(tbl$col_styles$c$color, "blue")
})

test_that("tt_row sets row styles", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df, rownames = FALSE) |> tt_row(0, bold = TRUE, background = "gray")
  expect_true(tbl$row_styles[["0"]]$bold)
  expect_equal(tbl$row_styles[["0"]]$background, "gray")
})

test_that("tt_row handles hlines", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df, rownames = FALSE) |> tt_row(1, hline_above = TRUE, hline_below = TRUE)
  expect_true(length(tbl$hlines) >= 2)
})

test_that("tt_cell sets individual cell styles", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df, rownames = FALSE) |> tt_cell(1, 1, background = "yellow")
  expect_equal(tbl$cell_styles[["1_1"]]$background, "yellow")
})

test_that("tt_cell handles colspan", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  tbl <- tt(df, rownames = FALSE) |> tt_cell(1, 1, colspan = 2)
  expect_equal(tbl$cell_styles[["1_1"]]$colspan, 2)
})

test_that("tt_header_above adds grouped header", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df, rownames = FALSE) |> tt_header_above(c("Group" = 2))
  expect_length(tbl$headers_above, 1)
  expect_equal(tbl$headers_above[[1]]$header, c("Group" = 2))
})

test_that("tt_header_above validates span sum", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(tt(df, rownames = FALSE) |> tt_header_above(c("Group" = 3)), "must sum to 2")
})

test_that("tt_pack_rows creates row groups", {
  df <- data.frame(a = 1:5, b = 6:10)

  tbl <- tt(df, rownames = FALSE) |> tt_pack_rows("Group 1", 1, 3)
  expect_length(tbl$row_groups, 1)
  expect_equal(tbl$row_groups[[1]]$group_label, "Group 1")
  expect_equal(tbl$row_groups[[1]]$start_row, 1L)
  expect_equal(tbl$row_groups[[1]]$end_row, 3L)
})

test_that("tt_footnote adds footnotes", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df, rownames = FALSE) |> tt_footnote(general = "Note 1")
  expect_equal(tbl$footnotes$general, "Note 1")

  tbl2 <- tt(df, rownames = FALSE) |> tt_footnote(number = c("First", "Second"))
  expect_equal(tbl2$footnotes$number, c("First", "Second"))
})

test_that("tt_hline adds horizontal lines", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df, rownames = FALSE) |> tt_hline(1, stroke = "red")
  expect_length(tbl$hlines, 1)
  expect_equal(tbl$hlines[[1]]$y, 1L)
})

test_that("tt_vline adds vertical lines", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df, rownames = FALSE) |> tt_vline(1, stroke = "blue")
  expect_length(tbl$vlines, 1)
  expect_equal(tbl$vlines[[1]]$x, 1L)
})

test_that("tt_column rotation works with static and data-driven values", {
  df <- data.frame(a = 1:3, b = 4:6)

  # Static numeric rotation
  tbl <- tt(df, rownames = FALSE) |> tt_column(a, rotate = 90)
  expect_equal(tbl$col_styles$a$rotate, 90)

  # Static string rotation
  tbl2 <- tt(df, rownames = FALSE) |> tt_column(a, rotate = "45deg")
  expect_equal(tbl2$col_styles$a$rotate, "45deg")

  # Data-driven rotation
  df2 <- data.frame(label = c("A", "B"), value = 1:2, angle = c("0deg", "90deg"))
  tbl3 <- tt(df2, cols = c(label, value), rownames = FALSE) |>
    tt_column(label, rotate = angle)
  expect_equal(tbl3$col_styles$label$rotate_col, "angle")
})

test_that("tt_row and tt_cell rotation works", {
  df <- data.frame(a = 1:3, b = 4:6)

  # Row rotation
  tbl <- tt(df, rownames = FALSE) |> tt_row(0, rotate = "90deg")
  expect_equal(tbl$row_styles[["0"]]$rotate, "90deg")

  # Cell rotation
  tbl2 <- tt(df, rownames = FALSE) |> tt_cell(1, 1, rotate = "-45deg")
  expect_equal(tbl2$cell_styles[["1_1"]]$rotate, "-45deg")
})

# Pattern-based styling tests

test_that("tt_column pattern expansion works for color", {
  df <- data.frame(
    a = 1:3, b = 4:6,
    color_a = c("red", "green", "blue"),
    color_b = c("black", "gray", "white")
  )

  tbl <- tt(df, cols = c(a, b), rownames = FALSE) |>
    tt_column(c(a, b), color = "color_{col}")

  expect_equal(tbl$col_styles$a$color_col, "color_a")
  expect_equal(tbl$col_styles$b$color_col, "color_b")
})

test_that("tt_column pattern expansion works for background", {
  df <- data.frame(
    x = 1:3, y = 4:6,
    bg_x = c("yellow", "orange", "pink"),
    bg_y = c("cyan", "magenta", "lime")
  )

  tbl <- tt(df, cols = c(x, y), rownames = FALSE) |>
    tt_column(c(x, y), background = "bg_{col}")

  expect_equal(tbl$col_styles$x$background_col, "bg_x")
  expect_equal(tbl$col_styles$y$background_col, "bg_y")
})

test_that("tt_column pattern expansion works for multiple style attributes", {
  df <- data.frame(
    val = 1:3,
    color_val = c("red", "green", "blue"),
    bg_val = c("white", "gray", "black"),
    bold_val = c(TRUE, FALSE, TRUE)
  )

  tbl <- tt(df, cols = val, rownames = FALSE) |>
    tt_column(val, color = "color_{col}", background = "bg_{col}", bold = "bold_{col}")

  expect_equal(tbl$col_styles$val$color_col, "color_val")
  expect_equal(tbl$col_styles$val$background_col, "bg_val")
  expect_equal(tbl$col_styles$val$bold_col, "bold_val")
})

test_that("tt_column .missing = 'ignore' silently skips missing columns", {

  df <- data.frame(a = 1:3, b = 4:6)

  # Should not produce warning with .missing = "ignore"
  expect_silent({
    tbl <- tt(df, rownames = FALSE) |>
      tt_column(a, color = "color_{col}", .missing = "ignore")
  })

  # color_col should not be set since color_a doesn't exist
  expect_null(tbl$col_styles$a$color_col)
})

test_that("tt_column .missing = 'warn' produces warning for missing columns", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_warning(
    tt(df, rownames = FALSE) |>
      tt_column(a, color = "color_{col}", .missing = "warn"),
    "Column 'color_a' not found"
  )
})

test_that("tt_column .missing = 'error' stops for missing columns", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    tt(df, rownames = FALSE) |>
      tt_column(a, color = "color_{col}", .missing = "error"),
    "Column 'color_a' not found"
  )
})

test_that("tt_column mixed pattern, static, and column ref in same call", {
  df <- data.frame(
    a = 1:3, b = 4:6,
    color_a = c("red", "green", "blue"),
    my_bg = c("white", "gray", "black")
  )

  tbl <- tt(df, cols = c(a, b), rownames = FALSE) |>
    tt_column(a, color = "color_{col}", background = my_bg, bold = TRUE)

  # Pattern should expand

  expect_equal(tbl$col_styles$a$color_col, "color_a")
  # Column ref should work
  expect_equal(tbl$col_styles$a$background_col, "my_bg")
  # Static should work
  expect_true(tbl$col_styles$a$bold)
})

test_that("tt_column string without {col} is treated as static value", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df, rownames = FALSE) |>
    tt_column(a, color = "blue")

  # Should be static color, not a column reference
  expect_equal(tbl$col_styles$a$color, "blue")
  expect_null(tbl$col_styles$a$color_col)
})

test_that("tt_column pattern works with font_size and rotate", {
  df <- data.frame(
    a = 1:3,
    size_a = c("10pt", "12pt", "14pt"),
    angle_a = c("0deg", "45deg", "90deg")
  )

  tbl <- tt(df, cols = a, rownames = FALSE) |>
    tt_column(a, font_size = "size_{col}", rotate = "angle_{col}")

  expect_equal(tbl$col_styles$a$font_size_col, "size_a")
  expect_equal(tbl$col_styles$a$rotate_col, "angle_a")
})

test_that("tt_column pattern works with italic", {
  df <- data.frame(
    a = 1:3,
    italic_a = c(TRUE, FALSE, TRUE)
  )

  tbl <- tt(df, cols = a, rownames = FALSE) |>
    tt_column(a, italic = "italic_{col}")

  expect_equal(tbl$col_styles$a$italic_col, "italic_a")
})
