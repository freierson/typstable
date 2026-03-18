test_that("tt_style sets table-level properties", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_style(stroke = TRUE, striped = TRUE)
  expect_true(tbl$stroke)
  expect_true(tbl$striped)

  tbl2 <- tt(df) |> tt_style(inset = "5pt", fill = "gray")
  expect_equal(tbl2$inset, "5pt")
  expect_equal(tbl2$fill, "gray")
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

  tbl <- tt(df) |> tt_row(0, bold = TRUE, fill = "gray")
  expect_true(tbl$row_styles[["0"]]$bold)
  expect_equal(tbl$row_styles[["0"]]$fill, "gray")
})

test_that("tt_row handles hlines", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_row(1, hline_above = TRUE, hline_below = TRUE)
  expect_true(length(tbl$hlines) >= 2)
})

test_that("tt_row errors when row is missing", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_error(tt(df) |> tt_row(), "must specify at least one row number")
})

test_that("tt_row stores rotate, inset, stroke attributes", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_row(1, rotate = "45deg", inset = "8pt", stroke = "2pt + red")
  expect_equal(tbl$row_styles[["1"]]$rotate, "45deg")
  expect_equal(tbl$row_styles[["1"]]$inset, "8pt")
  expect_equal(tbl$row_styles[["1"]]$stroke, "2pt + red")
})

test_that("tt_cell sets individual cell styles", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_cell(1, 1, fill = "yellow")
  expect_equal(tbl$cell_styles[["1_1"]]$fill, "yellow")
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

test_that("tt_column rotation works with static and data-driven values", {
  df <- data.frame(a = 1:3, b = 4:6)

  # Static numeric rotation
  tbl <- tt(df) |> tt_column(a, rotate = 90)
  expect_equal(tbl$col_styles$a$rotate, 90)

  # Static string rotation
  tbl2 <- tt(df) |> tt_column(a, rotate = "45deg")
  expect_equal(tbl2$col_styles$a$rotate, "45deg")

  # Data-driven rotation
  df2 <- data.frame(label = c("A", "B"), value = 1:2, angle = c("0deg", "90deg"))
  tbl3 <- tt(df2, cols = c(label, value)) |>
    tt_column(label, rotate = angle)
  expect_equal(tbl3$col_styles$label$rotate_col, "angle")
})

test_that("tt_row and tt_cell rotation works", {
  df <- data.frame(a = 1:3, b = 4:6)

  # Row rotation
  tbl <- tt(df) |> tt_row(0, rotate = "90deg")
  expect_equal(tbl$row_styles[["0"]]$rotate, "90deg")

  # Cell rotation
  tbl2 <- tt(df) |> tt_cell(1, 1, rotate = "-45deg")
  expect_equal(tbl2$cell_styles[["1_1"]]$rotate, "-45deg")
})

# Pattern-based styling tests

test_that("tt_column pattern expansion works for color", {
  df <- data.frame(
    a = 1:3, b = 4:6,
    color_a = c("red", "green", "blue"),
    color_b = c("black", "gray", "white")
  )

  tbl <- tt(df, cols = c(a, b)) |>
    tt_column(c(a, b), color = "color_{col}")

  expect_equal(tbl$col_styles$a$color_col, "color_a")
  expect_equal(tbl$col_styles$b$color_col, "color_b")
})

test_that("tt_column pattern expansion works for fill", {
  df <- data.frame(
    x = 1:3, y = 4:6,
    bg_x = c("yellow", "orange", "pink"),
    bg_y = c("cyan", "magenta", "lime")
  )

  tbl <- tt(df, cols = c(x, y)) |>
    tt_column(c(x, y), fill = "bg_{col}")

  expect_equal(tbl$col_styles$x$fill_col, "bg_x")
  expect_equal(tbl$col_styles$y$fill_col, "bg_y")
})

test_that("tt_column pattern expansion works for multiple style attributes", {
  df <- data.frame(
    val = 1:3,
    color_val = c("red", "green", "blue"),
    bg_val = c("white", "gray", "black"),
    bold_val = c(TRUE, FALSE, TRUE)
  )

  tbl <- tt(df, cols = val) |>
    tt_column(val, color = "color_{col}", fill = "bg_{col}", bold = "bold_{col}")

  expect_equal(tbl$col_styles$val$color_col, "color_val")
  expect_equal(tbl$col_styles$val$fill_col, "bg_val")
  expect_equal(tbl$col_styles$val$bold_col, "bold_val")
})

test_that("tt_column .missing = 'ignore' silently skips missing columns", {

  df <- data.frame(a = 1:3, b = 4:6)

  # Should not produce warning with .missing = "ignore"
  expect_silent({
    tbl <- tt(df) |>
      tt_column(a, color = "color_{col}", .missing = "ignore")
  })

  # color_col should not be set since color_a doesn't exist
  expect_null(tbl$col_styles$a$color_col)
})

test_that("tt_column .missing = 'warn' produces warning for missing columns", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_warning(
    tt(df) |>
      tt_column(a, color = "color_{col}", .missing = "warn"),
    "Column 'color_a' not found"
  )
})

test_that("tt_column .missing = 'error' stops for missing columns", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    tt(df) |>
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

  tbl <- tt(df, cols = c(a, b)) |>
    tt_column(a, color = "color_{col}", fill = my_bg, bold = TRUE)

  # Pattern should expand

  expect_equal(tbl$col_styles$a$color_col, "color_a")
  # Column ref should work
  expect_equal(tbl$col_styles$a$fill_col, "my_bg")
  # Static should work
  expect_true(tbl$col_styles$a$bold)
})

test_that("tt_column string without {col} is treated as static value", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |>
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

  tbl <- tt(df, cols = a) |>
    tt_column(a, font_size = "size_{col}", rotate = "angle_{col}")

  expect_equal(tbl$col_styles$a$font_size_col, "size_a")
  expect_equal(tbl$col_styles$a$rotate_col, "angle_a")
})

test_that("tt_column pattern works with italic", {
  df <- data.frame(
    a = 1:3,
    italic_a = c(TRUE, FALSE, TRUE)
  )

  tbl <- tt(df, cols = a) |>
    tt_column(a, italic = "italic_{col}")

  expect_equal(tbl$col_styles$a$italic_col, "italic_a")
})

# Last-write-wins precedence tests

test_that("last-write-wins: row after column wins", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |>
    tt_column(a, color = "blue") |>
    tt_row(1, color = "red")

  result <- tt_render(tbl)
  # Row 1, column a should have red (row was called last)
  expect_match(result, "red")
})

test_that("last-write-wins: column after row wins", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |>
    tt_row(1, color = "red") |>
    tt_column(a, color = "blue")

  result <- tt_render(tbl)
  # Row 1, column a should have blue (column was called last)
  expect_match(result, "blue")
})

test_that("last-write-wins: per-attribute independence", {
  df <- data.frame(a = 1:3, b = 4:6)

  # Row sets color=red, then column sets bold only
  # The row's color should survive since column didn't override it
  tbl <- tt(df) |>
    tt_row(1, color = "red") |>
    tt_column(a, bold = TRUE)

  result <- tt_render(tbl)
  # Should have both red and bold for row 1, col a
  expect_match(result, "red")
  expect_match(result, "\\*")
})

test_that("last-write-wins: non-conflicting attributes merge correctly", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |>
    tt_row(1, color = "red") |>
    tt_column(a, fill = "yellow")

  result <- tt_render(tbl)
  # Both should be present since they don't conflict

  expect_match(result, "red")
  expect_match(result, "yellow")
})

test_that("last-write-wins: data-driven column style respects sequence vs row style", {
  df <- data.frame(
    a = 1:3,
    color_a = c("blue", "green", "purple")
  )

  # Row sets color=red first, then data-driven column color is set after
  tbl <- tt(df, cols = a) |>
    tt_row(1, color = "red") |>
    tt_column(a, color = color_a)

  result <- tt_render(tbl)
  # Column was called last, so data-driven blue should win for row 1
  expect_match(result, "blue")
})

# Stroke parameter tests

test_that("tt_cell stores stroke in cell_styles", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_cell(1, 1, stroke = "2pt + red")
  expect_equal(tbl$cell_styles[["1_1"]]$stroke, "2pt + red")
})

test_that("tt_row stores stroke in row_styles", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_row(1, stroke = TRUE)
  expect_true(tbl$row_styles[["1"]]$stroke)
})

test_that("tt_column stores static stroke in col_styles", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_column(a, stroke = "(bottom: 1pt)")
  expect_equal(tbl$col_styles$a$stroke, "(bottom: 1pt)")
})

test_that("tt_column data-driven stroke via column ref", {
  df <- data.frame(a = 1:3, stroke_a = c("1pt + red", "1pt + blue", "1pt + green"))

  tbl <- tt(df, cols = a) |>
    tt_column(a, stroke = stroke_a)
  expect_equal(tbl$col_styles$a$stroke_col, "stroke_a")
})

# --- tt_pack_rows tests ---

test_that("tt_pack_rows index parameter with named numeric vector", {
  df <- data.frame(a = 1:8, b = 11:18)

  tbl <- tt(df) |>
    tt_pack_rows(index = c("Group A" = 3, "Group B" = 5))

  expect_length(tbl$row_groups, 2)
  expect_equal(tbl$row_groups[[1]]$group_label, "Group A")
  expect_equal(tbl$row_groups[[1]]$start_row, 1L)
  expect_equal(tbl$row_groups[[1]]$end_row, 3L)
  expect_equal(tbl$row_groups[[2]]$group_label, "Group B")
  expect_equal(tbl$row_groups[[2]]$start_row, 4L)
  expect_equal(tbl$row_groups[[2]]$end_row, 8L)
})

test_that("tt_pack_rows index respects indent and bold_label options", {
  df <- data.frame(a = 1:5, b = 6:10)

  tbl <- tt(df) |>
    tt_pack_rows(index = c("Group" = 5), indent = FALSE, bold_label = FALSE)

  expect_false(tbl$row_groups[[1]]$indent)
  expect_false(tbl$row_groups[[1]]$bold_label)
})

test_that("tt_pack_rows index must be named numeric", {
  df <- data.frame(a = 1:5, b = 6:10)

  # Unnamed numeric
  expect_error(
    tt(df) |> tt_pack_rows(index = c(3, 2)),
    "must be a named numeric vector"
  )

  # Named but not numeric
  expect_error(
    tt(df) |> tt_pack_rows(index = c("A" = "text")),
    "must be a named numeric vector"
  )
})

test_that("tt_pack_rows index row count must not exceed table rows", {
  df <- data.frame(a = 1:5, b = 6:10)

  expect_error(
    tt(df) |> tt_pack_rows(index = c("Group" = 10)),
    "rows but table only has 5 rows"
  )
})

test_that("tt_pack_rows group_label must be single character", {
  df <- data.frame(a = 1:5, b = 6:10)

  expect_error(
    tt(df) |> tt_pack_rows(group_label = c("A", "B"), start_row = 1, end_row = 3),
    "must be a single character string"
  )

  expect_error(
    tt(df) |> tt_pack_rows(group_label = 123, start_row = 1, end_row = 3),
    "must be a single character string"
  )
})

test_that("tt_pack_rows start_row and end_row must be single numbers", {
  df <- data.frame(a = 1:5, b = 6:10)

  expect_error(
    tt(df) |> tt_pack_rows("Group", start_row = c(1, 2), end_row = 3),
    "must be a single number"
  )

  expect_error(
    tt(df) |> tt_pack_rows("Group", start_row = 1, end_row = c(2, 3)),
    "must be a single number"
  )
})

test_that("tt_pack_rows row range bounds checking", {
  df <- data.frame(a = 1:5, b = 6:10)

  # start_row out of range
  expect_error(
    tt(df) |> tt_pack_rows("Group", start_row = 0, end_row = 3),
    "must be between 1 and 5"
  )

  expect_error(
    tt(df) |> tt_pack_rows("Group", start_row = 6, end_row = 6),
    "must be between 1 and 5"
  )

  # end_row out of range
  expect_error(
    tt(df) |> tt_pack_rows("Group", start_row = 1, end_row = 0),
    "must be between 1 and 5"
  )

  expect_error(
    tt(df) |> tt_pack_rows("Group", start_row = 1, end_row = 10),
    "must be between 1 and 5"
  )
})

# --- tt_style additional tests ---

test_that("tt_style position warns for invalid values", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_warning(
    tt(df) |> tt_style(position = "invalid"),
    "Invalid position"
  )
})

test_that("tt_style all valid positions work", {
  df <- data.frame(a = 1:3, b = 4:6)

  for (pos in c("auto", "left", "center", "right")) {
    tbl <- tt(df) |> tt_style(position = pos)
    expect_equal(tbl$position, pos)
  }
})

test_that("tt_style row_gutter and column_gutter parameters", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |>
    tt_style(row_gutter = "5pt", column_gutter = "10pt")

  expect_equal(tbl$row_gutter, "5pt")
  expect_equal(tbl$column_gutter, "10pt")
})

test_that("tt_style full_width = TRUE", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_style(full_width = TRUE)
  expect_true(tbl$full_width)
})

# --- tt_cell additional tests ---

test_that("tt_cell resolves column by character name", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_cell(1, "a", fill = "yellow")
  expect_equal(tbl$cell_styles[["1_1"]]$fill, "yellow")

  tbl2 <- tt(df) |> tt_cell(1, "b", fill = "blue")
  expect_equal(tbl2$cell_styles[["1_2"]]$fill, "blue")
})

test_that("tt_cell resolves column by numeric index", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_cell(1, 1, fill = "green")
  expect_equal(tbl$cell_styles[["1_1"]]$fill, "green")

  tbl2 <- tt(df) |> tt_cell(1, 2, fill = "red")
  expect_equal(tbl2$cell_styles[["1_2"]]$fill, "red")
})

test_that("tt_cell errors for invalid column name/index", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    tt(df) |> tt_cell(1, "nonexistent", fill = "red"),
    "Column not found"
  )

  expect_error(
    tt(df) |> tt_cell(1, 10, fill = "red"),
    "Column not found"
  )
})

test_that("tt_cell font_size, rotate, inset, stroke attributes", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |>
    tt_cell(1, 1, font_size = "14pt", rotate = "45deg", inset = "8pt", stroke = "2pt + red")

  expect_equal(tbl$cell_styles[["1_1"]]$font_size, "14pt")
  expect_equal(tbl$cell_styles[["1_1"]]$rotate, "45deg")
  expect_equal(tbl$cell_styles[["1_1"]]$inset, "8pt")
  expect_equal(tbl$cell_styles[["1_1"]]$stroke, "2pt + red")
})

test_that("tt_cell rowspan attribute", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_cell(1, 1, rowspan = 2)
  expect_equal(tbl$cell_styles[["1_1"]]$rowspan, 2)
})

test_that("tt_cell row range validation with no headers_above", {
  df <- data.frame(a = 1:3, b = 4:6)

  # Row 0 should be valid (header row)
  tbl <- tt(df) |> tt_cell(0, 1, bold = TRUE)
  expect_true(tbl$cell_styles[["0_1"]]$bold)

  # Negative row should error without headers_above
  expect_error(
    tt(df) |> tt_cell(-1, 1, bold = TRUE),
    "must be between"
  )
})

test_that("tt_cell italic attribute", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_cell(1, 1, italic = TRUE)
  expect_true(tbl$cell_styles[["1_1"]]$italic)
})

test_that("tt_cell align attribute", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_cell(1, 1, align = "center")
  expect_equal(tbl$cell_styles[["1_1"]]$cell_align, "center")
})

test_that("tt_cell row parameter validation", {
  df <- data.frame(a = 1:3, b = 4:6)

  # Multiple rows should error
  expect_error(
    tt(df) |> tt_cell(c(1, 2), 1, bold = TRUE),
    "must be a single row number"
  )
})

# --- tt_column additional tests ---

test_that("tt_column warns for empty column selection", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_warning(
    tt(df) |> tt_column(c(), bold = TRUE),
    "No columns matched"
  )
})

test_that("tt_column data-driven italic via column reference", {
  df <- data.frame(a = 1:3, is_italic = c(TRUE, FALSE, TRUE))

  tbl <- tt(df, cols = a) |>
    tt_column(a, italic = is_italic)

  expect_equal(tbl$col_styles$a$italic_col, "is_italic")
})

test_that("tt_column pattern expansion for inset and stroke", {
  df <- data.frame(
    a = 1:3,
    inset_a = c("5pt", "10pt", "15pt"),
    stroke_a = c("1pt + red", "1pt + blue", "1pt + green")
  )

  tbl <- tt(df, cols = a) |>
    tt_column(a, inset = "inset_{col}", stroke = "stroke_{col}")

  expect_equal(tbl$col_styles$a$inset_col, "inset_a")
  expect_equal(tbl$col_styles$a$stroke_col, "stroke_a")
})

test_that("tt_column border_right creates vlines", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  tbl <- tt(df) |> tt_column(a, border_right = "1pt + gray")

  expect_length(tbl$vlines, 1)
  expect_equal(tbl$vlines[[1]]$x, 1)  # After column 1
  expect_equal(tbl$vlines[[1]]$stroke, "1pt + gray")
})

test_that("tt_column border_left creates vlines", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  tbl <- tt(df) |> tt_column(b, border_left = "2pt + red")

  expect_length(tbl$vlines, 1)
  expect_equal(tbl$vlines[[1]]$x, 1)  # Before column 2
  expect_equal(tbl$vlines[[1]]$stroke, "2pt + red")
})

test_that("tt_column width parameter overrides default", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_column(a, width = "100pt")
  expect_equal(tbl$col_widths[1], "100pt")
})

test_that("tt_column data-driven bold via column reference", {
  df <- data.frame(a = 1:3, is_bold = c(TRUE, FALSE, TRUE))

  tbl <- tt(df, cols = a) |>
    tt_column(a, bold = is_bold)

  expect_equal(tbl$col_styles$a$bold_col, "is_bold")
})

test_that("tt_column data-driven font_size via column reference", {
  df <- data.frame(a = 1:3, my_size = c("10pt", "12pt", "14pt"))

  tbl <- tt(df, cols = a) |>
    tt_column(a, font_size = my_size)

  expect_equal(tbl$col_styles$a$font_size_col, "my_size")
})

test_that("tt_column data-driven rotate via column reference", {
  df <- data.frame(a = 1:3, my_angle = c("0deg", "45deg", "90deg"))

  tbl <- tt(df, cols = a) |>
    tt_column(a, rotate = my_angle)

  expect_equal(tbl$col_styles$a$rotate_col, "my_angle")
})

test_that("tt_column data-driven inset via column reference", {
  df <- data.frame(a = 1:3, my_inset = c("5pt", "10pt", "15pt"))

  tbl <- tt(df, cols = a) |>
    tt_column(a, inset = my_inset)

  expect_equal(tbl$col_styles$a$inset_col, "my_inset")
})

test_that("tt_column missing column ref warns by default", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_warning(
    tt(df) |> tt_column(a, bold = nonexistent),
    "not found"
  )
})

test_that("tt_column missing font_size ref warns", {
  df <- data.frame(a = 1:3)

  expect_warning(
    tt(df, cols = a) |>
      tt_column(a, font_size = "missing_{col}"),
    "not found"
  )
})

test_that("tt_column missing rotate ref warns", {
  df <- data.frame(a = 1:3)

  expect_warning(
    tt(df, cols = a) |>
      tt_column(a, rotate = "missing_{col}"),
    "not found"
  )
})

test_that("tt_column missing inset ref warns", {
  df <- data.frame(a = 1:3)

  expect_warning(
    tt(df, cols = a) |>
      tt_column(a, inset = "missing_{col}"),
    "not found"
  )
})

test_that("tt_column missing stroke ref warns", {
  df <- data.frame(a = 1:3)

  expect_warning(
    tt(df, cols = a) |>
      tt_column(a, stroke = "missing_{col}"),
    "not found"
  )
})

# --- Coverage: missing bare-symbol column ref warnings for all style attributes ---

test_that("tt_column missing italic bare-symbol ref warns", {
  df <- data.frame(a = 1:3)

  expect_warning(
    tt(df, cols = a) |>
      tt_column(a, italic = nonexistent),
    "not found"
  )
})

test_that("tt_column missing color bare-symbol ref warns", {
  df <- data.frame(a = 1:3)

  expect_warning(
    tt(df, cols = a) |>
      tt_column(a, color = nonexistent),
    "not found"
  )
})

test_that("tt_column missing fill bare-symbol ref warns", {
  df <- data.frame(a = 1:3)

  expect_warning(
    tt(df, cols = a) |>
      tt_column(a, fill = nonexistent),
    "not found"
  )
})

test_that("tt_column missing font_size bare-symbol ref warns", {
  df <- data.frame(a = 1:3)

  expect_warning(
    tt(df, cols = a) |>
      tt_column(a, font_size = nonexistent),
    "not found"
  )
})

test_that("tt_column missing rotate bare-symbol ref warns", {
  df <- data.frame(a = 1:3)

  expect_warning(
    tt(df, cols = a) |>
      tt_column(a, rotate = nonexistent),
    "not found"
  )
})

test_that("tt_column missing inset bare-symbol ref warns", {
  df <- data.frame(a = 1:3)

  expect_warning(
    tt(df, cols = a) |>
      tt_column(a, inset = nonexistent),
    "not found"
  )
})

test_that("tt_column missing stroke bare-symbol ref warns", {
  df <- data.frame(a = 1:3)

  expect_warning(
    tt(df, cols = a) |>
      tt_column(a, stroke = nonexistent),
    "not found"
  )
})

test_that("tt_column static italic value works", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_column(a, italic = TRUE)
  expect_true(tbl$col_styles$a$italic)
})

test_that("tt_column static inset value works", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_column(a, inset = "10pt")
  expect_equal(tbl$col_styles$a$inset, "10pt")
})

test_that("tt_column static rotate value works", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl <- tt(df) |> tt_column(a, font_size = "14pt")
  expect_equal(tbl$col_styles$a$font_size, "14pt")
})

# --- tt_lines additional tests ---

test_that("tt_hline missing y errors", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    tt(df) |> tt_hline(),
    "must be a single number"
  )
})

test_that("tt_hline warns for out-of-range y", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_warning(
    tt(df) |> tt_hline(y = 10),
    "should be between 0 and 4"
  )

  expect_warning(
    tt(df) |> tt_hline(y = -1),
    "should be between 0 and 4"
  )
})

test_that("tt_vline missing x errors", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    tt(df) |> tt_vline(),
    "must be a single number"
  )
})

test_that("tt_vline warns for out-of-range x", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_warning(
    tt(df) |> tt_vline(x = 10),
    "should be between 0 and 2"
  )

  expect_warning(
    tt(df) |> tt_vline(x = -1),
    "should be between 0 and 2"
  )
})
