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

test_that("tt_render handles header_above", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |> tt_header_above(c("Group" = 2)) |> tt_render()

  expect_true(grepl("colspan: 2", code))
  expect_true(grepl("Group", code))
})

test_that("no table.header() wrapper without headers_above", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |> tt_render()

  expect_false(grepl("table\\.header\\(", code))
})

test_that("tt_render handles cell fill", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |> tt_cell(1, 1, fill = "yellow") |> tt_render()

  expect_true(grepl("table\\.cell\\(", code))
  expect_true(grepl("fill:", code))
})

# --- Negative row index tests (header_above targeting) ---

test_that("tt_row() accepts negative indices for header_above rows", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |>
    tt_header_above(c("Group" = 2)) |>
    tt_row(-1, italic = TRUE) |>
    tt_render()

  # The header_above row should be rendered with italic
  expect_true(grepl("_\\*Group\\*_", code))
})

test_that("tt_row() warns for out-of-range negative indices", {
  df <- data.frame(a = 1:2, b = 3:4)
  tbl <- tt(df) |>
    tt_header_above(c("Group" = 2))

  expect_warning(tt_row(tbl, -2, bold = TRUE), "out of range")
})

test_that("tt_row() warns about hlines on header_above rows", {
  df <- data.frame(a = 1:2, b = 3:4)
  tbl <- tt(df) |>
    tt_header_above(c("Group" = 2))

  expect_warning(tt_row(tbl, -1, hline_above = TRUE), "not supported")
})

test_that("tt_cell() accepts negative indices for header_above rows", {
  df <- data.frame(a = 1:2, b = 3:4, c = 5:6, d = 7:8)
  code <- tt(df) |>
    tt_header_above(c("AB" = 2, "CD" = 2)) |>
    tt_cell(-1, 1, color = "red") |>
    tt_render()

  # The first group should have red text
  expect_true(grepl("fill: red", code) || grepl("fill:.*red", code) || grepl("text\\(fill: red\\)", code))
})

test_that("tt_cell() normalizes column to group start", {
  df <- data.frame(a = 1:2, b = 3:4, c = 5:6, d = 7:8)
  # Column 2 falls within group "AB" (cols 1-2), so should normalize to col 1
  code1 <- tt(df) |>
    tt_header_above(c("AB" = 2, "CD" = 2)) |>
    tt_cell(-1, 1, fill = "blue") |>
    tt_render()
  code2 <- tt(df) |>
    tt_header_above(c("AB" = 2, "CD" = 2)) |>
    tt_cell(-1, 2, fill = "blue") |>
    tt_render()

  # Both should produce the same output (col 2 normalizes to col 1)
  expect_identical(code1, code2)
})

test_that("tt_cell() errors for invalid negative row index", {
  df <- data.frame(a = 1:2, b = 3:4)
  tbl <- tt(df) |>
    tt_header_above(c("Group" = 2))

  expect_error(tt_cell(tbl, -2, 1, bold = TRUE), "must be between")
})

test_that("tt_cell() warns about colspan/rowspan on header_above cells", {
  df <- data.frame(a = 1:2, b = 3:4, c = 5:6, d = 7:8)
  tbl <- tt(df) |>
    tt_header_above(c("AB" = 2, "CD" = 2))

  expect_warning(tt_cell(tbl, -1, 1, colspan = 2), "not supported")
})

test_that("tt_cell() content override works for header_above", {
  df <- data.frame(a = 1:2, b = 3:4, c = 5:6, d = 7:8)
  code <- tt(df) |>
    tt_header_above(c("AB" = 2, "CD" = 2)) |>
    tt_cell(-1, 1, content = "NewLabel") |>
    tt_render()

  expect_true(grepl("NewLabel", code))
  expect_false(grepl("\\*AB\\*", code))
})

test_that("tt_header_above new params render correctly", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |>
    tt_header_above(c("Group" = 2), italic = TRUE, font_size = "14pt") |>
    tt_render()

  # Should have italic markup and text size
  expect_true(grepl("_", code))
  expect_true(grepl("14pt", code))
})

test_that("tt_row() override takes precedence over header_spec defaults", {
  df <- data.frame(a = 1:2, b = 3:4)
  # header_above defaults to bold=TRUE, override with bold=FALSE via tt_row
  code <- tt(df) |>
    tt_header_above(c("Group" = 2)) |>
    tt_row(-1, bold = FALSE) |>
    tt_render()

  # Group should NOT be wrapped in *...* (bold)
  expect_false(grepl("\\*Group\\*", code))
})

test_that("multiple header_above rows addressable by negative index", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |>
    tt_header_above(c("Inner" = 2)) |>
    tt_header_above(c("Outer" = 2)) |>
    tt_row(-1, fill = "yellow") |>
    tt_row(-2, italic = TRUE) |>
    tt_render()

  # -1 = innermost (Inner), -2 = outermost (Outer)
  # Inner group should have yellow background
  expect_true(grepl("yellow", code))
  # Outer group should be italic
  expect_true(grepl("_.*Outer.*_", code))
})

# Stroke rendering tests

test_that("cell stroke renders as table.cell(stroke: ...) in output", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |>
    tt_cell(1, 1, stroke = "2pt + red") |>
    tt_render()

  expect_true(grepl("table\\.cell\\(.*stroke: 2pt \\+ red", code))
})

test_that("row stroke applies to all cells in the row", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |>
    tt_row(1, stroke = TRUE) |>
    tt_render()

  # Should have stroke: 1pt + black for cells in row 1
  expect_true(grepl("stroke: 1pt \\+ black", code))
})

test_that("column stroke applies to data cells in the column", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |>
    tt_column(a, stroke = "(bottom: 1pt)") |>
    tt_render()

  expect_true(grepl("stroke: \\(bottom: 1pt\\)", code))
})

# Preamble tests

test_that("preamble is prepended before table code", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df, preamble = '#set text(font: "Arial")') |>
    tt_render()

  expect_true(grepl('^#set text\\(font: "Arial"\\)\n#table\\(', code))
})

test_that("no preamble by default", {
  df <- data.frame(a = 1:2, b = 3:4)
  code <- tt(df) |> tt_render()

  expect_true(grepl("^#table\\(", code))
})

test_that("NA renders correct by default", {
  df <- data.frame(a = c(NA, 'abc'), b = c(3, NA))
  code <- tt(df) |> tt_render()
  expect_true(grepl("[-], [3]", code, fixed = TRUE))
  expect_true(grepl("[abc], [-]", code, fixed = TRUE))
})

test_that("na_string argument works renders correct by default", {
  df <- data.frame(a = c(NA, 'abc'), b = c(3, NA))
  code <- tt(df, na_string = '') |> tt_render()
  expect_true(grepl("[], [3]", code, fixed = TRUE))
  expect_true(grepl("[abc], []", code, fixed = TRUE))
})

# --- Vertical lines rendering tests ---

test_that("vlines render with all parameters", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  code <- tt(df) |>
    tt_vline(x = 1, start = 1, end = 2, stroke = "2pt + blue") |>
    tt_render()

  expect_true(grepl("table\\.vline\\(", code))
  expect_true(grepl("x: 1", code))
  expect_true(grepl("start: 1", code))
  expect_true(grepl("end: 3", code))  # end is incremented by 1 in render
  expect_true(grepl("stroke: 2pt \\+ blue", code))
})

test_that("vlines render with minimal parameters", {
  df <- data.frame(a = 1:3, b = 4:6)

  code <- tt(df) |>
    tt_vline(x = 1) |>
    tt_render()

  expect_true(grepl("table\\.vline\\(x: 1\\)", code))
})

# --- Row groups rendering tests ---

test_that("group label row renders with colspan", {
  df <- data.frame(a = 1:5, b = 6:10, c = 11:15)

  code <- tt(df) |>
    tt_pack_rows("My Group", 1, 3) |>
    tt_render()

  expect_true(grepl("table\\.cell\\(colspan: 3\\)\\[\\*My Group\\*\\]", code))
})

test_that("group label without bold renders correctly", {
  df <- data.frame(a = 1:5, b = 6:10)

  code <- tt(df) |>
    tt_pack_rows("Plain Group", 1, 3, bold_label = FALSE) |>
    tt_render()

  expect_true(grepl("table\\.cell\\(colspan: 2\\)\\[Plain Group\\]", code))
  expect_false(grepl("\\*Plain Group\\*", code))
})

test_that("group label escapes special characters", {
  df <- data.frame(a = 1:5, b = 6:10)

  code <- tt(df) |>
    tt_pack_rows("Group*with_special#chars", 1, 3) |>
    tt_render()

  expect_true(grepl("Group\\\\\\*with\\\\_special\\\\#chars", code))
})

# --- Horizontal lines rendering tests ---

test_that("hline with stroke renders correctly", {
  df <- data.frame(a = 1:3, b = 4:6)

  code <- tt(df) |>
    tt_hline(y = 2, stroke = "2pt + red") |>
    tt_render()

  expect_true(grepl("table\\.hline\\(stroke: 2pt \\+ red\\)", code))
})

test_that("hline with start/end columns renders correctly", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  code <- tt(df) |>
    tt_hline(y = 2, start = 0, end = 1) |>
    tt_render()

  expect_true(grepl("table\\.hline\\(start: 0, end: 2\\)", code))
})

test_that("hline below row via tt_row renders correctly", {
  df <- data.frame(a = 1:3, b = 4:6)

  code <- tt(df) |>
    tt_row(1, hline_below = TRUE) |>
    tt_render()

  # Should have an hline after row 1
  expect_true(grepl("table\\.hline\\(\\)", code))
})

# --- Cell spanning rendering tests ---

test_that("rowspan renders in table.cell", {
  df <- data.frame(a = 1:3, b = 4:6)

  code <- tt(df) |>
    tt_cell(1, 1, rowspan = 2) |>
    tt_render()

  expect_true(grepl("table\\.cell\\(rowspan: 2\\)", code))
})

test_that("colspan and rowspan together render correctly", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  code <- tt(df) |>
    tt_cell(1, 1, colspan = 2, rowspan = 2) |>
    tt_render()

  expect_true(grepl("colspan: 2", code))
  expect_true(grepl("rowspan: 2", code))
})

# --- Row gutter and column gutter rendering ---

test_that("row_gutter renders in table arguments", {
  df <- data.frame(a = 1:2, b = 3:4)

  code <- tt(df) |>
    tt_style(row_gutter = "5pt") |>
    tt_render()

  expect_true(grepl("row-gutter: 5pt", code))
})

test_that("column_gutter renders in table arguments", {
  df <- data.frame(a = 1:2, b = 3:4)

  code <- tt(df) |>
    tt_style(column_gutter = "8pt") |>
    tt_render()

  expect_true(grepl("column-gutter: 8pt", code))
})
