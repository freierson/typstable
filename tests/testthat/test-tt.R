test_that("tt creates typst_table object", {
  tbl <- tt(mtcars[1:3, 1:3])
  expect_s3_class(tbl, "typst_table")
  expect_equal(tbl$nrow, 3)
  expect_equal(tbl$ncol, 3)
})

test_that("tt accepts data.frame and tibble", {
  df <- data.frame(a = 1:3, b = letters[1:3])
  tbl1 <- tt(df)
  expect_s3_class(tbl1, "typst_table")

  if (requireNamespace("tibble", quietly = TRUE)) {
    tib <- tibble::tibble(a = 1:3, b = letters[1:3])
    tbl2 <- tt(tib)
    expect_s3_class(tbl2, "typst_table")
  }
})

test_that("tt handles cols selection", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  tbl <- tt(df, cols = c(a, c))
  expect_equal(tbl$ncol, 2)
  expect_equal(tbl$display_cols, c("a", "c"))
})

test_that("tt handles custom col_names", {
  df <- data.frame(a = 1:3, b = 4:6)
  tbl <- tt(df, col_names = c("Column A", "Column B"))
  expect_equal(tbl$col_names, c("Column A", "Column B"))
})

test_that("tt validates col_names length", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_error(tt(df, col_names = c("A")), "must have 2 elements")
})

test_that("tt handles alignment", {
  df <- data.frame(a = 1:3, b = 4:6)

  tbl1 <- tt(df, align = "right")
  expect_equal(tbl1$col_align, c("right", "right"))

  tbl2 <- tt(df, align = c("left", "right"))
  expect_equal(tbl2$col_align, c("left", "right"))
})

test_that("tt_widths sets proportional widths", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  tbl <- tt(df) |> tt_widths(1, 2, 1)
  expect_equal(tbl$col_widths, c("1fr", "2fr", "1fr"))
})

test_that("tt_widths accepts auto mixed with numeric", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  tbl <- tt(df) |> tt_widths("auto", 2, 1)
  expect_equal(tbl$col_widths, c("auto", "2fr", "1fr"))
})

test_that("tt_widths works with named columns", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  tbl <- tt(df) |> tt_widths(a = 1, b = 2, c = 1)
  expect_equal(tbl$col_widths, c("1fr", "2fr", "1fr"))
})

test_that("tt_widths validates width count", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_error(tt(df) |> tt_widths(1, 2, 3), "Expected 2 widths")
})

test_that("tt stores escape setting", {
  tbl1 <- tt(mtcars[1:3, 1:3], escape = TRUE)
  expect_true(tbl1$escape)

  tbl2 <- tt(mtcars[1:3, 1:3], escape = FALSE)
  expect_false(tbl2$escape)
})

test_that("tt stores preamble", {
  tbl <- tt(mtcars[1:3, 1:3], preamble = '#set text(font: "Arial")')
  expect_equal(tbl$preamble, '#set text(font: "Arial")')
})

test_that("tt preamble is NULL by default", {
  tbl <- tt(mtcars[1:3, 1:3])
  expect_null(tbl$preamble)
})

test_that("tt stores epilogue", {
  tbl <- tt(mtcars[1:3, 1:3], epilogue = '#emph[Source: mtcars]')
  expect_equal(tbl$epilogue, '#emph[Source: mtcars]')
})

test_that("tt epilogue is NULL by default", {
  tbl <- tt(mtcars[1:3, 1:3])
  expect_null(tbl$epilogue)
})

# --- Input validation tests ---

test_that("tt rejects non-data.frame input", {
  expect_error(tt(list(a = 1:3)), "must be a data.frame")
  expect_error(tt(matrix(1:9, 3, 3)), "must be a data.frame")
  expect_error(tt(1:10), "must be a data.frame")
})

test_that("tt align validation wrong length", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  expect_error(
    tt(df, align = c("left", "right")),
    "must be a single value or have 3 elements"
  )
})

# --- testing different ways to set column widths

test_that("tt_widths string passthrough: arbitrary Typst lengths accepted", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  tbl <- tt(df) |> tt_widths("100pt", "50%", "2cm")
  expect_equal(tbl$col_widths, c("100pt", "50%", "2cm"))
})

test_that("tt_widths named partial update keeps unmentioned columns unchanged", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  tbl <- tt(df) |> tt_widths(b = "1cm")
  expect_equal(tbl$col_widths, c("auto", "1cm", "auto"))
})

test_that("tt_widths named update with numeric uses .unit", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  tbl <- tt(df) |> tt_widths(b = 1)
  expect_equal(tbl$col_widths, c("auto", "1fr", "auto"))
})

test_that("tt_column width passed through as-is", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  tbl <- tt(df) |> tt_column(b, width = "1fr")
  expect_equal(tbl$col_widths[2], "1fr")
})

test_that("tt_widths .default overwrites unmentioned columns", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  tbl <- tt(df) |> tt_widths(b = 2, .default = "1fr")
  expect_equal(tbl$col_widths, c("1fr", "2fr", "1fr"))
})

test_that("tt_widths custom .unit function", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  tbl <- tt(df) |> tt_widths(1, 2, 1, .unit = \(x) paste0(x, "pt"))
  expect_equal(tbl$col_widths, c("1pt", "2pt", "1pt"))
})

test_that("tt_widths last-value-in-pipe wins", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  tbl <- tt(df) |> tt_widths(1, 1, 1) |> tt_widths(b = 2)
  expect_equal(tbl$col_widths, c("1fr", "2fr", "1fr"))
})

test_that("tt_widths fails if given just one positional argument", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  expect_error(
    tt(df) |> tt_widths(1),
    "Expected 3 widths, got 1"
  )
})

test_that("tt_widths works if given positional arguments", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  tbl1 <- tt(df, rownames = TRUE) |>
    tt_widths(1,2,3,4)
  tbl2 <- tt(df)|>
    tt_widths(1,2,3)
  expect_equal(tbl1$col_widths, c("1fr", "2fr", "3fr", "4fr"))
  expect_equal(tbl2$col_widths, c("1fr", "2fr", "3fr"))
})

# --- tt_widths validation tests ---
test_that("tt_widths mixing named/unnamed error", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  expect_error(
    tt(df) |> tt_widths(a = 1, 2, c = 1),
    "Cannot mix named and unnamed widths"
  )
})

test_that("tt_widths invalid column name error", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    tt(df) |> tt_widths(a = 1, nonexistent = 2),
    "Column 'nonexistent' not found"
  )
})

test_that("tt_widths requires at least one width", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    tt(df) |> tt_widths(),
    "At least one width must be provided"
  )
})

# --- rownames handling tests ---

test_that("rownames = TRUE with NULL rownames uses numeric indices", {
  df <- data.frame(a = 1:3, b = 4:6)
  rownames(df) <- NULL

  tbl <- tt(df, rownames = TRUE)

  # Should have 3 columns (rownames + a + b)
  expect_equal(tbl$ncol, 3)
  # First column should be the rownames (as character numbers)
  expect_equal(tbl$display_data[, 1], c("1", "2", "3"))
})

test_that("rownames default uses numeric indices", {
  df <- data.frame(a = 1:3, b = 4:6)
  tbl <- tt(df)
  tbl_out <- trimws(paste0(capture.output(print(tbl)),collapse='\n'))
  expect_equal(tbl$ncol, 2)
  expect_equal(tbl_out,
  "#table(\n  columns: (auto, auto),\n  stroke: none,\n  table.header(\n    table.hline(stroke: 1pt),\n    [a], [b],\n    table.hline(stroke: 0.5pt)\n  ),\n  [1], [4],\n  [2], [5],\n  [3], [6],\n  table.hline(stroke: 1pt)\n)")
})

test_that("rownames = TRUE with cols selection includes rownames column", {
  df <- data.frame(a = 1:3, b = 4:6)
  rownames(df) <- c("r1", "r2", "r3")

  tbl <- tt(df, rownames = TRUE, cols = a)
  expect_equal(tbl$ncol, 2)
  expect_true(".rownames" %in% tbl$display_cols)
})

test_that("col_widths wrong length errors", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_error(tt(df, col_widths = c("1fr", "2fr", "3fr")), "must be")
})
