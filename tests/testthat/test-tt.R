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

test_that("tt_widths rejects non-positive widths", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    tt(df) |> tt_widths(0, 1),
    "must be positive numbers"
  )

  expect_error(
    tt(df) |> tt_widths(-1, 1),
    "must be positive numbers"
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
  "#table(\n  columns: (1fr, 1fr),\n  stroke: none,\n  table.header(\n    table.hline(stroke: 1pt),\n    [a], [b],\n    table.hline(stroke: 0.5pt)\n  ),\n  [1], [4],\n  [2], [5],\n  [3], [6],\n  table.hline(stroke: 1pt)\n)")
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
