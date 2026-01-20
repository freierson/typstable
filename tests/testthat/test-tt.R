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
