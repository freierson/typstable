# Tests for print.typst_table and knit_print.typst_table

test_that("print.typst_table outputs Typst code to console", {
  df <- data.frame(a = 1:2, b = 3:4)
  tbl <- tt(df)

  output <- capture.output(print(tbl))
  output_str <- paste(output, collapse = "\n")

  expect_true(grepl("#table\\(", output_str))
  expect_true(grepl("columns:", output_str))
})

test_that("print.typst_table returns invisibly", {
  df <- data.frame(a = 1:2, b = 3:4)
  tbl <- tt(df)

  result <- withVisible(print(tbl))

  expect_false(result$visible)
  expect_true(is.character(result$value))
  expect_true(grepl("#table\\(", result$value))
})

test_that("print.typst_table returns the Typst code string", {
  df <- data.frame(a = 1:2, b = 3:4)
  tbl <- tt(df)

  result <- print(tbl)

  expect_true(is.character(result))
  expect_equal(result, tt_render(tbl))
})

test_that("knit_print.typst_table returns knitr::asis_output with raw Typst block", {
  skip_if_not_installed("knitr")

  df <- data.frame(a = 1:2, b = 3:4)
  tbl <- tt(df)

  result <- knit_print.typst_table(tbl)

  expect_s3_class(result, "knit_asis")
  result_str <- as.character(result)

  # Should be wrapped in ```{=typst} ... ``` block

  expect_true(grepl("```\\{=typst\\}", result_str))
  expect_true(grepl("#table\\(", result_str))
  expect_true(grepl("```\n$", result_str))
})

test_that("knit_print.typst_table includes all table content", {
  skip_if_not_installed("knitr")

  df <- data.frame(a = 1:2, b = 3:4)
  tbl <- tt(df) |>
    tt_style(stroke = TRUE) |>
    tt_column(a, bold = TRUE)

  result <- knit_print.typst_table(tbl)
  result_str <- as.character(result)

  expect_true(grepl("stroke:", result_str))
  expect_true(grepl("\\*", result_str))
})
