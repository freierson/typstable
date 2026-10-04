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

  result <- capture.output(vis <- withVisible(print(tbl)))

  expect_false(vis$visible)
  expect_true(is.character(vis$value))
  expect_true(grepl("#table\\(", vis$value))
})

test_that("print.typst_table returns the Typst code string", {
  df <- data.frame(a = 1:2, b = 3:4)
  tbl <- tt(df)

  result <- capture.output(code <- print(tbl))

  expect_true(is.character(code))
  expect_equal(code, tt_render(tbl))
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

test_that("knit_print.typst_table emits unwrapped Typst when knitting to typst", {
  skip_if_not_installed("knitr")

  old <- knitr::opts_knit$get("out.format")
  knitr::opts_knit$set(out.format = "typst")
  on.exit(knitr::opts_knit$set(out.format = old), add = TRUE)

  tbl <- tt(data.frame(a = 1:2, b = 3:4))
  result_str <- as.character(knit_print.typst_table(tbl))

  # No Pandoc raw block fence: typst compiles the .typ directly
  expect_false(grepl("```", result_str, fixed = TRUE))
  expect_true(grepl("\n#table\\(", result_str))
})

test_that("knitting an .Rtyp document produces a raw #table, not a fenced block", {
  skip_if_not_installed("knitr", minimum_version = "1.50")

  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  input <- file.path(dir, "doc.Rtyp")
  writeLines(c(
    "Text.",
    "",
    "```{r}",
    "#| echo: false",
    "#| results: asis",
    "tt(data.frame(col1 = c(1, 2), col2 = c(\"a\", \"b\")), rownames = FALSE)",
    "```"
  ), input)

  output <- knitr::knit(input, output = file.path(dir, "doc.typ"),
                        quiet = TRUE, envir = environment())
  out <- readLines(output)

  expect_false(any(grepl("{=typst}", out, fixed = TRUE)))
  expect_true(any(grepl("^#table\\(", out)))
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

test_that(".onLoad registers knit_print method without error", {
  skip_if_not_installed("knitr")

  # .onLoad should run without error when knitr is available and loaded
  expect_no_error(
    typstable:::.onLoad(NULL, "typstable")
  )
})
