test_that("tt_save validates input arguments", {
  tbl <- tt(mtcars[1:3, 1:3])

  # Invalid table
  expect_error(tt_save("not a table", tempfile(fileext = ".svg")),
               "must be a typst_table object")

  # Invalid file argument
  expect_error(tt_save(tbl, ""), "non-empty character")
  expect_error(tt_save(tbl, c("a.svg", "b.svg")), "non-empty character")
  expect_error(tt_save(tbl, 123), "non-empty character")

  # Invalid extension
  expect_error(tt_save(tbl, "output.txt"), "Unsupported file extension")
  expect_error(tt_save(tbl, "output.html"), "Unsupported file extension")

  # Invalid ppi
  expect_error(tt_save(tbl, tempfile(fileext = ".png"), ppi = -1),
               "must be a positive number")
  expect_error(tt_save(tbl, tempfile(fileext = ".png"), ppi = "high"),
               "must be a positive number")

  # Invalid overwrite
  expect_error(tt_save(tbl, tempfile(fileext = ".svg"), overwrite = "yes"),
               "must be TRUE or FALSE")
})

test_that("tt_save writes .typ files without Typst CLI", {
  tbl <- tt(mtcars[1:3, 1:3])
  temp_file <- tempfile(fileext = ".typ")
  on.exit(unlink(temp_file), add = TRUE)

  result <- tt_save(tbl, temp_file)

  expect_equal(result, temp_file)
  expect_true(file.exists(temp_file))

  # Check content contains table code
  content <- readLines(temp_file)
  content_str <- paste(content, collapse = "\n")
  expect_match(content_str, "#table\\(")
  expect_match(content_str, "columns:")
})

test_that("tt_save respects overwrite argument for .typ", {
  tbl <- tt(mtcars[1:3, 1:3])
  temp_file <- tempfile(fileext = ".typ")
  on.exit(unlink(temp_file), add = TRUE)

  # First write succeeds
  tt_save(tbl, temp_file)
  expect_true(file.exists(temp_file))

  # Second write with overwrite = FALSE should fail
  expect_error(tt_save(tbl, temp_file, overwrite = FALSE),
               "already exists")

  # Second write with overwrite = TRUE should succeed
  expect_silent(tt_save(tbl, temp_file, overwrite = TRUE))
})

test_that("tt_save writes styled tables to .typ", {
  tbl <- tt(mtcars[1:3, 1:3]) |>
    tt_style(stroke = TRUE, striped = TRUE)
  temp_file <- tempfile(fileext = ".typ")
  on.exit(unlink(temp_file), add = TRUE)

  tt_save(tbl, temp_file)
  content <- paste(readLines(temp_file), collapse = "\n")

  # Styled table should have stroke setting
  expect_match(content, "stroke:")
  expect_match(content, "fill:")
})

test_that("tt_typst_available returns logical", {
  result <- tt_typst_available()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("tt_typst_available handles explicit path", {
  # Non-existent path should return FALSE
  expect_false(tt_typst_available("/nonexistent/path/to/typst"))

  # If typst is available on PATH, explicit path should work
  if (tt_typst_available()) {
    typst_path <- Sys.which("typst")
    expect_true(tt_typst_available(typst_path))
  }
})

# Tests that require Typst CLI
test_that("tt_save creates SVG output", {
  skip_if_not(tt_typst_available(), "Typst CLI not available")

  tbl <- tt(mtcars[1:3, 1:3])
  temp_file <- tempfile(fileext = ".svg")
  on.exit(unlink(temp_file), add = TRUE)

  result <- tt_save(tbl, temp_file)

  expect_equal(result, temp_file)
  expect_true(file.exists(temp_file))

  # Check it's valid SVG
  content <- readLines(temp_file, n = 2)
  expect_match(paste(content, collapse = ""), "<svg|\\?xml")
})

test_that("tt_save creates PNG output", {
  skip_if_not(tt_typst_available(), "Typst CLI not available")

  tbl <- tt(mtcars[1:3, 1:3])
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  result <- tt_save(tbl, temp_file)

  expect_equal(result, temp_file)
  expect_true(file.exists(temp_file))

  # Check file size is reasonable (PNG header is at least a few hundred bytes)
  expect_gt(file.info(temp_file)$size, 100)
})

test_that("tt_save creates PDF output", {
  skip_if_not(tt_typst_available(), "Typst CLI not available")

  tbl <- tt(mtcars[1:3, 1:3])
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  result <- tt_save(tbl, temp_file)

  expect_equal(result, temp_file)
  expect_true(file.exists(temp_file))

  # Check it starts with PDF magic number
  content <- readBin(temp_file, "raw", n = 5)
  expect_equal(rawToChar(content), "%PDF-")
})

test_that("tt_save respects ppi parameter for PNG", {
  skip_if_not(tt_typst_available(), "Typst CLI not available")

  tbl <- tt(mtcars[1:3, 1:3])
  temp_low <- tempfile(fileext = ".png")
  temp_high <- tempfile(fileext = ".png")
  on.exit(unlink(c(temp_low, temp_high)), add = TRUE)

  tt_save(tbl, temp_low, ppi = 72)
  tt_save(tbl, temp_high, ppi = 288)

  # Higher PPI should produce larger file
  size_low <- file.info(temp_low)$size
  size_high <- file.info(temp_high)$size
  expect_gt(size_high, size_low)
})

test_that("tt_save handles page dimensions", {
  skip_if_not(tt_typst_available(), "Typst CLI not available")

  tbl <- tt(mtcars[1:3, 1:3])

  # Auto dimensions (default)
  temp_auto <- tempfile(fileext = ".svg")
  on.exit(unlink(temp_auto), add = TRUE)
  tt_save(tbl, temp_auto)
  expect_true(file.exists(temp_auto))

  # Explicit dimensions
  temp_fixed <- tempfile(fileext = ".svg")
  on.exit(unlink(temp_fixed), add = TRUE)
  tt_save(tbl, temp_fixed, width = "6in", height = "4in")
  expect_true(file.exists(temp_fixed))
})

test_that("tt_save handles styled tables correctly", {
  skip_if_not(tt_typst_available(), "Typst CLI not available")

  tbl <- tt(mtcars[1:5, 1:3]) |>
    tt_style(stroke = TRUE, striped = TRUE) |>
    tt_row(1, bold = TRUE)

  temp_file <- tempfile(fileext = ".svg")
  on.exit(unlink(temp_file), add = TRUE)

  expect_silent(tt_save(tbl, temp_file))
  expect_true(file.exists(temp_file))
})
