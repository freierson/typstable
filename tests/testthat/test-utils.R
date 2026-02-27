test_that(".escape_typst escapes special characters", {
  expect_equal(.escape_typst("hello"), "hello")
  expect_equal(.escape_typst("hello*world"), "hello\\*world")
  expect_equal(.escape_typst("a_b"), "a\\_b")
  expect_equal(.escape_typst("#test"), "\\#test")
  expect_equal(.escape_typst("@mention"), "\\@mention")
})

test_that(".escape_typst escapes backslashes", {
  expect_equal(.escape_typst("a\\b"), "a\\\\b")
  expect_equal(.escape_typst("\\$5.00"), "\\\\$5.00")
})

test_that(".escape_typst handles empty strings", {
  expect_equal(.escape_typst(""), "")
})

test_that(".to_typst_color handles various formats", {
  expect_equal(.to_typst_color("red"), "red")
  expect_equal(.to_typst_color("blue"), "blue")
  expect_equal(.to_typst_color("#ff0000"), 'rgb("#ff0000")')
  expect_equal(.to_typst_color("ff0000"), 'rgb("#ff0000")')
  expect_equal(.to_typst_color('rgb("#123")'), 'rgb("#123")')
})

test_that(".to_typst_color handles R color names", {
  result <- .to_typst_color("steelblue")
  expect_true(grepl("^rgb\\(", result))
})

test_that(".to_typst_length validates units", {
  expect_equal(.to_typst_length(10), "10pt")
  expect_equal(.to_typst_length("5em"), "5em")
  expect_equal(.to_typst_length("10%"), "10%")
  expect_equal(.to_typst_length("2fr", allow_fr = TRUE), "2fr")
  expect_equal(.to_typst_length("auto"), "auto")
})

test_that(".to_typst_align handles abbreviations",
{
  expect_equal(.to_typst_align("l"), "left")
  expect_equal(.to_typst_align("c"), "center")
  expect_equal(.to_typst_align("r"), "right")
  expect_equal(.to_typst_align("left"), "left")
})

test_that(".to_typst_color passes through unrecognized strings as raw Typst", {
  # Gradient expressions
  expect_equal(
    .to_typst_color("gradient.linear(red, blue)"),
    "gradient.linear(red, blue)"
  )
  # Tiling patterns
  expect_equal(
    .to_typst_color("pattern(size: (10pt, 10pt))[#rect()]"),
    "pattern(size: (10pt, 10pt))[#rect()]"
  )
  # Variable names
  expect_equal(.to_typst_color("my-color"), "my-color")
  # Method calls
  expect_equal(.to_typst_color("red.lighten(50%)"), "red.lighten(50%)")
})

test_that(".to_typst_stroke converts correctly", {
  expect_equal(.to_typst_stroke(TRUE), "1pt + black")
  expect_equal(.to_typst_stroke("red"), "1pt + red")
  expect_equal(.to_typst_stroke("2pt + blue"), "2pt + blue")
  expect_null(.to_typst_stroke(NULL))
  expect_null(.to_typst_stroke(FALSE))
})

test_that(".to_typst_stroke passes through complex Typst expressions", {
  # Dictionary stroke specs
  expect_equal(
    .to_typst_stroke("(bottom: 0.5pt)"),
    "(bottom: 0.5pt)"
  )
  # Function calls
  expect_equal(
    .to_typst_stroke("stroke(paint: red, thickness: 2pt)"),
    "stroke(paint: red, thickness: 2pt)"
  )
  # Partial sides
  expect_equal(
    .to_typst_stroke("(bottom: 1pt, rest: none)"),
    "(bottom: 1pt, rest: none)"
  )
})

# --- .to_typst_angle tests ---

test_that(".to_typst_angle handles numeric input", {
  expect_equal(.to_typst_angle(90), "90deg")
  expect_equal(.to_typst_angle(0), "0deg")
  expect_equal(.to_typst_angle(-45), "-45deg")
  expect_equal(.to_typst_angle(180.5), "180.5deg")
})

test_that(".to_typst_angle handles string input with units", {
  expect_equal(.to_typst_angle("90deg"), "90deg")
  expect_equal(.to_typst_angle("1.57rad"), "1.57rad")
  expect_equal(.to_typst_angle("100grad"), "100grad")
  expect_equal(.to_typst_angle("0.5turn"), "0.5turn")
  expect_equal(.to_typst_angle("-45deg"), "-45deg")
})

test_that(".to_typst_angle handles NA", {
  expect_equal(.to_typst_angle(NA), NA_character_)
})

test_that(".to_typst_angle handles vector input", {
  result <- .to_typst_angle(c(90, 45, 0))
  expect_equal(result, c("90deg", "45deg", "0deg"))

  result2 <- .to_typst_angle(c("90deg", "1rad"))
  expect_equal(result2, c("90deg", "1rad"))
})

test_that(".to_typst_angle handles NULL", {
  expect_null(.to_typst_angle(NULL))
})

test_that(".to_typst_angle warns for invalid input", {
  expect_warning(result <- .to_typst_angle("invalid"), "Invalid angle")
  expect_equal(result, "0deg")

  expect_warning(result2 <- .to_typst_angle("90px"), "Invalid angle")
  expect_equal(result2, "0deg")
})

# --- .to_typst_color edge case tests ---

test_that(".to_typst_color handles NA", {
  expect_equal(.to_typst_color(NA), NA_character_)
})

test_that(".to_typst_color handles NULL", {
  expect_null(.to_typst_color(NULL))
})

test_that(".to_typst_color handles vectors", {
  result <- .to_typst_color(c("red", "blue", "#ff0000"))
  expect_equal(result, c("red", "blue", 'rgb("#ff0000")'))
})

# --- .to_typst_length edge case tests ---

test_that(".to_typst_length handles NA", {
  expect_equal(.to_typst_length(NA), NA_character_)
})

test_that(".to_typst_length handles NULL", {
  expect_null(.to_typst_length(NULL))
})

test_that(".to_typst_length warns for invalid input", {
  expect_warning(result <- .to_typst_length("invalid"), "Invalid length")
  expect_equal(result, "auto")
})

test_that(".to_typst_length handles vectors", {
  result <- .to_typst_length(c(10, 20, 30))
  expect_equal(result, c("10pt", "20pt", "30pt"))

  result2 <- .to_typst_length(c("5em", "10%", "2cm"))
  expect_equal(result2, c("5em", "10%", "2cm"))
})

# --- .to_typst_align edge case tests ---

test_that(".to_typst_align handles NA", {
  expect_equal(.to_typst_align(NA), NA_character_)
})

test_that(".to_typst_align handles NULL", {
  expect_null(.to_typst_align(NULL))
})

test_that(".to_typst_align handles combined alignments", {
  expect_equal(.to_typst_align("center + horizon"), "center + horizon")
  expect_equal(.to_typst_align("left + top"), "left + top")
  expect_equal(.to_typst_align("right + bottom"), "right + bottom")
})

test_that(".to_typst_align warns for invalid input", {
  expect_warning(result <- .to_typst_align("invalid"), "Invalid alignment")
  expect_equal(result, "left")
})

test_that(".to_typst_align handles vectors", {
  result <- .to_typst_align(c("l", "c", "r"))
  expect_equal(result, c("left", "center", "right"))
})
