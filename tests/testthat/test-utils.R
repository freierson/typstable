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

test_that(".escape_typst handles NA and empty strings", {
  expect_equal(.escape_typst(NA_character_), "")
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
