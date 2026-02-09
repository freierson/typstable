#' Add grouped header row above column names
#'
#' Adds a spanning header row above the existing column names to group related
#' columns together.
#'
#' @param table A `typst_table` object.
#' @param header Named vector specifying header groups. Names are the group labels,
#'   values are the number of columns each group spans. Use empty string `""` for
#'   columns without a group header.
#' @param bold Logical. Make header text bold (default TRUE).
#' @param align Header alignment (default "center").
#' @param color Text color.
#' @param fill Fill color.
#' @param italic Logical. Make header text italic.
#' @param font_size Font size.
#' @param rotate Rotation angle (e.g., `"90deg"`, `90`, `"1.5rad"`).
#' @param inset Cell padding (e.g., `"10pt"`, `"5pt 8pt"`).
#' @param stroke Stroke (border) specification for the header cell(s). Can be `TRUE`
#'   for default 1pt black, a color, a Typst stroke spec like `"2pt + blue"`, or a
#'   Typst dictionary like `"(bottom: 1pt)"`.
#' @param line Logical. Add horizontal line below the header (default TRUE).
#' @param gap Width of visual gap between header groups (e.g., "10pt", "0.5em").
#'   When specified, empty columns are inserted between groups. Default "10pt".
#'   Use NULL to disable gaps.
#'
#' @return The modified `typst_table` object.
#'
#' @examples
#' # Group columns under headers
#' tt(mtcars[1:5, 1:6], rownames = FALSE) |>
#'   tt_header_above(c("Performance" = 3, "Design" = 3))
#'
#' # Leave some columns ungrouped
#' tt(mtcars[1:5, 1:6], rownames = FALSE) |>
#'   tt_header_above(c(" " = 1, "Group A" = 2, "Group B" = 3))
#'
#' # Disable gaps between groups
#' tt(mtcars[1:5, 1:6], rownames = FALSE) |>
#'   tt_header_above(c(" " = 1, "Group A" = 2, "Group B" = 3), gap = NULL)
#'
#' @export
tt_header_above <- function(table,
                            header,
                            bold = TRUE,
                            align = "center",
                            color = NULL,
                            fill = NULL,
                            italic = NULL,
                            font_size = NULL,
                            rotate = NULL,
                            inset = NULL,
                            stroke = NULL,
                            line = TRUE,
                            gap = "10pt") {
  .check_typst_table(table)
  table <- .copy_table(table)

  if (missing(header) || length(header) == 0) {
    rlang::abort("`header` must be a named vector specifying column spans")
  }

  # Validate that spans sum to number of columns
  total_span <- sum(header)
  if (total_span != table$ncol) {
    rlang::abort(paste0(
      "Header spans must sum to ", table$ncol, " (number of columns), got ", total_span
    ))
  }

  # Ensure all values are named
  if (is.null(names(header))) {
    rlang::abort("`header` must be a named vector (e.g., c('Group A' = 2, 'Group B' = 3))")
  }

  # Store header specification
  header_spec <- list(
    header = header,
    bold = bold,
    align = align,
    color = color,
    fill = fill,
    italic = italic,
    font_size = font_size,
    rotate = rotate,
    inset = inset,
    stroke = stroke,
    line = line,
    gap = gap
  )

  # Prepend to headers_above (most recent goes first)
  table$headers_above <- c(list(header_spec), table$headers_above)

  table
}
