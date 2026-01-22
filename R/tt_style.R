#' Style overall table appearance
#'
#' Sets global styling properties for the entire table including stroke (borders),
#' fill (background), striped rows, and spacing.
#'
#' @param table A `typst_table` object.
#' @param stroke Stroke (border) specification: `TRUE` for default 1pt black borders,
#'   a color name/hex for 1pt borders in that color, or a Typst stroke specification
#'   like `"2pt + blue"`.
#' @param fill Background fill color for the entire table.
#' @param striped Logical. If `TRUE`, alternates row background colors for readability.
#' @param inset Cell padding. Can be a single value (e.g., `"5pt"`) or named vector
#'   for different padding on each side.
#' @param row_gutter Vertical spacing between rows.
#' @param column_gutter Horizontal spacing between columns.
#' @param position Table position on page: `"auto"`, `"left"`, `"center"`, `"right"`.
#' @param full_width Logical. If `TRUE`, table spans full page width.
#' @param header_separate Logical. If `TRUE`, adds a horizontal line below the header row.
#'
#' @return The modified `typst_table` object.
#'
#' @examples
#' # Add borders and striped rows
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_style(stroke = TRUE, striped = TRUE)
#'
#' # Custom border color and padding
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_style(stroke = "gray", inset = "8pt")
#'
#' # Header separator line
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_style(header_separate = TRUE)
#'
#' @export
tt_style <- function(table,
                     stroke = NULL,
                     fill = NULL,
                     striped = NULL,
                     inset = NULL,
                     row_gutter = NULL,
                     column_gutter = NULL,
                     position = NULL,
                     full_width = FALSE,
                     header_separate = NULL) {
  .check_typst_table(table)
  table <- .copy_table(table)

  if (!is.null(stroke)) {
    table$stroke <- stroke
  }

  if (!is.null(fill)) {
    table$fill <- fill
  }

  if (!is.null(striped)) {
    table$striped <- striped
  }

  if (!is.null(inset)) {
    table$inset <- inset
  }

  if (!is.null(row_gutter)) {
    table$row_gutter <- row_gutter
  }

  if (!is.null(column_gutter)) {
    table$column_gutter <- column_gutter
  }

  if (!is.null(position)) {
    valid_positions <- c("auto", "left", "center", "right")
    if (!position %in% valid_positions) {
      rlang::warn(paste0(
        "Invalid position '", position, "', must be one of: ", paste(valid_positions, collapse = ", ")
      ))
    } else {
      table$position <- position
    }
  }

  if (!is.null(full_width)) {
    table$full_width <- full_width
  }

  if (!is.null(header_separate)) {
    table$header_separate <- header_separate
  }

  table
}
