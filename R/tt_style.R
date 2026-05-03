#' Style overall table appearance
#'
#' Sets global styling properties for the entire table including stroke (borders),
#' fill, striped rows, and spacing.
#'
#' @param table A `typst_table` object.
#' @param stroke Stroke (border) specification: `TRUE` for default 1pt black borders,
#'   a color name/hex for 1pt borders in that color, or a Typst stroke specification
#'   like `"2pt + blue"`. When stroke is set, separator lines from
#'   `tt_header_above()` are automatically suppressed.
#' @param fill Fill color for the entire table.
#' @param striped Logical. If `TRUE`, alternates row background colors for readability.
#' @param inset Cell padding. Can be a single value (e.g., `"5pt"`) or named vector
#'   for different padding on each side.
#' @param row_gutter Vertical spacing between rows.
#' @param column_gutter Horizontal spacing between columns.
#' @param position Table position on page: `"auto"`, `"left"`, `"center"`, `"right"`.
#' @return The modified `typst_table` object.
#'
#' @examples
#' # Add borders and striped rows
#' tt(mtcars[1:5, 1:3]) |>
#'   tt_style(stroke = TRUE, striped = TRUE)
#'
#' # Custom border color and padding
#' tt(mtcars[1:5, 1:3]) |>
#'   tt_style(stroke = "gray", inset = "8pt")
#'
#' @export
tt_style <- function(table,
                     stroke = NULL,
                     fill = NULL,
                     striped = NULL,
                     inset = NULL,
                     row_gutter = NULL,
                     column_gutter = NULL,
                     position = NULL) {
  .check_typst_table(table)
  table <- .copy_table(table)

  if (!is.null(stroke)) {
    table$stroke <- stroke
    if (isTRUE(table$booktabs)) {
      table$hlines <- c(table$hlines, list(
        list(y = 0L,              start = NULL, end = NULL, stroke = FALSE),
        list(y = 1L,              start = NULL, end = NULL, stroke = FALSE),
        list(y = table$nrow + 1L, start = NULL, end = NULL, stroke = FALSE)
      ))
    }
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

  table
}
