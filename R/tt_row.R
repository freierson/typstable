#' Style specific rows
#'
#' Applies formatting to one or more rows. Use row = 0 to style the header row.
#'
#' @param table A `typst_table` object.
#' @param row Integer vector of row numbers to style. Use 0 for the header row,
#'   1 to n for data rows. Use negative indices to target `tt_header_above()` rows:
#'   -1 is the innermost header_above (closest to the main header), -2 is the next
#'   row up, etc.
#' @param bold Logical. Make text bold.
#' @param italic Logical. Make text italic.
#' @param color Text color.
#' @param fill Fill color.
#' @param align Row alignment override.
#' @param font_size Font size.
#' @param rotate Rotation angle (e.g., `"90deg"`, `90`, `"1.5rad"`).
#' @param inset Cell padding (e.g., `"10pt"`, `"5pt 8pt"`).
#' @param stroke Stroke (border) specification for the cell(s). Can be `TRUE` for
#'   default 1pt black, a color, a Typst stroke spec like `"2pt + blue"`, or a
#'   Typst dictionary like `"(bottom: 1pt)"`.
#' @param hline_above Add horizontal line above the row. Can be `TRUE` for default
#'   line or a stroke specification.
#' @param hline_below Add horizontal line below the row.
#'
#' @return The modified `typst_table` object.
#'
#' @examples
#' # Style header row
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_row(0, bold = TRUE, fill = "gray")
#'
#' # Highlight specific rows
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_row(c(1, 3, 5), fill = "#ffffcc")
#'
#' # Add horizontal lines
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_row(3, hline_above = TRUE, hline_below = TRUE)
#'
#' @export
tt_row <- function(table,
                   row,
                   bold = NULL,
                   italic = NULL,
                   color = NULL,
                   fill = NULL,
                   align = NULL,
                   font_size = NULL,
                   rotate = NULL,
                   inset = NULL,
                   stroke = NULL,
                   hline_above = NULL,
                   hline_below = NULL) {
  .check_typst_table(table)
  table <- .copy_table(table)

  if (missing(row) || length(row) == 0) {
    rlang::abort("`row` must specify at least one row number")
  }

  # Validate row numbers
  n_above <- length(table$headers_above)
  valid_min <- if (n_above > 0) -n_above else 0L
  valid_rows <- valid_min:table$nrow
  invalid <- setdiff(row, valid_rows)
  if (length(invalid) > 0) {
    rlang::warn(paste0(
      "Row(s) ", paste(invalid, collapse = ", "), " out of range (",
      valid_min, " to ", table$nrow, "), ignoring"
    ))
    row <- intersect(row, valid_rows)
  }

  if (length(row) == 0) return(table)

  # Increment sequence counter for last-write-wins ordering
  table$style_seq <- table$style_seq + 1L
  seq <- table$style_seq

  # Apply style to each row
  for (r in row) {
    row_key <- as.character(r)

    # Create or update style for this row
    style <- table$row_styles[[row_key]] %||% list()

    if (!is.null(bold)) style <- .set_style_attr(style, "bold", bold, seq)
    if (!is.null(italic)) style <- .set_style_attr(style, "italic", italic, seq)
    if (!is.null(color)) style <- .set_style_attr(style, "color", color, seq)
    if (!is.null(fill)) style <- .set_style_attr(style, "fill", fill, seq)
    if (!is.null(align)) style <- .set_style_attr(style, "cell_align", .to_typst_align(align), seq)
    if (!is.null(font_size)) style <- .set_style_attr(style, "font_size", font_size, seq)
    if (!is.null(rotate)) style <- .set_style_attr(style, "rotate", rotate, seq)
    if (!is.null(inset)) style <- .set_style_attr(style, "inset", inset, seq)
    if (!is.null(stroke)) style <- .set_style_attr(style, "stroke", stroke, seq)

    table$row_styles[[row_key]] <- style

    # Handle horizontal lines (not supported for header_above rows)
    if (r < 0 && (!is.null(hline_above) || !is.null(hline_below))) {
      rlang::warn("hline_above/hline_below not supported for header_above rows, ignoring")
      next
    }
    if (!is.null(hline_above)) {
      # For header (row 0), line goes at position 0
      # For data rows, line goes at row position (accounting for header)
      y_pos <- if (r == 0) 0 else r
      hline_stroke <- if (isTRUE(hline_above)) NULL else hline_above
      table$hlines <- c(table$hlines, list(list(y = y_pos, stroke = hline_stroke)))
    }

    if (!is.null(hline_below)) {
      # Line below row r goes at position r+1
      y_pos <- r + 1
      hline_stroke <- if (isTRUE(hline_below)) NULL else hline_below
      table$hlines <- c(table$hlines, list(list(y = y_pos, stroke = hline_stroke)))
    }
  }

  table
}
