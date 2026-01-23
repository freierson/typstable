#' Style specific rows
#'
#' Applies formatting to one or more rows. Use row = 0 to style the header row.
#'
#' @param table A `typst_table` object.
#' @param row Integer vector of row numbers to style. Use 0 for the header row,
#'   1 to n for data rows.
#' @param bold Logical. Make text bold.
#' @param italic Logical. Make text italic.
#' @param color Text color.
#' @param background Background fill color.
#' @param align Row alignment override.
#' @param font_size Font size.
#' @param rotate Rotation angle (e.g., `"90deg"`, `90`, `"1.5rad"`).
#' @param hline_above Add horizontal line above the row. Can be `TRUE` for default
#'   line or a stroke specification.
#' @param hline_below Add horizontal line below the row.
#'
#' @return The modified `typst_table` object.
#'
#' @examples
#' # Style header row
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_row(0, bold = TRUE, background = "gray")
#'
#' # Highlight specific rows
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_row(c(1, 3, 5), background = "#ffffcc")
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
                   background = NULL,
                   align = NULL,
                   font_size = NULL,
                   rotate = NULL,
                   hline_above = NULL,
                   hline_below = NULL) {
  .check_typst_table(table)
  table <- .copy_table(table)

  if (missing(row) || length(row) == 0) {
    rlang::abort("`row` must specify at least one row number")
  }

  # Validate row numbers
  valid_rows <- 0:table$nrow
  invalid <- setdiff(row, valid_rows)
  if (length(invalid) > 0) {
    rlang::warn(paste0(
      "Row(s) ", paste(invalid, collapse = ", "), " out of range (0-", table$nrow, "), ignoring"
    ))
    row <- intersect(row, valid_rows)
  }

  if (length(row) == 0) return(table)

  # Apply style to each row
  for (r in row) {
    row_key <- as.character(r)

    # Create or update style for this row
    style <- table$row_styles[[row_key]] %||% list()

    if (!is.null(bold)) style$bold <- bold
    if (!is.null(italic)) style$italic <- italic
    if (!is.null(color)) style$color <- color
    if (!is.null(background)) style$background <- background
    if (!is.null(align)) style$cell_align <- .to_typst_align(align)
    if (!is.null(font_size)) style$font_size <- font_size
    if (!is.null(rotate)) style$rotate <- rotate

    table$row_styles[[row_key]] <- style

    # Handle horizontal lines
    if (!is.null(hline_above)) {
      # For header (row 0), line goes at position 0
      # For data rows, line goes at row position (accounting for header)
      y_pos <- if (r == 0) 0 else r
      stroke <- if (isTRUE(hline_above)) NULL else hline_above
      table$hlines <- c(table$hlines, list(list(y = y_pos, stroke = stroke)))
    }

    if (!is.null(hline_below)) {
      # Line below row r goes at position r+1
      y_pos <- r + 1
      stroke <- if (isTRUE(hline_below)) NULL else hline_below
      table$hlines <- c(table$hlines, list(list(y = y_pos, stroke = stroke)))
    }
  }

  table
}
