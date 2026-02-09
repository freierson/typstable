#' Add horizontal line to table
#'
#' Adds a horizontal line at a specific position in the table.
#'
#' @param table A `typst_table` object.
#' @param y Position of the line (0 = top of table, 1 = below header,
#'   2 = below first data row, etc.).
#' @param start Starting column for partial line (0-indexed, inclusive).
#' @param end Ending column for partial line (0-indexed, inclusive).
#' @param stroke Line style: `TRUE` for default, color name, or full stroke spec
#'   like `"2pt + red"`.
#'
#' @return The modified `typst_table` object.
#'
#' @examples
#' # Add line below header
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_hline(1)
#'
#' # Add colored line
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_hline(1, stroke = "blue")
#'
#' # Partial line spanning some columns
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_hline(1, start = 0, end = 2)
#'
#' @export
tt_hline <- function(table,
                     y,
                     start = NULL,
                     end = NULL,
                     stroke = TRUE) {
  .check_typst_table(table)
  table <- .copy_table(table)

  if (missing(y) || !is.numeric(y) || length(y) != 1) {
    rlang::abort("`y` must be a single number specifying line position")
  }

  # Validate position
  max_y <- table$nrow + 1  # After last data row
  if (y < 0 || y > max_y) {
    rlang::warn(paste0(
      "`y` should be between 0 and ", max_y, ", got ", y
    ))
  }

  # Build hline spec
  hline_spec <- list(
    y = as.integer(y),
    start = start,
    end = end,
    stroke = if (isTRUE(stroke)) NULL else stroke
  )

  table$hlines <- c(table$hlines, list(hline_spec))

  table
}

#' Add vertical line to table
#'
#' Adds a vertical line at a specific position in the table.
#'
#' @param table A `typst_table` object.
#' @param x Position of the line (0 = before first column, 1 = after first column, etc.).
#' @param start Starting row for partial line (0-indexed, inclusive, 0 = header).
#' @param end Ending row for partial line (0-indexed, inclusive).
#' @param stroke Line style: `TRUE` for default, color name, or full stroke spec.
#'
#' @return The modified `typst_table` object.
#'
#' @examples
#' # Add line after first column
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_vline(1)
#'
#' # Add partial vertical line (data rows only)
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_vline(1, start = 1)
#'
#' @export
tt_vline <- function(table,
                     x,
                     start = NULL,
                     end = NULL,
                     stroke = TRUE) {
  .check_typst_table(table)
  table <- .copy_table(table)

  if (missing(x) || !is.numeric(x) || length(x) != 1) {
    rlang::abort("`x` must be a single number specifying line position")
  }

  # Validate position
  max_x <- table$ncol  # After last column
  if (x < 0 || x > max_x) {
    rlang::warn(paste0(
      "`x` should be between 0 and ", max_x, ", got ", x
    ))
  }

  # Build vline spec
  vline_spec <- list(
    x = as.integer(x),
    start = start,
    end = end,
    stroke = if (isTRUE(stroke)) NULL else stroke
  )

  table$vlines <- c(table$vlines, list(vline_spec))

  table
}
