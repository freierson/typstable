#' Style specific cells
#'
#' Applies formatting to individual cells. Allows for precise control over
#' cell appearance including spanning multiple rows or columns.
#'
#' @param table A `typst_table` object.
#' @param row Row number (1-indexed for data rows, 0 for header).
#' @param column Column specification: integer index or column name.
#' @param bold Logical. Make text bold.
#' @param italic Logical. Make text italic.
#' @param color Text color.
#' @param background Background fill color.
#' @param align Cell alignment.
#' @param colspan Number of columns to span (default 1).
#' @param rowspan Number of rows to span (default 1).
#' @param content Optional content to replace cell value.
#'
#' @return The modified `typst_table` object.
#'
#' @examples
#' # Highlight a specific cell
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_cell(1, 1, background = "yellow", bold = TRUE)
#'
#' # Cell spanning multiple columns
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_cell(1, 1, colspan = 2, content = "Combined")
#'
#' @export
tt_cell <- function(table,
                    row,
                    column,
                    bold = NULL,
                    italic = NULL,
                    color = NULL,
                    background = NULL,
                    align = NULL,
                    colspan = 1,
                    rowspan = 1,
                    content = NULL) {
  .check_typst_table(table)
  table <- .copy_table(table)

  # Validate row
  if (missing(row) || length(row) != 1) {
    rlang::abort("`row` must be a single row number")
  }
  if (row < 0 || row > table$nrow) {
    rlang::abort(paste0(
      "`row` must be between 0 (header) and ", table$nrow
    ))
  }

  # Resolve column
  col_idx <- .resolve_column_index(table, rlang::enquo(column))
  if (is.null(col_idx)) {
    rlang::abort("Column not found in table")
  }

  # Create cell key
  cell_key <- paste0(row, "_", col_idx)

  # Create or update style for this cell
  style <- table$cell_styles[[cell_key]] %||% list()

  if (!is.null(bold)) style$bold <- bold
  if (!is.null(italic)) style$italic <- italic
  if (!is.null(color)) style$color <- color
  if (!is.null(background)) style$background <- background
  if (!is.null(align)) style$cell_align <- .to_typst_align(align)
  if (!is.null(content)) style$content <- content
  if (colspan > 1) style$colspan <- colspan
  if (rowspan > 1) style$rowspan <- rowspan

  table$cell_styles[[cell_key]] <- style

  table
}

#' Resolve column to index
#' @noRd
.resolve_column_index <- function(table, col_quo) {
  col_val <- rlang::eval_tidy(col_quo)

  if (is.numeric(col_val)) {
    if (col_val < 1 || col_val > table$ncol) {
      return(NULL)
    }
    return(as.integer(col_val))
  }

  if (is.character(col_val)) {
    idx <- match(col_val, table$display_cols)
    if (is.na(idx)) {
      return(NULL)
    }
    return(idx)
  }

  # Try as symbol (column name)
  if (rlang::quo_is_symbol(col_quo)) {
    col_name <- rlang::as_name(col_quo)
    idx <- match(col_name, table$display_cols)
    if (is.na(idx)) {
      return(NULL)
    }
    return(idx)
  }

  NULL
}
