#' Style specific cells
#'
#' Applies formatting to individual cells. Allows for precise control over
#' cell appearance including spanning multiple rows or columns.
#'
#' @param table A `typst_table` object.
#' @param row Row number (1-indexed for data rows, 0 for header). Use negative
#'   indices to target `tt_header_above()` rows: -1 is the innermost header_above
#'   (closest to the main header), -2 is the next row up, etc. The column is
#'   normalized to the start of the header group it falls within.
#' @param column Column specification: integer index or column name.
#' @param bold Logical. Make text bold.
#' @param italic Logical. Make text italic.
#' @param color Text color.
#' @param fill Fill color.
#' @param align Cell alignment.
#' @param font_size Font size.
#' @param rotate Rotation angle (e.g., `"90deg"`, `90`, `"1.5rad"`).
#' @param inset Cell padding (e.g., `"10pt"`, `"5pt 8pt"`).
#' @param stroke Stroke (border) specification for the cell. Can be `TRUE` for
#'   default 1pt black, a color, a Typst stroke spec like `"2pt + blue"`, or a
#'   Typst dictionary like `"(bottom: 1pt)"`.
#' @param colspan Number of columns to span (default 1).
#' @param rowspan Number of rows to span (default 1).
#' @param content Optional content to replace cell value.
#'
#' @return The modified `typst_table` object.
#'
#' @examples
#' # Highlight a specific cell
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_cell(1, 1, fill = "yellow", bold = TRUE)
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
                    fill = NULL,
                    align = NULL,
                    font_size = NULL,
                    rotate = NULL,
                    inset = NULL,
                    stroke = NULL,
                    colspan = 1,
                    rowspan = 1,
                    content = NULL) {
  .check_typst_table(table)
  table <- .copy_table(table)
  table$style_seq <- table$style_seq + 1L
  seq <- table$style_seq

  # Validate row
  if (missing(row) || length(row) != 1) {
    rlang::abort("`row` must be a single row number")
  }
  n_above <- length(table$headers_above)
  valid_min <- if (n_above > 0) -n_above else 0L
  if (row < valid_min || row > table$nrow) {
    rlang::abort(paste0(
      "`row` must be between ", valid_min, " and ", table$nrow
    ))
  }

  # Resolve column
  col_idx <- .resolve_column_index(table, rlang::enquo(column))
  if (is.null(col_idx)) {
    rlang::abort("Column not found in table")
  }

  # For negative rows (header_above), normalize column to group start
  if (row < 0) {
    header_spec_idx <- n_above - abs(row) + 1L
    header_spec <- table$headers_above[[header_spec_idx]]
    group_info <- .resolve_header_group(header_spec, col_idx)
    col_idx <- group_info$start_col

    # colspan/rowspan not supported for header_above cells
    if (colspan > 1 || rowspan > 1) {
      rlang::warn("colspan/rowspan not supported for header_above cells, ignoring")
      colspan <- 1
      rowspan <- 1
    }
  }

  # Create cell key
  cell_key <- paste0(row, "_", col_idx)

  # Create or update style for this cell
  style <- table$cell_styles[[cell_key]] %||% list()

  if (!is.null(bold)) style <- .set_style_attr(style, "bold", bold, seq)
  if (!is.null(italic)) style <- .set_style_attr(style, "italic", italic, seq)
  if (!is.null(color)) style <- .set_style_attr(style, "color", color, seq)
  if (!is.null(fill)) style <- .set_style_attr(style, "fill", fill, seq)
  if (!is.null(align)) style <- .set_style_attr(style, "cell_align", .to_typst_align(align), seq)
  if (!is.null(font_size)) style <- .set_style_attr(style, "font_size", font_size, seq)
  if (!is.null(rotate)) style <- .set_style_attr(style, "rotate", rotate, seq)
  if (!is.null(inset)) style <- .set_style_attr(style, "inset", inset, seq)
  if (!is.null(stroke)) style <- .set_style_attr(style, "stroke", stroke, seq)
  if (!is.null(content)) style <- .set_style_attr(style, "content", content, seq)
  if (colspan > 1) style <- .set_style_attr(style, "colspan", colspan, seq)
  if (rowspan > 1) style <- .set_style_attr(style, "rowspan", rowspan, seq)

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
