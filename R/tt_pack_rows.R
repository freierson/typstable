#' Group rows under a label
#'
#' Inserts a group label row and optionally indents the grouped rows.
#' Useful for creating visual sections in the table.
#'
#' @param table A `typst_table` object.
#' @param group_label The label text for the group (used with `start_row`/`end_row`).
#' @param start_row First row number in the group (1-indexed).
#' @param end_row Last row number in the group.
#' @param index A named numeric vector for multiple groups (alternative style).
#'   Names are group labels, values are row counts.
#'   Example: `c("Group A" = 3, "Group B" = 5)` creates two groups where
#'   "Group A" covers rows 1-3 and "Group B" covers rows 4-8.
#' @param indent Logical. Indent rows in the group (default TRUE).
#' @param bold Logical. Make the group label bold (default TRUE).
#' @param italic Logical. Make the group label italic.
#' @param color Text color for the group label.
#' @param fill Fill color for the group label row.
#' @param align Alignment for the group label row.
#' @param font_size Font size for the group label.
#' @param rotate Rotation angle (e.g., `"90deg"`, `90`, `"1.5rad"`).
#' @param inset Cell padding (e.g., `"10pt"`, `"5pt 8pt"`).
#' @param stroke Stroke (border) specification for the group label cell. Can be `TRUE` for
#'   default 1pt black, a color, a Typst stroke spec like `"2pt + blue"`, or a
#'   Typst dictionary like `"(bottom: 1pt)"`.
#' @param hline_below Add horizontal line below the group label. Can be `TRUE` for default
#'   line or a stroke specification.
#'
#' @return The modified `typst_table` object.
#'
#' @examples
#' # Group rows with a label
#' tt(mtcars[1:10, 1:3]) |>
#'   tt_pack_rows("4 Cylinders", 1, 5) |>
#'   tt_pack_rows("6 Cylinders", 6, 10)
#'
#' # Using index parameter (alternative style)
#' tt(mtcars[1:10, 1:3]) |>
#'   tt_pack_rows(index = c("4 Cylinders" = 5, "6 Cylinders" = 5))
#'
#' # Styled group labels
#' tt(mtcars[1:10, 1:3]) |>
#'   tt_pack_rows("4 Cylinders", 1, 5, fill = "#e6f3ff", italic = TRUE) |>
#'   tt_pack_rows("6 Cylinders", 6, 10, fill = "#e6f3ff", italic = TRUE)
#'
#' @export
tt_pack_rows <- function(table,
                         group_label = NULL,
                         start_row = NULL,
                         end_row = NULL,
                         index = NULL,
                         indent = TRUE,
                         bold = TRUE,
                         italic = NULL,
                         color = NULL,
                         fill = NULL,
                         align = NULL,
                         font_size = NULL,
                         rotate = NULL,
                         inset = NULL,
                         stroke = NULL,
                         hline_below = NULL) {
  .check_typst_table(table)
  table <- .copy_table(table)

  # Handle index parameter (alternative style)
  if (!is.null(index)) {
    if (!is.numeric(index) || is.null(names(index))) {
      rlang::abort("`index` must be a named numeric vector")
    }

    # Convert index to row groups
    end_rows <- cumsum(index)
    start_rows <- c(1, end_rows[-length(end_rows)] + 1)

    # Validate total rows
    if (end_rows[length(end_rows)] > table$nrow) {
      rlang::abort(paste0(
        "`index` specifies ", end_rows[length(end_rows)],
        " rows but table only has ", table$nrow, " rows"
      ))
    }

    for (i in seq_along(index)) {
      group_spec <- list(
        group_label = names(index)[i],
        start_row = as.integer(start_rows[i]),
        end_row = as.integer(end_rows[i]),
        indent = indent,
        bold = bold,
        italic = italic,
        color = color,
        fill = fill,
        align = align,
        font_size = font_size,
        rotate = rotate,
        inset = inset,
        stroke = stroke,
        hline_below = hline_below
      )
      table$row_groups <- c(table$row_groups, list(group_spec))
    }

    return(table)
  }

  # Original single-group logic
  if (is.null(group_label) || !is.character(group_label) || length(group_label) != 1) {
    rlang::abort("`group_label` must be a single character string")
  }

  if (is.null(start_row) || !is.numeric(start_row) || length(start_row) != 1) {
    rlang::abort("`start_row` must be a single number")
  }

  if (is.null(end_row) || !is.numeric(end_row) || length(end_row) != 1) {
    rlang::abort("`end_row` must be a single number")
  }

  # Validate row range
  if (start_row < 1 || start_row > table$nrow) {
    rlang::abort(paste0(
      "`start_row` must be between 1 and ", table$nrow
    ))
  }

  if (end_row < start_row || end_row > table$nrow) {
    rlang::abort(paste0(
      "`end_row` must be between ", start_row, " and ", table$nrow
    ))
  }

  # Store row group specification
  group_spec <- list(
    group_label = group_label,
    start_row = as.integer(start_row),
    end_row = as.integer(end_row),
    indent = indent,
    bold = bold,
    italic = italic,
    color = color,
    fill = fill,
    align = align,
    font_size = font_size,
    rotate = rotate,
    inset = inset,
    stroke = stroke,
    hline_below = hline_below
  )

  table$row_groups <- c(table$row_groups, list(group_spec))

  table
}
