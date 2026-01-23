#' Create a Typst table
#'
#' Creates a `typst_table` object from a data.frame or tibble that can be
#' rendered to Typst markup. Use pipe-friendly styling functions like
#' `tt_style()`, `tt_column()`, `tt_row()`, and `tt_cell()` to customize
#' the table appearance.
#'
#' @param data A data.frame or tibble to convert to a table.
#' @param cols <[`tidy-select`][tidyselect::language]> Columns to include in the
#'   displayed table. Hidden columns remain available for data-driven formatting.
#'   Defaults to all columns.
#' @param col_names Optional character vector of display names for columns.
#'   Must match the number of selected columns. If NULL, uses column names from data.
#' @param col_widths Column width specification: "auto" (default) creates equal-width
#'   columns that fill the container. Use `tt_widths()` for custom proportions.
#' @param align Column alignment: single value applied to all columns, or vector
#'   of alignments. Valid values: "left"/"l", "center"/"c", "right"/"r".
#' @param caption Optional table caption (displayed above table).
#' @param label Optional label for cross-referencing (e.g., "tbl-results").
#' @param escape Logical. If TRUE (default), escapes Typst special characters.
#' @param rownames Controls row name handling: TRUE (default) includes row names
#'   as the first column with an empty header, FALSE excludes row names, or a
#'   string to use as the column header for row names.
#'
#' @return A `typst_table` object that can be further styled and rendered.
#'
#' @examples
#' # Basic table (includes row names by default)
#' tt(mtcars[1:5, 1:3])
#'
#' # Select specific columns (excludes row names)
#' tt(mtcars, cols = c(mpg, cyl, hp), rownames = FALSE)
#'
#' # Custom column names
#' tt(mtcars[1:5, 1:3], col_names = c("Miles/Gallon", "Cylinders", "Horsepower"), rownames = FALSE)
#'
#' # Right-align numeric columns
#' tt(mtcars[1:5, 1:3], align = "right")
#'
#' @export
tt <- function(data,
               cols = tidyselect::everything(),
               col_names = NULL,
               col_widths = "auto",
               align = NULL,
               caption = NULL,
               label = NULL,
               escape = TRUE,
               rownames = TRUE) {
  # Validate input
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data.frame or tibble")
  }

  # Handle rownames parameter
  rownames_display_name <- NULL
  if (!isFALSE(rownames)) {
    # Determine display name for rownames column
    rownames_display_name <- if (isTRUE(rownames)) "" else as.character(rownames)

    # Get rownames and prepend as first column
    rn <- rownames(data)
    if (is.null(rn)) rn <- as.character(seq_len(nrow(data)))

    # Use internal placeholder name (R doesn't allow empty column names)
    data <- cbind(
      stats::setNames(data.frame(rn, stringsAsFactors = FALSE), ".rownames"),
      data
    )
  }

  # Store original data for data-driven formatting
  original_data <- data

  # Select columns to display using tidy-select
  cols_quo <- rlang::enquo(cols)
  selected_idx <- tidyselect::eval_select(cols_quo, data = data)
  display_cols <- names(selected_idx)

  # Subset data to display columns (keep order from selection)
  display_data <- data[, display_cols, drop = FALSE]

  # Set column names
  if (is.null(col_names)) {
    col_names <- display_cols
    # Replace .rownames placeholder with actual display name
    if (!is.null(rownames_display_name)) {
      col_names[col_names == ".rownames"] <- rownames_display_name
    }
  } else {
    if (length(col_names) != length(display_cols)) {
      rlang::abort(paste0(
        "`col_names` must have ", length(display_cols), " elements, not ", length(col_names)
      ))
    }
  }


  # Process column widths
  # Default "auto" now means equal fractional widths (full width table)
  if (length(col_widths) == 1 && col_widths == "auto") {
    col_widths <- rep("1fr", length(display_cols))
  } else if (length(col_widths) != length(display_cols)) {
    rlang::abort(paste0(
      "`col_widths` must be 'auto' or have ", length(display_cols), " elements"
    ))
  }

  # Process alignment
  if (is.null(align)) {
    align <- rep("left", length(display_cols))
  } else if (length(align) == 1) {
    align <- rep(align, length(display_cols))
  } else if (length(align) != length(display_cols)) {
    rlang::abort(paste0(
      "`align` must be a single value or have ", length(display_cols), " elements"
    ))
  }
  align <- .to_typst_align(align)

  # Create typst_table object
  table <- structure(
    list(
      # Data
      original_data = original_data,
      display_data = display_data,
      display_cols = display_cols,
      col_names = col_names,
      nrow = nrow(display_data),
      ncol = length(display_cols),

      # Global settings
      escape = escape,
      caption = caption,
      label = label,

      # Column configuration
      col_widths = col_widths,
      col_align = align,

      # Table styling (set by tt_style)
      stroke = NULL,
      fill = NULL,
      striped = NULL,
      inset = NULL,
      row_gutter = NULL,
      column_gutter = NULL,
      position = NULL,
      full_width = FALSE,
      header_separate = NULL,

      # Style overrides (populated by tt_column, tt_row, tt_cell)
      col_styles = list(),
      row_styles = list(),
      cell_styles = list(),

      # Additional features
      headers_above = list(),
      row_groups = list(),
      footnotes = list(),
      hlines = list(),
      vlines = list()
    ),
    class = "typst_table"
  )

  table
}

#' Set proportional column widths
#'
#' Sets column widths as proportions that fill the page or container width.
#' Widths are converted to Typst `fr` (fractional) units.
#'
#' @param table A `typst_table` object.
#' @param ... Width values as numbers. Can be unnamed (applied in order) or
#'   named by column. Values represent relative proportions.
#'
#' @return The modified `typst_table` object.
#'
#' @details
#' Widths are relative proportions, not absolute values. For example,
#' `tt_widths(tbl, 1, 2, 1)` creates columns at 25%, 50%, 25% of the container width.
#'
#' @examples
#' # Equal widths
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |> tt_widths(1, 1, 1)
#'
#' # Proportional widths (25%, 50%, 25%)
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |> tt_widths(1, 2, 1)
#'
#' # Named columns
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |> tt_widths(mpg = 1, cyl = 2, disp = 1)
#'
#' @export
tt_widths <- function(table, ...) {
  .check_typst_table(table)
  table <- .copy_table(table)

  widths <- list(...)

  if (length(widths) == 0) {
    rlang::abort("At least one width must be provided")
  }

  # Check if named or positional
  width_names <- names(widths)

  if (is.null(width_names) || all(width_names == "")) {
    # Positional: apply in order
    if (length(widths) != table$ncol) {
      rlang::abort(paste0(
        "Expected ", table$ncol, " widths, got ", length(widths)
      ))
    }
    width_values <- unlist(widths)
  } else {
    # Named: match to column names
    width_values <- rep(1, table$ncol)  # Default to equal widths
    names(width_values) <- table$display_cols

    for (i in seq_along(widths)) {
      name <- width_names[i]
      if (name == "") {
        rlang::abort("Cannot mix named and unnamed widths")
      }
      if (!name %in% table$display_cols) {
        rlang::abort(paste0("Column '", name, "' not found in table"))
      }
      width_values[name] <- widths[[i]]
    }
    width_values <- unname(width_values)
  }

  # Validate all widths are numeric and positive
  if (!all(is.numeric(width_values)) || any(width_values <= 0)) {
    rlang::abort("All widths must be positive numbers")
  }

  # Convert to fr units
  table$col_widths <- paste0(width_values, "fr")

  table
}
