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
#' @param col_widths Column width specification. A single value is recycled to all
#'   columns. Defaults to "auto" which sizes columns to fit content. Use `tt_widths()`
#'   for proportional widths that fill the container.
#' @param align Column alignment: single value applied to all columns, or vector
#'   of alignments. Valid values: "left"/"l", "center"/"c", "right"/"r".
#' @param preamble Optional character string of raw Typst code to insert before the
#'   table. Useful for `#set` rules, `#let` bindings, or other Typst directives that
#'   should apply to the table (e.g., `'#set text(font: "Arial")'`).
#' @param epilogue Optional character string of raw Typst code to insert after the
#'   table. Useful for notes, captions, or other content that should follow the table.
#' @param escape Logical. If TRUE (default), escapes Typst special characters.
#' @param rownames Logical. TRUE includes row names as the first column
#'   with an empty header, FALSE (default) excludes them.
#' @param na_string Character. How NA values are displayed in the table (default "-").
#' @param booktabs Logical. If TRUE (default), renders with booktabs-style rules
#'   (no cell borders, three horizontal rules: top, mid, bottom). If FALSE,
#'   renders a grid-style table with `0.5pt + black` borders on all cells.
#' @param repeat_header Logical. If TRUE (default), wraps header rows in
#'   `table.header()` so they repeat on each page in multi-page tables.
#'
#' @return A `typst_table` object that can be further styled and rendered.
#'
#' @examples
#' # Basic table
#' tt(mtcars[1:5, 1:3])
#'
#' # Select specific columns
#' tt(mtcars, cols = c(mpg, cyl, hp))
#'
#' # Custom column names
#' tt(mtcars[1:5, 1:3], col_names = c("Miles/Gallon", "Cylinders", "Horsepower"))
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
               preamble = NULL,
               epilogue = NULL,
               escape = TRUE,
               rownames = FALSE,
               na_string = '-',
               booktabs = TRUE,
               repeat_header = TRUE) {
  # Validate input
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data.frame or tibble")
  }

  # Handle rownames parameter
  rownames_display_name <- NULL
  if (isTRUE(rownames)) {
    rownames_display_name <- ""

    # Get rownames and prepend as first column
    rn <- rownames(data)

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
  if(rownames && !(".rownames" %in% display_cols)) {
    display_cols <- c(".rownames", display_cols)
  }

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
  # Single value is recycled to all columns
  if (length(col_widths) == 1) {
    col_widths <- rep(col_widths, length(display_cols))
  } else if (length(col_widths) != length(display_cols)) {
    rlang::abort(paste0(
      "`col_widths` must be a single value or have ", length(display_cols), " elements"
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
      preamble = preamble,
      epilogue = epilogue,
      escape = escape,
      na_string = na_string,

      # Column configuration
      col_widths = col_widths,
      col_align = align,

      # Booktabs mode
      booktabs = booktabs,

      # Table styling (set by tt_style)
      stroke = if (isTRUE(booktabs)) NULL else "0.5pt + black",
      fill = NULL,
      striped = NULL,
      inset = NULL,
      row_gutter = NULL,
      column_gutter = NULL,
      position = NULL,
      # Style overrides (populated by tt_column, tt_row, tt_cell)
      col_styles = list(),
      row_styles = list(),
      cell_styles = list(),
      style_seq = 0L,

      # Additional features
      headers_above = list(),
      row_groups = list(),
      hlines = if (isTRUE(booktabs)) list(
        list(y = 0L,                      start = NULL, end = NULL, stroke = "1pt"),
        list(y = 1L,                      start = NULL, end = NULL, stroke = "0.5pt"),
        list(y = nrow(display_data) + 1L, start = NULL, end = NULL, stroke = "1pt")
      ) else list(),
      vlines = list(),

      # Header repeating
      repeat_header = repeat_header
    ),
    class = "typst_table"
  )

  table
}

#' Set column widths
#'
#' Sets column widths. Numeric values are converted to Typst units via `.unit`.
#' String values are passed through directly to Typst (e.g. `"auto"`, `"1fr"`,
#' `"100pt"`, `"50%"`). Validity of strings is not checked in R — Typst will
#' error on invalid values.
#'
#' @param table A `typst_table` object.
#' @param ... Width values as numbers or strings. Can be unnamed (positional,
#'   applied in column order) or named by column name. Positional mode requires
#'   exactly one value per column.
#' @param .unit Function applied to numeric values to produce a Typst length
#'   string. Default appends `"fr"` (fractional units), so `1` becomes `"1fr"`.
#' @param .default When using named columns, the width to apply to columns not
#'   explicitly mentioned. `NULL` (default) leaves unmentioned columns unchanged.
#'   Any other value (e.g. `"1fr"`, `"auto"`) is applied to all unmentioned columns.
#'
#' @return The modified `typst_table` object.
#'
#' @details
#' Column widths follow last-value-in-pipe-wins logic: calling `tt_widths()` a
#' second time with named columns only updates those columns.
#'
#' Fractional units (`fr`) distribute available space proportionally. For
#' example, `tt_widths(tbl, 1, 2, 1)` creates columns at 25%, 50%, 25% of the
#' container width.
#'
#' @examples
#' # Proportional widths (25%, 50%, 25%)
#' tt(mtcars[1:5, 1:3]) |> tt_widths(1, 2, 1)
#'
#' # String widths passed through directly
#' tt(mtcars[1:5, 1:3]) |> tt_widths("100pt", "auto", "50%")
#'
#' # Update only one column, leave others unchanged
#' tt(mtcars[1:5, 1:3]) |> tt_widths(cyl = 2)
#'
#' # Set all unmentioned columns to auto
#' tt(mtcars[1:5, 1:3]) |> tt_widths(cyl = 2, .default = "auto")
#'
#' # Custom numeric unit
#' tt(mtcars[1:5, 1:3]) |> tt_widths(1, 2, 1, .unit = \(x) paste0(x, "pt"))
#'
#' @export
tt_widths <- function(table, ...,
                      .unit = function(x) paste0(x, "fr"),
                      .default = NULL) {
  .check_typst_table(table)
  table <- .copy_table(table)

  widths <- list(...)

  if (length(widths) == 0) {
    rlang::abort("At least one width must be provided")
  }

  convert_width <- function(w) {
    if (is.numeric(w)) .unit(w) else as.character(w)
  }

  width_names <- names(widths)
  has_names   <- !is.null(width_names) && any(nzchar(width_names))
  has_unnamed <- is.null(width_names)  || any(!nzchar(width_names))

  if (has_names && has_unnamed) {
    rlang::abort("Cannot mix named and unnamed widths")
  }

  if (!has_names) {
    # Positional mode: must supply exactly ncol values
    if (length(widths) != table$ncol) {
      rlang::abort(paste0(
        "Expected ", table$ncol, " widths, got ", length(widths)
      ))
    }
    table$col_widths <- vapply(widths, convert_width, character(1))
  } else {
    # Named mode: only update specified columns
    invalid_cols <- setdiff(width_names, table$display_cols)
    if (length(invalid_cols) > 0) {
      rlang::abort(paste0("Column '", invalid_cols[1], "' not found in table"))
    }

    new_widths <- if (is.null(.default)) {
      table$col_widths
    } else {
      rep(convert_width(.default), table$ncol)
    }

    for (nm in width_names) {
      new_widths[which(table$display_cols == nm)] <- convert_width(widths[[nm]])
    }

    table$col_widths <- new_widths
  }

  table
}
