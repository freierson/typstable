#' Style specific columns
#'
#' Applies formatting to one or more columns. Supports both static values and
#' data-driven formatting where styling parameters reference other columns.
#'
#' @param table A `typst_table` object.
#' @param column <[`tidy-select`][tidyselect::language]> Column(s) to style.
#' @param width Column width (e.g., `"100pt"`, `"2fr"`). Overrides width set in `tt()`.
#' @param align Column alignment: `"left"`, `"center"`, `"right"`.
#' @param bold Logical or column name. If logical, applies to all rows. If column name
#'   (unquoted), uses that column's values (coerced to logical) for per-row bold.
#' @param italic Logical or column name. Same behavior as `bold`.
#' @param color Text color. Can be a color value or column name for per-row colors.
#' @param background Background fill color. Can be a color value or column name.
#' @param border_left Left border specification (e.g., `TRUE`, `"1pt + gray"`).
#' @param border_right Right border specification.
#' @param font_size Font size (e.g., `"10pt"`, `"0.9em"`). Can be a column name.
#' @param rotate Rotation angle (e.g., `"90deg"`, `90`, `"1.5rad"`). Can be a column name.
#'
#' @return The modified `typst_table` object.
#'
#' @details
#' ## Data-driven formatting
#'
#' Style parameters can reference columns in the original data for per-row values:
#'
#' ```r
#' data |>
#'   mutate(bg_color = ifelse(value > 100, "red", "white")) |>
#'   tt(cols = c(name, value)) |>
#'   tt_column(value, background = bg_color)
#' ```
#'
#' The `bg_color` column is hidden from display but used for styling.
#'
#' @examples
#' # Right-align and bold a numeric column
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_column(mpg, align = "right", bold = TRUE)
#'
#' # Style multiple columns at once
#' tt(mtcars[1:5, 1:3], rownames = FALSE) |>
#'   tt_column(c(mpg, disp), align = "right", color = "blue")
#'
#' @export
tt_column <- function(table,
                      column,
                      width = NULL,
                      align = NULL,
                      bold = NULL,
                      italic = NULL,
                      color = NULL,
                      background = NULL,
                      border_left = NULL,
                      border_right = NULL,
                      font_size = NULL,
                      rotate = NULL) {
  .check_typst_table(table)
  table <- .copy_table(table)

  # Evaluate column selection using tidy-select
  col_quo <- rlang::enquo(column)
  selected_idx <- tidyselect::eval_select(col_quo, data = table$display_data)
  selected_cols <- names(selected_idx)

  if (length(selected_cols) == 0) {
    rlang::warn("No columns matched the selection")
    return(table)
  }

  # Capture styling arguments as quosures for data-driven detection
  bold_quo <- rlang::enquo(bold)
  italic_quo <- rlang::enquo(italic)
  color_quo <- rlang::enquo(color)
  background_quo <- rlang::enquo(background)
  font_size_quo <- rlang::enquo(font_size)
  rotate_quo <- rlang::enquo(rotate)

  # Apply style to each selected column
  for (col_name in selected_cols) {
    col_idx <- which(table$display_cols == col_name)

    # Create or update style for this column
    style <- table$col_styles[[col_name]] %||% list()

    # Handle width
    if (!is.null(width)) {
      table$col_widths[col_idx] <- .to_typst_length(width, allow_fr = TRUE)
    }

    # Handle alignment
    if (!is.null(align)) {
      table$col_align[col_idx] <- .to_typst_align(align)
    }

    # Handle bold (static or data-driven)
    if (!rlang::quo_is_null(bold_quo)) {
      if (.is_column_ref(bold_quo)) {
        ref_col <- .get_column_name(bold_quo)
        if (ref_col %in% names(table$original_data)) {
          style$bold_col <- ref_col
        } else {
          rlang::warn(paste0("Column '", ref_col, "' not found for bold"))
        }
      } else {
        style$bold <- rlang::eval_tidy(bold_quo)
      }
    }

    # Handle italic (static or data-driven)
    if (!rlang::quo_is_null(italic_quo)) {
      if (.is_column_ref(italic_quo)) {
        ref_col <- .get_column_name(italic_quo)
        if (ref_col %in% names(table$original_data)) {
          style$italic_col <- ref_col
        } else {
          rlang::warn(paste0("Column '", ref_col, "' not found for italic"))
        }
      } else {
        style$italic <- rlang::eval_tidy(italic_quo)
      }
    }

    # Handle color (static or data-driven)
    if (!rlang::quo_is_null(color_quo)) {
      if (.is_column_ref(color_quo)) {
        ref_col <- .get_column_name(color_quo)
        if (ref_col %in% names(table$original_data)) {
          style$color_col <- ref_col
        } else {
          rlang::warn(paste0("Column '", ref_col, "' not found for color"))
        }
      } else {
        style$color <- rlang::eval_tidy(color_quo)
      }
    }

    # Handle background (static or data-driven)
    if (!rlang::quo_is_null(background_quo)) {
      if (.is_column_ref(background_quo)) {
        ref_col <- .get_column_name(background_quo)
        if (ref_col %in% names(table$original_data)) {
          style$background_col <- ref_col
        } else {
          rlang::warn(paste0("Column '", ref_col, "' not found for background"))
        }
      } else {
        style$background <- rlang::eval_tidy(background_quo)
      }
    }

    # Handle font_size (static or data-driven)
    if (!rlang::quo_is_null(font_size_quo)) {
      if (.is_column_ref(font_size_quo)) {
        ref_col <- .get_column_name(font_size_quo)
        if (ref_col %in% names(table$original_data)) {
          style$font_size_col <- ref_col
        } else {
          rlang::warn(paste0("Column '", ref_col, "' not found for font_size"))
        }
      } else {
        style$font_size <- rlang::eval_tidy(font_size_quo)
      }
    }

    # Handle rotate (static or data-driven)
    if (!rlang::quo_is_null(rotate_quo)) {
      if (.is_column_ref(rotate_quo)) {
        ref_col <- .get_column_name(rotate_quo)
        if (ref_col %in% names(table$original_data)) {
          style$rotate_col <- ref_col
        } else {
          rlang::warn(paste0("Column '", ref_col, "' not found for rotate"))
        }
      } else {
        style$rotate <- rlang::eval_tidy(rotate_quo)
      }
    }

    # Handle borders
    if (!is.null(border_left)) {
      style$border_left <- border_left
      # Add vline before this column
      table$vlines <- c(table$vlines, list(list(x = col_idx, stroke = border_left)))
    }

    if (!is.null(border_right)) {
      style$border_right <- border_right
      # Add vline after this column
      table$vlines <- c(table$vlines, list(list(x = col_idx + 1, stroke = border_right)))
    }

    table$col_styles[[col_name]] <- style
  }

  table
}
