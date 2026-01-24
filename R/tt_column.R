#' Style specific columns
#'
#' Applies formatting to one or more columns. Supports both static values and
#' data-driven formatting where styling parameters reference other columns.
#'
#' @param table A `typst_table` object.
#' @param column <[`tidy-select`][tidyselect::language]> Column(s) to style.
#' @param width Column width (e.g., `"100pt"`, `"2fr"`). Overrides width set in `tt()`.
#' @param align Column alignment: `"left"`, `"center"`, `"right"`.
#' @param bold Logical, column name, or pattern string. If logical, applies to all rows.
#'   If column name (unquoted), uses that column's values. If pattern containing `{col}`
#'   (e.g., `"bold_{col}"`), expands to column name per styled column.
#' @param italic Logical, column name, or pattern string. Same behavior as `bold`.
#' @param color Text color. Can be a color value, column name, or pattern string.
#' @param background Background fill color. Can be a color value, column name, or pattern string.
#' @param border_left Left border specification (e.g., `TRUE`, `"1pt + gray"`).
#' @param border_right Right border specification.
#' @param font_size Font size (e.g., `"10pt"`, `"0.9em"`). Can be a column name or pattern string.
#' @param rotate Rotation angle (e.g., `"90deg"`, `90`, `"1.5rad"`). Can be a column name or pattern string.
#' @param .missing How to handle missing pattern columns: `"warn"` (default) emits a warning,
#'   `"ignore"` silently skips, `"error"` stops execution.
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
#' ## Pattern-based styling
#'
#' When styling multiple columns with a naming convention, use `{col}` patterns
#' to automatically expand column references:
#'
#' ```r
#' # Instead of repetitive calls:
#' data |>
#'   tt(cols = c(mpg, qsec)) |>
#'   tt_column(everything(), color = "color_{col}", background = "bg_{col}")
#' ```
#'
#' For column `mpg`, this looks for `color_mpg` and `bg_mpg` in the data.
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
#' # Pattern-based styling (column-specific style columns)
#' df <- data.frame(
#'   a = 1:3, b = 4:6,
#'   color_a = c("red", "green", "blue"),
#'   color_b = c("black", "gray", "white")
#' )
#' tt(df, cols = c(a, b), rownames = FALSE) |>
#'   tt_column(c(a, b), color = "color_{col}")
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
                      rotate = NULL,
                      .missing = c("warn", "ignore", "error")) {
  .check_typst_table(table)
  table <- .copy_table(table)
  .missing <- match.arg(.missing)

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

    # Handle bold (data-driven, pattern, or static)
    if (!rlang::quo_is_null(bold_quo)) {
      if (.is_column_ref(bold_quo)) {
        # Bare symbol column reference
        ref_col <- .get_column_name(bold_quo)
        if (ref_col %in% names(table$original_data)) {
          style$bold_col <- ref_col
        } else {
          .handle_missing_style_col(ref_col, col_name, "bold", .missing)
        }
      } else {
        bold_val <- rlang::eval_tidy(bold_quo)
        if (.is_style_pattern(bold_val)) {
          # Pattern: expand {col} -> actual column name
          ref_col <- .expand_style_pattern(bold_val, col_name)
          if (ref_col %in% names(table$original_data)) {
            style$bold_col <- ref_col
          } else {
            .handle_missing_style_col(ref_col, col_name, "bold", .missing)
          }
        } else {
          # Static value
          style$bold <- bold_val
        }
      }
    }

    # Handle italic (data-driven, pattern, or static)
    if (!rlang::quo_is_null(italic_quo)) {
      if (.is_column_ref(italic_quo)) {
        # Bare symbol column reference
        ref_col <- .get_column_name(italic_quo)
        if (ref_col %in% names(table$original_data)) {
          style$italic_col <- ref_col
        } else {
          .handle_missing_style_col(ref_col, col_name, "italic", .missing)
        }
      } else {
        italic_val <- rlang::eval_tidy(italic_quo)
        if (.is_style_pattern(italic_val)) {
          # Pattern: expand {col} -> actual column name
          ref_col <- .expand_style_pattern(italic_val, col_name)
          if (ref_col %in% names(table$original_data)) {
            style$italic_col <- ref_col
          } else {
            .handle_missing_style_col(ref_col, col_name, "italic", .missing)
          }
        } else {
          # Static value
          style$italic <- italic_val
        }
      }
    }

    # Handle color (data-driven, pattern, or static)
    if (!rlang::quo_is_null(color_quo)) {
      if (.is_column_ref(color_quo)) {
        # Bare symbol column reference
        ref_col <- .get_column_name(color_quo)
        if (ref_col %in% names(table$original_data)) {
          style$color_col <- ref_col
        } else {
          .handle_missing_style_col(ref_col, col_name, "color", .missing)
        }
      } else {
        color_val <- rlang::eval_tidy(color_quo)
        if (.is_style_pattern(color_val)) {
          # Pattern: expand {col} -> actual column name
          ref_col <- .expand_style_pattern(color_val, col_name)
          if (ref_col %in% names(table$original_data)) {
            style$color_col <- ref_col
          } else {
            .handle_missing_style_col(ref_col, col_name, "color", .missing)
          }
        } else {
          # Static value
          style$color <- color_val
        }
      }
    }

    # Handle background (data-driven, pattern, or static)
    if (!rlang::quo_is_null(background_quo)) {
      if (.is_column_ref(background_quo)) {
        # Bare symbol column reference
        ref_col <- .get_column_name(background_quo)
        if (ref_col %in% names(table$original_data)) {
          style$background_col <- ref_col
        } else {
          .handle_missing_style_col(ref_col, col_name, "background", .missing)
        }
      } else {
        background_val <- rlang::eval_tidy(background_quo)
        if (.is_style_pattern(background_val)) {
          # Pattern: expand {col} -> actual column name
          ref_col <- .expand_style_pattern(background_val, col_name)
          if (ref_col %in% names(table$original_data)) {
            style$background_col <- ref_col
          } else {
            .handle_missing_style_col(ref_col, col_name, "background", .missing)
          }
        } else {
          # Static value
          style$background <- background_val
        }
      }
    }

    # Handle font_size (data-driven, pattern, or static)
    if (!rlang::quo_is_null(font_size_quo)) {
      if (.is_column_ref(font_size_quo)) {
        # Bare symbol column reference
        ref_col <- .get_column_name(font_size_quo)
        if (ref_col %in% names(table$original_data)) {
          style$font_size_col <- ref_col
        } else {
          .handle_missing_style_col(ref_col, col_name, "font_size", .missing)
        }
      } else {
        font_size_val <- rlang::eval_tidy(font_size_quo)
        if (.is_style_pattern(font_size_val)) {
          # Pattern: expand {col} -> actual column name
          ref_col <- .expand_style_pattern(font_size_val, col_name)
          if (ref_col %in% names(table$original_data)) {
            style$font_size_col <- ref_col
          } else {
            .handle_missing_style_col(ref_col, col_name, "font_size", .missing)
          }
        } else {
          # Static value
          style$font_size <- font_size_val
        }
      }
    }

    # Handle rotate (data-driven, pattern, or static)
    if (!rlang::quo_is_null(rotate_quo)) {
      if (.is_column_ref(rotate_quo)) {
        # Bare symbol column reference
        ref_col <- .get_column_name(rotate_quo)
        if (ref_col %in% names(table$original_data)) {
          style$rotate_col <- ref_col
        } else {
          .handle_missing_style_col(ref_col, col_name, "rotate", .missing)
        }
      } else {
        rotate_val <- rlang::eval_tidy(rotate_quo)
        if (.is_style_pattern(rotate_val)) {
          # Pattern: expand {col} -> actual column name
          ref_col <- .expand_style_pattern(rotate_val, col_name)
          if (ref_col %in% names(table$original_data)) {
            style$rotate_col <- ref_col
          } else {
            .handle_missing_style_col(ref_col, col_name, "rotate", .missing)
          }
        } else {
          # Static value
          style$rotate <- rotate_val
        }
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
