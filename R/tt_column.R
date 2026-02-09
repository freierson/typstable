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
#' @param fill Fill color. Can be a color value, column name, or pattern string.
#' @param border_left Left border specification (e.g., `TRUE`, `"1pt + gray"`).
#' @param border_right Right border specification.
#' @param font_size Font size (e.g., `"10pt"`, `"0.9em"`). Can be a column name or pattern string.
#' @param rotate Rotation angle (e.g., `"90deg"`, `90`, `"1.5rad"`). Can be a column name or pattern string.
#' @param inset Cell padding (e.g., `"10pt"`, `"5pt 8pt"`). Can be a column name or pattern string.
#' @param stroke Stroke (border) specification for the cell(s). Can be `TRUE` for
#'   default 1pt black, a color, a Typst stroke spec like `"2pt + blue"`, or a
#'   Typst dictionary like `"(bottom: 1pt)"`. Can also be a column name or pattern string.
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
#'   tt_column(value, fill = bg_color)
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
#'   tt_column(everything(), color = "color_{col}", fill = "bg_{col}")
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
                      fill = NULL,
                      border_left = NULL,
                      border_right = NULL,
                      font_size = NULL,
                      rotate = NULL,
                      inset = NULL,
                      stroke = NULL,
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
  fill_quo <- rlang::enquo(fill)
  font_size_quo <- rlang::enquo(font_size)
  rotate_quo <- rlang::enquo(rotate)
  inset_quo <- rlang::enquo(inset)
  stroke_quo <- rlang::enquo(stroke)

  # Increment sequence counter for last-write-wins ordering
  table$style_seq <- table$style_seq + 1L
  seq <- table$style_seq

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
          style <- .set_style_attr(style, "bold_col", ref_col, seq)
        } else {
          .handle_missing_style_col(ref_col, col_name, "bold", .missing)
        }
      } else {
        bold_val <- rlang::eval_tidy(bold_quo)
        if (.is_style_pattern(bold_val)) {
          # Pattern: expand {col} -> actual column name
          ref_col <- .expand_style_pattern(bold_val, col_name)
          if (ref_col %in% names(table$original_data)) {
            style <- .set_style_attr(style, "bold_col", ref_col, seq)
          } else {
            .handle_missing_style_col(ref_col, col_name, "bold", .missing)
          }
        } else {
          # Static value
          style <- .set_style_attr(style, "bold", bold_val, seq)
        }
      }
    }

    # Handle italic (data-driven, pattern, or static)
    if (!rlang::quo_is_null(italic_quo)) {
      if (.is_column_ref(italic_quo)) {
        # Bare symbol column reference
        ref_col <- .get_column_name(italic_quo)
        if (ref_col %in% names(table$original_data)) {
          style <- .set_style_attr(style, "italic_col", ref_col, seq)
        } else {
          .handle_missing_style_col(ref_col, col_name, "italic", .missing)
        }
      } else {
        italic_val <- rlang::eval_tidy(italic_quo)
        if (.is_style_pattern(italic_val)) {
          # Pattern: expand {col} -> actual column name
          ref_col <- .expand_style_pattern(italic_val, col_name)
          if (ref_col %in% names(table$original_data)) {
            style <- .set_style_attr(style, "italic_col", ref_col, seq)
          } else {
            .handle_missing_style_col(ref_col, col_name, "italic", .missing)
          }
        } else {
          # Static value
          style <- .set_style_attr(style, "italic", italic_val, seq)
        }
      }
    }

    # Handle color (data-driven, pattern, or static)
    if (!rlang::quo_is_null(color_quo)) {
      if (.is_column_ref(color_quo)) {
        # Bare symbol column reference
        ref_col <- .get_column_name(color_quo)
        if (ref_col %in% names(table$original_data)) {
          style <- .set_style_attr(style, "color_col", ref_col, seq)
        } else {
          .handle_missing_style_col(ref_col, col_name, "color", .missing)
        }
      } else {
        color_val <- rlang::eval_tidy(color_quo)
        if (.is_style_pattern(color_val)) {
          # Pattern: expand {col} -> actual column name
          ref_col <- .expand_style_pattern(color_val, col_name)
          if (ref_col %in% names(table$original_data)) {
            style <- .set_style_attr(style, "color_col", ref_col, seq)
          } else {
            .handle_missing_style_col(ref_col, col_name, "color", .missing)
          }
        } else {
          # Static value
          style <- .set_style_attr(style, "color", color_val, seq)
        }
      }
    }

    # Handle fill (data-driven, pattern, or static)
    if (!rlang::quo_is_null(fill_quo)) {
      if (.is_column_ref(fill_quo)) {
        # Bare symbol column reference
        ref_col <- .get_column_name(fill_quo)
        if (ref_col %in% names(table$original_data)) {
          style <- .set_style_attr(style, "fill_col", ref_col, seq)
        } else {
          .handle_missing_style_col(ref_col, col_name, "fill", .missing)
        }
      } else {
        fill_val <- rlang::eval_tidy(fill_quo)
        if (.is_style_pattern(fill_val)) {
          # Pattern: expand {col} -> actual column name
          ref_col <- .expand_style_pattern(fill_val, col_name)
          if (ref_col %in% names(table$original_data)) {
            style <- .set_style_attr(style, "fill_col", ref_col, seq)
          } else {
            .handle_missing_style_col(ref_col, col_name, "fill", .missing)
          }
        } else {
          # Static value
          style <- .set_style_attr(style, "fill", fill_val, seq)
        }
      }
    }

    # Handle font_size (data-driven, pattern, or static)
    if (!rlang::quo_is_null(font_size_quo)) {
      if (.is_column_ref(font_size_quo)) {
        # Bare symbol column reference
        ref_col <- .get_column_name(font_size_quo)
        if (ref_col %in% names(table$original_data)) {
          style <- .set_style_attr(style, "font_size_col", ref_col, seq)
        } else {
          .handle_missing_style_col(ref_col, col_name, "font_size", .missing)
        }
      } else {
        font_size_val <- rlang::eval_tidy(font_size_quo)
        if (.is_style_pattern(font_size_val)) {
          # Pattern: expand {col} -> actual column name
          ref_col <- .expand_style_pattern(font_size_val, col_name)
          if (ref_col %in% names(table$original_data)) {
            style <- .set_style_attr(style, "font_size_col", ref_col, seq)
          } else {
            .handle_missing_style_col(ref_col, col_name, "font_size", .missing)
          }
        } else {
          # Static value
          style <- .set_style_attr(style, "font_size", font_size_val, seq)
        }
      }
    }

    # Handle rotate (data-driven, pattern, or static)
    if (!rlang::quo_is_null(rotate_quo)) {
      if (.is_column_ref(rotate_quo)) {
        # Bare symbol column reference
        ref_col <- .get_column_name(rotate_quo)
        if (ref_col %in% names(table$original_data)) {
          style <- .set_style_attr(style, "rotate_col", ref_col, seq)
        } else {
          .handle_missing_style_col(ref_col, col_name, "rotate", .missing)
        }
      } else {
        rotate_val <- rlang::eval_tidy(rotate_quo)
        if (.is_style_pattern(rotate_val)) {
          # Pattern: expand {col} -> actual column name
          ref_col <- .expand_style_pattern(rotate_val, col_name)
          if (ref_col %in% names(table$original_data)) {
            style <- .set_style_attr(style, "rotate_col", ref_col, seq)
          } else {
            .handle_missing_style_col(ref_col, col_name, "rotate", .missing)
          }
        } else {
          # Static value
          style <- .set_style_attr(style, "rotate", rotate_val, seq)
        }
      }
    }

    # Handle inset (data-driven, pattern, or static)
    if (!rlang::quo_is_null(inset_quo)) {
      if (.is_column_ref(inset_quo)) {
        # Bare symbol column reference
        ref_col <- .get_column_name(inset_quo)
        if (ref_col %in% names(table$original_data)) {
          style <- .set_style_attr(style, "inset_col", ref_col, seq)
        } else {
          .handle_missing_style_col(ref_col, col_name, "inset", .missing)
        }
      } else {
        inset_val <- rlang::eval_tidy(inset_quo)
        if (.is_style_pattern(inset_val)) {
          # Pattern: expand {col} -> actual column name
          ref_col <- .expand_style_pattern(inset_val, col_name)
          if (ref_col %in% names(table$original_data)) {
            style <- .set_style_attr(style, "inset_col", ref_col, seq)
          } else {
            .handle_missing_style_col(ref_col, col_name, "inset", .missing)
          }
        } else {
          # Static value
          style <- .set_style_attr(style, "inset", inset_val, seq)
        }
      }
    }

    # Handle stroke (data-driven, pattern, or static)
    if (!rlang::quo_is_null(stroke_quo)) {
      if (.is_column_ref(stroke_quo)) {
        # Bare symbol column reference
        ref_col <- .get_column_name(stroke_quo)
        if (ref_col %in% names(table$original_data)) {
          style <- .set_style_attr(style, "stroke_col", ref_col, seq)
        } else {
          .handle_missing_style_col(ref_col, col_name, "stroke", .missing)
        }
      } else {
        stroke_val <- rlang::eval_tidy(stroke_quo)
        if (.is_style_pattern(stroke_val)) {
          # Pattern: expand {col} -> actual column name
          ref_col <- .expand_style_pattern(stroke_val, col_name)
          if (ref_col %in% names(table$original_data)) {
            style <- .set_style_attr(style, "stroke_col", ref_col, seq)
          } else {
            .handle_missing_style_col(ref_col, col_name, "stroke", .missing)
          }
        } else {
          # Static value
          style <- .set_style_attr(style, "stroke", stroke_val, seq)
        }
      }
    }

    # Handle borders
    if (!is.null(border_left)) {
      style$border_left <- border_left
      # Add vline before this column (Typst x is 0-based: col_idx - 1)
      table$vlines <- c(table$vlines, list(list(x = col_idx - 1, stroke = border_left)))
    }

    if (!is.null(border_right)) {
      style$border_right <- border_right
      # Add vline after this column (Typst x is 0-based: col_idx)
      table$vlines <- c(table$vlines, list(list(x = col_idx, stroke = border_right)))
    }

    table$col_styles[[col_name]] <- style
  }

  table
}
