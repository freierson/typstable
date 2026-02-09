# Utility functions for typstable package

#' Escape special Typst characters
#'
#' @param x Character vector to escape
#' @return Escaped character vector
#' @noRd
.escape_typst <- function(x) {
  if (!is.character(x)) {
    x <- as.character(x)
  }
  x[is.na(x)] <- ""

  # Escape all Typst special characters: \ * _ ` # @ < >
  gsub("([\\\\*_`#@<>])", "\\\\\\1", x, perl = TRUE)
}

#' Convert color specification to Typst format
#'
#' Accepts various color formats and converts them to Typst color syntax.
#'
#' @param color Color specification: hex string, named color, or rgb values
#' @return Typst color string (e.g., `rgb("#ff0000")` or `blue`)
#' @noRd
.to_typst_color <- function(color) {
  if (is.null(color)) return(NULL)

  # Handle vectors
  if (length(color) > 1) {
    return(vapply(color, .to_typst_color, character(1), USE.NAMES = FALSE))
  }

  # Handle NA
  if (is.na(color)) return(NA_character_)

  # Already a Typst color expression (starts with rgb, luma, etc.)
  if (grepl("^(rgb|luma|cmyk|oklab|oklch|color\\.linear-rgb|color\\.hsl|color\\.hsv)\\(", color)) {
    return(color)
  }

  # Named Typst colors
  typst_colors <- c("black", "gray", "silver", "white", "navy", "blue", "aqua",
                    "teal", "eastern", "purple", "fuchsia", "maroon", "red",
                    "orange", "yellow", "olive", "green", "lime")
  if (tolower(color) %in% typst_colors) {
    return(tolower(color))
  }

  # Hex color (with or without #)
  if (grepl("^#?[0-9A-Fa-f]{3,8}$", color)) {
    hex <- sub("^#", "", color)
    return(paste0('rgb("#', hex, '")'))
  }

  # Try to convert R color name to hex
  tryCatch({
    rgb_vals <- grDevices::col2rgb(color)
    hex <- sprintf("%02x%02x%02x", rgb_vals[1], rgb_vals[2], rgb_vals[3])
    return(paste0('rgb("#', hex, '")'))
  }, error = function(e) {
    # Not a recognized color — pass through as raw Typst
    return(color)
  })
}

#' Convert length/width specification to Typst format
#'
#' Validates and converts length values to Typst length syntax.
#'
#' @param length Length specification: number (assumed pt), or string with unit
#' @param allow_fr Logical, whether to allow fr units (for column widths)
#' @return Typst length string
#' @noRd
.to_typst_length <- function(length, allow_fr = FALSE) {
  if (is.null(length)) return(NULL)

  # Handle vectors
  if (length(length) > 1) {
    return(vapply(length, .to_typst_length, character(1),
                  allow_fr = allow_fr, USE.NAMES = FALSE))
  }

  # Handle NA
  if (is.na(length)) return(NA_character_)

  # Numeric: assume pt
  if (is.numeric(length)) {
    return(paste0(length, "pt"))
  }

  # String with unit
  valid_units <- c("pt", "mm", "cm", "in", "em", "%")
  if (allow_fr) valid_units <- c(valid_units, "fr")

  # Check if it's a valid length string
  pattern <- paste0("^-?[0-9]*\\.?[0-9]+(", paste(valid_units, collapse = "|"), ")$")
  if (grepl(pattern, length)) {
    return(length)
  }

  # Special values
  if (length %in% c("auto", "1fr", "2fr", "3fr", "4fr", "5fr") ||
      (allow_fr && grepl("^[0-9]+fr$", length))) {
    return(length)
  }

  rlang::warn(paste0("Invalid length '", length, "', using auto"))
  return("auto")
}

#' Convert alignment specification to Typst format
#'
#' @param align Alignment: "left", "center", "right", "top", "bottom", or combinations
#' @return Typst alignment value
#' @noRd
.to_typst_align <- function(align) {
  if (is.null(align)) return(NULL)

  # Handle vectors
  if (length(align) > 1) {
    return(vapply(align, .to_typst_align, character(1), USE.NAMES = FALSE))
  }

  # Handle NA
  if (is.na(align)) return(NA_character_)

  # Map common aliases
  align <- tolower(align)
  align_map <- c(
    "l" = "left",
    "c" = "center",
    "r" = "right",
    "t" = "top",
    "b" = "bottom"
  )

  if (align %in% names(align_map)) {
    align <- unname(align_map[align])
  }

  # Valid Typst alignments
  valid <- c("left", "center", "right", "top", "bottom", "horizon", "start", "end")

  # Handle combined alignments (e.g., "center + horizon")
  if (grepl("\\+", align)) {
    parts <- trimws(strsplit(align, "\\+")[[1]])
    if (all(parts %in% valid)) {
      return(paste(parts, collapse = " + "))
    }
  }

  if (align %in% valid) {
    return(align)
  }

  rlang::warn(paste0("Invalid alignment '", align, "', using left"))
  return("left")
}

#' Convert angle specification to Typst format
#'
#' Validates and converts angle values to Typst angle syntax.
#'
#' @param angle Angle specification: number (assumed degrees), or string with unit
#'              (deg, rad, grad, turn)
#' @return Typst angle string
#' @noRd
.to_typst_angle <- function(angle) {
  if (is.null(angle)) return(NULL)

  # Handle vectors
  if (length(angle) > 1) {
    return(vapply(angle, .to_typst_angle, character(1), USE.NAMES = FALSE))
  }

  # Handle NA
  if (is.na(angle)) return(NA_character_)

  # Numeric: assume degrees
  if (is.numeric(angle)) {
    return(paste0(angle, "deg"))
  }

  # String with unit - validate format
  valid_units <- c("deg", "rad", "grad", "turn")
  pattern <- paste0("^-?[0-9]*\\.?[0-9]+(", paste(valid_units, collapse = "|"), ")$")

  if (grepl(pattern, angle)) {
    return(angle)
  }

  rlang::warn(paste0("Invalid angle '", angle, "', using 0deg"))
  return("0deg")
}

#' Convert stroke specification to Typst format
#'
#' @param stroke Stroke specification: TRUE (1pt + black), color, or width + color
#' @return Typst stroke string
#' @noRd
.to_typst_stroke <- function(stroke) {
  if (is.null(stroke) || isFALSE(stroke)) return(NULL)

  if (isTRUE(stroke)) {
    return("1pt + black")
  }

  # Complex Typst expression (parentheses, braces, or colons) — pass through
  if (is.character(stroke) && grepl("[(){}:]", stroke)) {
    return(stroke)
  }

  # If it's just a color, add default width
  if (is.character(stroke) && !grepl("\\+", stroke) && !grepl("pt|mm|cm", stroke)) {
    color <- .to_typst_color(stroke)
    return(paste0("1pt + ", color))
  }

  # Already formatted stroke
  stroke
}

#' Check if a value is a column reference (symbol)
#'
#' @param x A quosure to check
#' @return Logical
#' @noRd
.is_column_ref <- function(x) {
  rlang::quo_is_symbol(x)
}

#' Check if a value is a style pattern containing {col}
#'
#' @param x A value to check
#' @return Logical
#' @noRd
.is_style_pattern <- function(x) {
  is.character(x) && length(x) == 1 && grepl("{col}", x, fixed = TRUE)
}

#' Expand a style pattern by replacing {col} with column name
#'
#' @param pattern Pattern string containing {col}
#' @param col_name Column name to substitute
#' @return Expanded string
#' @noRd
.expand_style_pattern <- function(pattern, col_name) {
  gsub("{col}", col_name, pattern, fixed = TRUE)
}

#' Handle missing style column based on mode
#'
#' @param ref_col Name of the referenced column that was not found
#' @param display_col Name of the column being styled
#' @param attr_name Name of the style attribute (e.g., "color", "fill")
#' @param missing_mode One of "warn", "ignore", or "error"
#' @noRd
.handle_missing_style_col <- function(ref_col, display_col, attr_name, missing_mode) {
  msg <- paste0("Column '", ref_col, "' not found for ", attr_name,
                " styling of column '", display_col, "'")
  switch(match.arg(missing_mode, c("warn", "ignore", "error")),
    ignore = invisible(NULL),
    warn = rlang::warn(msg),
    error = rlang::abort(msg)
  )
}

#' Get column name from a quosure
#'
#' @param x A quosure
#' @return Column name as string, or NULL if not a symbol
#' @noRd
.get_column_name <- function(x) {
  if (rlang::quo_is_symbol(x)) {
    rlang::as_name(x)
  } else {
    NULL
  }
}

#' Format text with Typst styling
#'
#' Wraps content with Typst text formatting.
#'
#' @param content Content to format
#' @param bold Logical, make bold
#' @param italic Logical, make italic
#' @param color Text color
#' @param size Font size
#' @param rotate Rotation angle
#' @return Formatted Typst string
#' @noRd
.format_text <- function(content, bold = FALSE, italic = FALSE,
                         color = NULL, size = NULL, rotate = NULL) {
  result <- content

  # Apply text() wrapper if color or size specified
  if (!is.null(color) || !is.null(size)) {
    args <- character()
    if (!is.null(color)) {
      args <- c(args, paste0("fill: ", .to_typst_color(color)))
    }
    if (!is.null(size)) {
      args <- c(args, paste0("size: ", .to_typst_length(size)))
    }
    result <- paste0("#text(", paste(args, collapse = ", "), ")[", result, "]")
  }

  # Apply bold/italic with markup
  if (isTRUE(bold)) {
    result <- paste0("*", result, "*")
  }
  if (isTRUE(italic)) {
    result <- paste0("_", result, "_")
  }

  # Apply rotation as outermost wrapper (after bold/italic)
  if (!is.null(rotate)) {
    angle <- .to_typst_angle(rotate)
    if (!is.na(angle) && angle != "0deg") {
      result <- paste0("#rotate(", angle, ", reflow: true)[", result, "]")
    }
  }

  result
}

#' Validate that input is a typst_table object
#'
#' @param x Object to validate
#' @param arg_name Name of the argument for error messages
#' @noRd
.check_typst_table <- function(x, arg_name = "table") {
  if (!inherits(x, "typst_table")) {
    rlang::abort(paste0(
      "`", arg_name, "` must be a typst_table object created with `tt()`"
    ))
  }
}

#' Get gap column information from headers_above
#'
#' Analyzes headers_above specifications to determine where gap columns
#' should be inserted between header groups.
#'
#' @param table A typst_table object
#' @return A list with:
#'   - has_gaps: logical, whether any gaps are needed
#'   - positions: integer vector of original column indices after which to insert gaps
#'   - widths: character vector of gap widths (same length as positions)
#'   - total_cols: total number of columns including gaps
#' @noRd
.get_gap_info <- function(table) {
  result <- list(
    has_gaps = FALSE,
    positions = integer(0),
    widths = character(0),
    total_cols = table$ncol
  )

  if (length(table$headers_above) == 0) {
    return(result)
  }

  # No gaps when stroke is active (grid borders make gaps redundant)
  if (!is.null(table$stroke)) {
    return(result)
  }

  # Look for the innermost (last) header_above with gap specified and multiple groups
  for (header_spec in rev(table$headers_above)) {
    if (!is.null(header_spec$gap) && length(header_spec$header) > 1) {
      gap_width <- .to_typst_length(header_spec$gap)

      # Calculate positions where gaps should be inserted
      # Gaps go between groups (after each group except the last)
      positions <- integer(0)
      cumsum_cols <- cumsum(header_spec$header)

      # Insert gap after each group boundary except the last
      for (i in seq_len(length(cumsum_cols) - 1)) {
        positions <- c(positions, cumsum_cols[i])
      }

      result$has_gaps <- TRUE
      result$positions <- positions
      result$widths <- rep(gap_width, length(positions))
      result$total_cols <- table$ncol + length(positions)

      break  # Only use innermost header with gaps
    }
  }

  result
}

#' Deep copy a typst_table object
#'
#' Creates a deep copy to avoid modifying the original when piping.
#'
#' @param table A typst_table object
#' @return A deep copy of the table
#' @noRd
.copy_table <- function(table) {
  # Lists are copied by value in R, but environments are not
  # Since we use lists, simple assignment creates a copy
  # But nested lists need explicit copying for safety
  new_table <- table
  new_table$col_styles <- lapply(table$col_styles, function(x) x)
  new_table$row_styles <- lapply(table$row_styles, function(x) x)
  new_table$cell_styles <- lapply(table$cell_styles, function(x) x)
  new_table$headers_above <- lapply(table$headers_above, function(x) x)
  new_table$row_groups <- lapply(table$row_groups, function(x) x)
  new_table$hlines <- lapply(table$hlines, function(x) x)
  new_table$vlines <- lapply(table$vlines, function(x) x)
  new_table
}

#' Resolve a column index to its header_above group
#'
#' Maps a column index to the group it falls within in a header_above spec,
#' returning the group index and its starting column.
#'
#' @param header_spec A header_above specification (list with $header named vector)
#' @param col_idx Integer column index
#' @return A list with group_idx and start_col
#' @noRd
.resolve_header_group <- function(header_spec, col_idx) {
  cumspans <- cumsum(header_spec$header)
  group_idx <- which(col_idx <= cumspans)[1]
  start_col <- if (group_idx == 1) 1L else cumspans[group_idx - 1] + 1L
  list(group_idx = group_idx, start_col = start_col)
}

#' Set a style attribute with sequence tracking
#'
#' @param style A style list
#' @param attr Attribute name (e.g., "bold", "color")
#' @param value The value to set
#' @param seq The sequence number for last-write-wins ordering
#' @return The updated style list
#' @noRd
.set_style_attr <- function(style, attr, value, seq) {
  style[[attr]] <- value
  style[[paste0(".seq_", attr)]] <- seq
  style
}
