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
    rlang::warn(paste0("Unknown color '", color, "', using black"))
    return("black")
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
#' @return Formatted Typst string
#' @noRd
.format_text <- function(content, bold = FALSE, italic = FALSE,
                         color = NULL, size = NULL) {
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

  # Look for first header_above with gap specified and multiple groups
  for (header_spec in table$headers_above) {
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

      break  # Only use first header with gaps
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
