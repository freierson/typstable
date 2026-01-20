#' Render a typst_table to Typst markup
#'
#' Generates Typst code from a `typst_table` object. This is called automatically
#' by `print()` and `knit_print()`, but can also be called directly.
#'
#' @param table A `typst_table` object.
#'
#' @return A character string containing Typst table markup.
#'
#' @examples
#' code <- tt(mtcars[1:3, 1:3]) |> tt_render()
#' cat(code)
#'
#' @export
tt_render <- function(table) {
  .check_typst_table(table)

  # Build table arguments
  args <- .render_table_args(table)

  # Build booktabs-style rules (default)
  booktabs <- .render_booktabs(table)

  # Build header content (may include headers_above)
  header <- .render_all_headers(table)

  # Build horizontal lines (user-specified)
  hlines <- .render_hlines(table)

  # Build vertical lines
  vlines <- .render_vlines(table)

  # Build data rows
  rows <- .render_data_rows(table)

  # Build footnotes
  footnotes <- .render_footnotes(table)

  # Assemble table with booktabs rules
  table_content <- c(booktabs$top, header, booktabs$mid, hlines$after_header, rows, booktabs$bottom)
  table_content <- table_content[table_content != ""]  # Remove empty strings
  table_content <- paste(table_content, collapse = "\n  ")

  # Add vlines to args if any
  if (length(vlines) > 0) {
    # Vlines need to go in the table body, not args
    table_content <- paste(c(vlines, table_content), collapse = "\n  ")
  }

  table_code <- paste0("#table(\n  ", args, ",\n  ", table_content, "\n)")

  # Wrap in figure if caption or label provided
  if (!is.null(table$caption) || !is.null(table$label)) {
    table_code <- .wrap_in_figure(table_code, table$caption, table$label)
  }

  # Add footnotes after table
  if (nchar(footnotes) > 0) {
    table_code <- paste(table_code, footnotes, sep = "\n\n")
  }

  table_code
}

#' Render table arguments
#' @noRd
.render_table_args <- function(table) {
  args <- character()

  # Columns (widths)
  col_spec <- paste0("(", paste(table$col_widths, collapse = ", "), ")")
  args <- c(args, paste0("columns: ", col_spec))

  # Alignment
  if (!all(table$col_align == "left")) {
    if (length(unique(table$col_align)) == 1) {
      # Single alignment for all
      args <- c(args, paste0("align: ", table$col_align[1]))
    } else {
      # Per-column alignment
      align_spec <- paste0("(", paste(table$col_align, collapse = ", "), ")")
      args <- c(args, paste0("align: ", align_spec))
    }
  }

  # Stroke - default to none for booktabs style (only horizontal rules)
  if (!is.null(table$stroke)) {
    stroke <- .to_typst_stroke(table$stroke)
    args <- c(args, paste0("stroke: ", stroke))
  } else {
    args <- c(args, "stroke: none")
  }

  # Fill (background)
  if (!is.null(table$fill)) {
    fill <- .to_typst_color(table$fill)
    args <- c(args, paste0("fill: ", fill))
  } else if (isTRUE(table$striped)) {
    # Striped rows
    args <- c(args, 'fill: (_, y) => if calc.odd(y) { rgb("#f5f5f5") }')
  }

  # Inset
  if (!is.null(table$inset)) {
    inset <- .to_typst_length(table$inset)
    args <- c(args, paste0("inset: ", inset))
  }

  # Gutter
  if (!is.null(table$row_gutter)) {
    gutter <- .to_typst_length(table$row_gutter)
    args <- c(args, paste0("row-gutter: ", gutter))
  }
  # Check if we need column-gutter for multi-group header lines
  needs_header_gap <- any(vapply(table$headers_above, function(h) {
    isTRUE(h$line) && length(h$header) > 1
  }, logical(1)))
  if (!is.null(table$column_gutter)) {
    gutter <- .to_typst_length(table$column_gutter)
    args <- c(args, paste0("column-gutter: ", gutter))
  } else if (needs_header_gap) {
    # Add small gutter for visual gaps between header group lines
    args <- c(args, "column-gutter: 10pt")
  }

  paste(args, collapse = ",\n  ")
}

#' Render booktabs-style horizontal rules
#' @noRd
.render_booktabs <- function(table) {
  # Default booktabs style: top rule, mid rule (after header), bottom rule
  # Skip if user has set stroke (they want custom borders)
  if (!is.null(table$stroke)) {
    return(list(top = "", mid = "", bottom = ""))
  }

  list(
    top = "table.hline(stroke: 1pt),",
    mid = "table.hline(stroke: 0.5pt),",
    bottom = "table.hline(stroke: 1pt)"  # No trailing comma - it's last
  )
}

#' Render all header rows (headers_above + main header)
#' @noRd
.render_all_headers <- function(table) {
  # Get the main header row cells
  main_header_cells <- .render_header_cells(table)

  # Get headers_above cells
  headers_above_rows <- .render_headers_above_cells(table)

  needs_wrapper <- isTRUE(table$header_separate) || length(table$headers_above) > 0

  if (needs_wrapper) {
    # Combine all header content inside table.header()
    all_header_content <- c(headers_above_rows, main_header_cells)
    paste0("table.header(\n    ", paste(all_header_content, collapse = ",\n    "), "\n  ),")
  } else {
    # Just return main header cells with trailing comma
    paste0(paste(main_header_cells, collapse = ", "), ",")
  }
}

#' Render main header row cells
#' @noRd
.render_header_cells <- function(table) {
  # Get header styling (row 0)
  header_style <- table$row_styles[["0"]]

  vapply(seq_along(table$col_names), function(i) {
    content <- table$col_names[i]

    # Escape if needed
    if (table$escape) {
      content <- .escape_typst(content)
    }

    # Get column style
    col_style <- table$col_styles[[table$display_cols[i]]]

    # Merge styles: header row style + column style
    style <- .merge_styles(header_style, col_style)

    # Check for cell-specific override (row 0, col i)
    cell_key <- paste0("0_", i)
    cell_style <- table$cell_styles[[cell_key]]
    if (!is.null(cell_style)) {
      style <- .merge_styles(style, cell_style)
    }

    .render_cell(content, style, table, row = 0, col = i)
  }, character(1))
}

#' Render headers_above cells (returns vector of cell strings)
#' @noRd
.render_headers_above_cells <- function(table) {
  if (length(table$headers_above) == 0) return(character())

  # Collect all cells from all headers_above rows
  all_cells <- character()

  for (header_spec in table$headers_above) {
    row_cells <- character()
    # Use cell borders for gaps when multiple groups with line=TRUE
    use_cell_borders <- isTRUE(header_spec$line) && length(header_spec$header) > 1

    for (i in seq_along(header_spec$header)) {
      span <- header_spec$header[i]
      label <- names(header_spec$header)[i]

      if (is.null(label) || label == "") {
        label <- ""
      }

      # Escape if needed
      if (table$escape && label != "") {
        label <- .escape_typst(label)
      }

      # Apply styling
      style <- list(
        bold = header_spec$bold %||% TRUE,
        align = header_spec$align,
        color = header_spec$color,
        background = header_spec$background
      )

      content <- .format_text(label, bold = style$bold)

      # Build cell arguments
      cell_args <- character()
      if (span > 1) {
        cell_args <- c(cell_args, paste0("colspan: ", span))
      }
      if (!is.null(style$align)) {
        cell_args <- c(cell_args, paste0("align: ", .to_typst_align(style$align)))
      }
      if (!is.null(style$background)) {
        cell_args <- c(cell_args, paste0("fill: ", .to_typst_color(style$background)))
      }
      if (use_cell_borders) {
        # Add bottom border to create lines with gaps between groups
        cell_args <- c(cell_args, "stroke: (bottom: 0.5pt)")
      }

      if (length(cell_args) > 0) {
        row_cells <- c(row_cells, paste0("table.cell(", paste(cell_args, collapse = ", "), ")[", content, "]"))
      } else {
        row_cells <- c(row_cells, paste0("[", content, "]"))
      }
    }

    all_cells <- c(all_cells, row_cells)

    # Add hline below if requested (only for single group)
    if (isTRUE(header_spec$line) && length(header_spec$header) == 1) {
      all_cells <- c(all_cells, "table.hline()")
    }
  }

  all_cells
}

#' Render data rows
#' @noRd
.render_data_rows <- function(table) {
  rows <- character(table$nrow)

  # Get row groups for indentation
  row_groups <- .get_row_group_info(table)

  for (i in seq_len(table$nrow)) {
    row_cells <- character(table$ncol)

    # Get row styling
    row_style <- table$row_styles[[as.character(i)]]

    # Check for hline above this row
    hline_above <- .get_hline_at(table, i, "above")

    # Check if this row starts a group
    group_label <- row_groups$labels[i]

    for (j in seq_len(table$ncol)) {
      col_name <- table$display_cols[j]
      content <- as.character(table$display_data[i, j])

      # Handle NA
      if (is.na(content)) content <- ""

      # Escape if needed
      if (table$escape) {
        content <- .escape_typst(content)
      }

      # Check for indent (from row group)
      if (isTRUE(row_groups$indent[i]) && j == 1) {
        content <- paste0("#h(1em)", content)
      }

      # Get column style (may include data-driven values)
      col_style <- .resolve_column_style(table, j, i)

      # Merge row style with column style
      style <- .merge_styles(row_style, col_style)

      # Check for cell-specific override
      cell_key <- paste0(i, "_", j)
      cell_style <- table$cell_styles[[cell_key]]
      if (!is.null(cell_style)) {
        style <- .merge_styles(style, cell_style)
      }

      row_cells[j] <- .render_cell(content, style, table, row = i, col = j)
    }

    # Combine cells for this row
    row_str <- paste(row_cells, collapse = ", ")

    # Prepend group label row if needed
    if (!is.null(group_label) && !is.na(group_label) && group_label != "") {
      group_row <- .render_group_label_row(table, group_label, row_groups$bold_label[i])
      row_str <- paste(group_row, row_str, sep = ",\n  ")
    }

    # Prepend hline if needed
    if (!is.null(hline_above)) {
      row_str <- paste(hline_above, row_str, sep = ",\n  ")
    }

    rows[i] <- row_str

    # Check for hline below this row
    hline_below <- .get_hline_at(table, i, "below")
    if (!is.null(hline_below)) {
      rows[i] <- paste(rows[i], hline_below, sep = ",\n  ")
    }
  }

  # Join rows with commas, add trailing comma for bottom rule
  paste0(paste(rows, collapse = ",\n  "), ",")
}

#' Render a single cell
#' @noRd
.render_cell <- function(content, style, table, row, col) {
  if (is.null(style) || length(style) == 0) {
    return(paste0("[", content, "]"))
  }

  # Check if we need table.cell wrapper (for fill, colspan, rowspan, align)
  needs_wrapper <- !is.null(style$background) ||
                   !is.null(style$colspan) && style$colspan > 1 ||
                   !is.null(style$rowspan) && style$rowspan > 1 ||
                   !is.null(style$cell_align)

  # Apply text formatting
  formatted <- content
  if (!is.null(style$content)) {
    # Override content
    formatted <- style$content
    if (table$escape) {
      formatted <- .escape_typst(formatted)
    }
  }

  formatted <- .format_text(
    formatted,
    bold = style$bold,
    italic = style$italic,
    color = style$color,
    size = style$font_size
  )

  if (needs_wrapper) {
    cell_args <- character()

    if (!is.null(style$colspan) && style$colspan > 1) {
      cell_args <- c(cell_args, paste0("colspan: ", style$colspan))
    }
    if (!is.null(style$rowspan) && style$rowspan > 1) {
      cell_args <- c(cell_args, paste0("rowspan: ", style$rowspan))
    }
    if (!is.null(style$cell_align)) {
      cell_args <- c(cell_args, paste0("align: ", .to_typst_align(style$cell_align)))
    }
    if (!is.null(style$background)) {
      cell_args <- c(cell_args, paste0("fill: ", .to_typst_color(style$background)))
    }

    paste0("table.cell(", paste(cell_args, collapse = ", "), ")[", formatted, "]")
  } else {
    paste0("[", formatted, "]")
  }
}

#' Resolve column style with data-driven values
#' @noRd
.resolve_column_style <- function(table, col_idx, row_idx) {
  col_name <- table$display_cols[col_idx]
  col_style <- table$col_styles[[col_name]]

  if (is.null(col_style)) return(NULL)

  # Check for data-driven attributes
  resolved <- col_style

  for (attr in c("background", "color", "bold", "italic", "font_size")) {
    col_ref <- col_style[[paste0(attr, "_col")]]
    if (!is.null(col_ref)) {
      # Look up value in original data
      if (col_ref %in% names(table$original_data)) {
        value <- table$original_data[row_idx, col_ref]
        if (!is.na(value)) {
          # For boolean attributes, convert to logical
          if (attr %in% c("bold", "italic")) {
            resolved[[attr]] <- as.logical(value)
          } else {
            resolved[[attr]] <- value
          }
        }
      }
    }
  }

  resolved
}

#' Merge two style lists
#' @noRd
.merge_styles <- function(base, override) {
  if (is.null(base)) return(override)
  if (is.null(override)) return(base)

  # Override takes precedence
  result <- base
  for (name in names(override)) {
    if (!is.null(override[[name]])) {
      result[[name]] <- override[[name]]
    }
  }
  result
}

#' Render horizontal lines
#' @noRd
.render_hlines <- function(table) {
  result <- list(after_header = NULL)

  # Check for header separator
  if (isTRUE(table$header_separate)) {
    result$after_header <- "table.hline(),"
  }

  result
}

#' Render vertical lines
#' @noRd
.render_vlines <- function(table) {
  if (length(table$vlines) == 0) return(character())

  vapply(table$vlines, function(vline) {
    args <- character()

    if (!is.null(vline$x)) {
      args <- c(args, paste0("x: ", vline$x))
    }
    if (!is.null(vline$start)) {
      args <- c(args, paste0("start: ", vline$start))
    }
    if (!is.null(vline$end)) {
      args <- c(args, paste0("end: ", vline$end))
    }
    if (!is.null(vline$stroke)) {
      args <- c(args, paste0("stroke: ", .to_typst_stroke(vline$stroke)))
    }

    if (length(args) > 0) {
      paste0("table.vline(", paste(args, collapse = ", "), "),")
    } else {
      "table.vline(),"
    }
  }, character(1))
}

#' Get row group info
#' @noRd
.get_row_group_info <- function(table) {
  labels <- rep(NA_character_, table$nrow)
  indent <- rep(FALSE, table$nrow)
  bold_label <- rep(TRUE, table$nrow)

  for (group in table$row_groups) {
    if (!is.null(group$start_row) && group$start_row <= table$nrow) {
      labels[group$start_row] <- group$group_label
      bold_label[group$start_row] <- group$bold_label %||% TRUE

      # Indent rows in group
      if (isTRUE(group$indent)) {
        end_row <- min(group$end_row %||% table$nrow, table$nrow)
        indent[group$start_row:end_row] <- TRUE
      }
    }
  }

  list(labels = labels, indent = indent, bold_label = bold_label)
}

#' Render group label row
#' @noRd
.render_group_label_row <- function(table, label, bold = TRUE) {
  if (table$escape) {
    label <- .escape_typst(label)
  }

  if (isTRUE(bold)) {
    label <- paste0("*", label, "*")
  }

  # Span all columns
  paste0("table.cell(colspan: ", table$ncol, ")[", label, "]")
}

#' Get hline at position
#' @noRd
.get_hline_at <- function(table, row, position = "above") {
  for (hline in table$hlines) {
    if ((position == "above" && hline$y == row) ||
        (position == "below" && hline$y == row + 1)) {
      args <- character()

      if (!is.null(hline$start)) {
        args <- c(args, paste0("start: ", hline$start))
      }
      if (!is.null(hline$end)) {
        args <- c(args, paste0("end: ", hline$end))
      }
      if (!is.null(hline$stroke)) {
        args <- c(args, paste0("stroke: ", .to_typst_stroke(hline$stroke)))
      }

      if (length(args) > 0) {
        return(paste0("table.hline(", paste(args, collapse = ", "), ")"))
      } else {
        return("table.hline()")
      }
    }
  }
  NULL
}

#' Render footnotes
#' @noRd
.render_footnotes <- function(table) {
  if (length(table$footnotes) == 0) return("")

  notes <- character()

  # General footnotes
  if (!is.null(table$footnotes$general)) {
    for (note in table$footnotes$general) {
      notes <- c(notes, note)
    }
  }

  # Numbered footnotes
  if (!is.null(table$footnotes$number)) {
    for (i in seq_along(table$footnotes$number)) {
      notes <- c(notes, paste0(i, ". ", table$footnotes$number[i]))
    }
  }

  # Symbol footnotes
  if (!is.null(table$footnotes$symbol)) {
    symbols <- c("*", "\u2020", "\u2021", "\u00a7", "\u00b6", "#")
    for (i in seq_along(table$footnotes$symbol)) {
      sym <- symbols[(i - 1) %% length(symbols) + 1]
      notes <- c(notes, paste0(sym, " ", table$footnotes$symbol[i]))
    }
  }

  if (length(notes) == 0) return("")

  paste0("#text(size: 0.9em)[\n", paste(notes, collapse = " \\\n"), "\n]")
}

#' Wrap table in figure
#' @noRd
.wrap_in_figure <- function(table_code, caption, label) {
  args <- character()

  if (!is.null(caption)) {
    args <- c(args, paste0("caption: [", caption, "]"))
  }

  # Remove leading # from table code when embedding in figure
  # (inside function arguments, you don't use # for function calls)
  table_inner <- sub("^#", "", table_code)

  inner <- table_code

  if (length(args) > 0) {
    inner <- paste0("#figure(\n  ", table_inner, ",\n  ", paste(args, collapse = ",\n  "), "\n)")
  }

  # Add label if provided
  if (!is.null(label)) {
    inner <- paste0(inner, " <", label, ">")
  }

  inner
}

#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
