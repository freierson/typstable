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

  # Resolve boundary hlines (pre-added at init or user overrides)
  top_rule    <- .resolve_boundary_hline(table, 0)
  mid_rule    <- .resolve_boundary_hline(table, 1)
  bottom_rule <- .resolve_boundary_hline(table, table$nrow + 1)

  # Build header content (returns vector of lines)
  header_lines <- .render_all_headers(table)

  # Build vertical lines (user-specified)
  vlines <- .render_vlines(table)

  # Build data rows
  rows <- .render_data_rows(table)

  if (isTRUE(table$repeat_header)) {
    # Wrap header content in table.header()
    inner_parts <- c(top_rule, header_lines, mid_rule)
    inner_parts <- inner_parts[nzchar(inner_parts)]
    header_block <- paste0(
      "table.header(\n    ",
      paste(inner_parts, collapse = ",\n    "),
      "\n  ),")
    body_parts <- c(header_block, rows, bottom_rule)
  } else {
    # No wrapper
    header_str <- paste(header_lines, collapse = ",\n  ")
    if (nzchar(header_str)) header_str <- paste0(header_str, ",")
    bt_top <- if (nzchar(top_rule)) paste0(top_rule, ",") else ""
    bt_mid <- if (nzchar(mid_rule)) paste0(mid_rule, ",") else ""
    body_parts <- c(bt_top, header_str, bt_mid, rows, bottom_rule)
  }

  table_content <- body_parts[nzchar(body_parts)]
  table_content <- paste(table_content, collapse = "\n  ")

  # Add user vlines
  if (length(vlines) > 0) {
    table_content <- paste(c(vlines, table_content), collapse = "\n  ")
  }

  table_code <- paste0("#table(\n  ", args, ",\n  ", table_content, "\n)")

  # Prepend preamble if set
  if (!is.null(table$preamble)) {
    table_code <- paste0(table$preamble, "\n", table_code)
  }

  # Append epilogue if set
  if (!is.null(table$epilogue)) {
    table_code <- paste0(table_code, "\n", table$epilogue)
  }

  table_code
}

#' Render table arguments
#' @noRd
.render_table_args <- function(table) {
  args <- character()

  col_widths <- table$col_widths
  col_align <- table$col_align

  col_spec <- paste0("(", paste(col_widths, collapse = ", "), ")")
  args <- c(args, paste0("columns: ", col_spec))

  # Alignment
  if (!all(col_align == "left")) {
    if (length(unique(col_align)) == 1) {
      # Single alignment for all
      args <- c(args, paste0("align: ", col_align[1]))
    } else {
      # Per-column alignment
      align_spec <- paste0("(", paste(col_align, collapse = ", "), ")")
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

  # Fill
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
  if (!is.null(table$column_gutter)) {
    gutter <- .to_typst_length(table$column_gutter)
    args <- c(args, paste0("column-gutter: ", gutter))
  }

  paste(args, collapse = ",\n  ")
}

#' Resolve a boundary hline (top/mid/bottom rule)
#'
#' Searches hlines in reverse for the last spec at position y. Returns the
#' rendered string, or "" if none found.
#' @noRd
.resolve_boundary_hline <- function(table, y) {
  for (i in rev(seq_along(table$hlines))) {
    hline <- table$hlines[[i]]
    if (hline$y == y) {
      if (isFALSE(hline$stroke)) return("")
      args <- character()
      if (!is.null(hline$start)) args <- c(args, paste0("start: ", hline$start))
      if (!is.null(hline$end))   args <- c(args, paste0("end: ", hline$end + 1))
      if (!is.null(hline$stroke)) args <- c(args, paste0("stroke: ", .to_typst_stroke(hline$stroke)))
      if (length(args) > 0) return(paste0("table.hline(", paste(args, collapse = ", "), ")"))
      else return("table.hline()")
    }
  }
  ""
}

#' Render all header rows (headers_above + main header)
#'
#' Returns a character vector where each element is one logical line of output
#' (a row of cells, an hline, a vline, or a hidden separator row).
#' @noRd
.render_all_headers <- function(table) {
  main_header_cells <- .render_header_cells(table)
  main_header_line <- paste(main_header_cells, collapse = ", ")

  headers_above_lines <- .render_headers_above_lines(table)

  c(headers_above_lines, main_header_line)
}

#' Render main header row cells
#' @noRd
.render_header_cells <- function(table) {
  # Get header styling (row 0)
  header_style <- table$row_styles[["0"]]

  cells <- character()

  for (i in seq_along(table$col_names)) {
    content <- table$col_names[i]

    # Escape if needed
    if (table$escape) {
      content <- .escape_typst(content)
    }

    # Get column style (filter out data-driven attributes for header row)
    col_style <- table$col_styles[[table$display_cols[i]]]
    if (!is.null(col_style)) {
      # Remove data-driven attributes (*_col) and their seq keys as they don't apply to header cells
      data_driven <- c("fill_col", "color_col", "bold_col", "italic_col", "font_size_col", "rotate_col", "inset_col", "stroke_col")
      data_driven_seq <- paste0(".seq_", data_driven)
      col_style <- col_style[!names(col_style) %in% c(data_driven, data_driven_seq)]
    }

    # Merge styles: header row style + column style
    style <- .merge_styles(header_style, col_style)

    # Check for cell-specific override (row 0, col i)
    cell_key <- paste0("0_", i)
    cell_style <- table$cell_styles[[cell_key]]
    if (!is.null(cell_style)) {
      style <- .merge_styles(style, cell_style)
    }

    cells <- c(cells, .render_cell(content, style, table, row = 0, col = i))
  }

  cells
}

#' Render headers_above as lines (hidden rows + white vlines approach)
#'
#' Returns a character vector of output lines: header cells, hlines,
#' white vlines for separation, and hidden separator rows.
#' @noRd
.render_headers_above_lines <- function(table) {
  if (length(table$headers_above) == 0) return(character())

  all_lines <- character()
  current_row <- 0

  for (header_idx in seq_along(table$headers_above)) {
    header_spec <- table$headers_above[[header_idx]]
    neg_row <- -(length(table$headers_above) - header_idx + 1)

    # Determine if separator (hidden row + white vlines) is needed
    needs_separator <- !is.null(header_spec$line_sep) &&
                       length(header_spec$header) > 1 &&
                       isTRUE(header_spec$line) &&
                       is.null(table$stroke)

    # Build cells for this header_above row
    row_cells <- .render_header_above_row_cells(table, header_spec, neg_row)
    all_lines <- c(all_lines, paste(row_cells, collapse = ", "))

    if (needs_separator) {
      # Full-width hline below this header row
      all_lines <- c(all_lines, "table.hline(stroke: 0.5pt)")

      # White vlines at group boundaries
      cumspans <- cumsum(header_spec$header)
      hidden_row <- current_row + 1
      line_sep_width <- .to_typst_length(header_spec$line_sep)

      for (i in seq_len(length(cumspans) - 1)) {
        x_pos <- cumspans[i]
        vline_str <- paste0(
          "table.vline(x: ", x_pos,
          ", start: ", hidden_row,
          ", end: ", hidden_row + 1,
          ", stroke: ", line_sep_width, " + white)")
        all_lines <- c(all_lines, vline_str)
      }

      # Hidden row (empty cells with zero vertical inset)
      hidden_cell <- "table.cell(inset: (y: 0pt))[]"
      all_lines <- c(all_lines, paste(rep(hidden_cell, table$ncol), collapse = ", "))

      current_row <- current_row + 2
    } else {
      # Single group or no separator — just add hline if requested
      if (isTRUE(header_spec$line) && is.null(table$stroke)) {
        all_lines <- c(all_lines, "table.hline(stroke: 0.5pt)")
      }
      current_row <- current_row + 1
    }
  }

  all_lines
}

#' Render cells for one header_above row
#' @noRd
.render_header_above_row_cells <- function(table, header_spec, neg_row) {
  row_cells <- character()

  # Build base style from header_spec (seq=0, lowest priority)
  base_style <- list()
  for (attr_name in c("bold", "italic", "color", "fill",
                       "font_size", "rotate", "inset", "stroke")) {
    val <- header_spec[[attr_name]]
    if (attr_name == "bold") val <- val %||% TRUE
    if (!is.null(val)) {
      base_style[[attr_name]] <- val
      base_style[[paste0(".seq_", attr_name)]] <- 0L
    }
  }
  # Map align -> cell_align for merge compatibility
  if (!is.null(header_spec$align)) {
    base_style$cell_align <- .to_typst_align(header_spec$align)
    base_style$.seq_cell_align <- 0L
  }

  # Merge with row-level override from tt_row()
  row_style <- table$row_styles[[as.character(neg_row)]]
  merged <- .merge_styles(base_style, row_style)

  cumspans <- cumsum(header_spec$header)

  for (i in seq_along(header_spec$header)) {
    span <- header_spec$header[i]
    label <- names(header_spec$header)[i]

    if (is.null(label) || label == "") {
      label <- ""
    }

    # Compute start_col for this group (used as cell key)
    start_col <- if (i == 1) 1L else cumspans[i - 1] + 1L

    # Merge with cell-level override from tt_cell()
    cell_key <- paste0(neg_row, "_", start_col)
    cell_style <- table$cell_styles[[cell_key]]
    final <- .merge_styles(merged, cell_style)

    # Handle content override
    if (!is.null(final[["content"]])) {
      label <- final[["content"]]
    }

    # Escape if needed
    if (table$escape && label != "") {
      label <- .escape_typst(label)
    }

    content <- .format_text(label,
      bold = final[["bold"]],
      italic = final[["italic"]],
      color = final[["color"]],
      size = final[["font_size"]],
      rotate = final[["rotate"]]
    )

    # Build cell arguments
    cell_args <- character()
    if (span > 1) {
      cell_args <- c(cell_args, paste0("colspan: ", span))
    }
    if (!is.null(final[["cell_align"]])) {
      cell_args <- c(cell_args, paste0("align: ", .to_typst_align(final[["cell_align"]])))
    }
    if (!is.null(final[["fill"]])) {
      cell_args <- c(cell_args, paste0("fill: ", .to_typst_color(final[["fill"]])))
    }
    if (!is.null(final[["inset"]])) {
      cell_args <- c(cell_args, paste0("inset: ", .to_typst_length(final[["inset"]])))
    }
    if (!is.null(final[["stroke"]])) {
      cell_args <- c(cell_args, paste0("stroke: ", .to_typst_stroke(final[["stroke"]])))
    }

    if (length(cell_args) > 0) {
      row_cells <- c(row_cells, paste0("table.cell(", paste(cell_args, collapse = ", "), ")[", content, "]"))
    } else {
      row_cells <- c(row_cells, paste0("[", content, "]"))
    }
  }

  row_cells
}

#' Render data rows
#' @noRd
.render_data_rows <- function(table) {
  rows <- character(table$nrow)

  # Get row groups for indentation
  row_groups <- .get_row_group_info(table)

  for (i in seq_len(table$nrow)) {
    row_cells <- character()

    # Get row styling
    row_style <- table$row_styles[[as.character(i)]]

    # Check for hline above this row (skip row 1 — handled as mid_rule)
    hline_above <- if (i == 1) NULL else .get_hline_at(table, i, "above")

    # Check if this row starts a group
    group_label <- row_groups$labels[i]

    for (j in seq_len(table$ncol)) {
      col_name <- table$display_cols[j]

      # Handle NA
      if (is.na(table$display_data[i, j])) {
        content <- table$na_string
      }
      else {
        content <- as.character(table$display_data[i, j])
      }

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

      row_cells <- c(row_cells, .render_cell(content, style, table, row = i, col = j))
    }

    # Combine cells for this row
    row_str <- paste(row_cells, collapse = ", ")

    # Prepend group label row if needed
    if (!is.null(group_label) && !is.na(group_label) && group_label != "") {
      group_style <- row_groups$group_styles[[i]]
      group_row <- .render_group_label_row(table, group_label, group_style)

      # Add hline below group label if requested
      if (!is.null(group_style$hline_below)) {
        hline_stroke <- if (isTRUE(group_style$hline_below)) NULL else group_style$hline_below
        if (!is.null(hline_stroke)) {
          group_row <- paste(group_row, paste0("table.hline(stroke: ", .to_typst_stroke(hline_stroke), ")"), sep = ",\n  ")
        } else {
          group_row <- paste(group_row, "table.hline()", sep = ",\n  ")
        }
      }

      row_str <- paste(group_row, row_str, sep = ",\n  ")
    }

    # Prepend hline if needed
    if (!is.null(hline_above)) {
      row_str <- paste(hline_above, row_str, sep = ",\n  ")
    }

    rows[i] <- row_str

    # Check for hline below this row (skip last row — handled as bottom_rule)
    if (i < table$nrow) {
      hline_below <- .get_hline_at(table, i, "below")
      if (!is.null(hline_below)) {
        rows[i] <- paste(rows[i], hline_below, sep = ",\n  ")
      }
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
  real_attrs <- names(style)[!startsWith(names(style), ".seq_")]
  if (length(real_attrs) == 0) {
    return(paste0("[", content, "]"))
  }

  # Check if we need table.cell wrapper (for fill, colspan, rowspan, align)
  needs_wrapper <- !is.null(style[["fill"]]) ||
                   !is.null(style[["colspan"]]) && style[["colspan"]] > 1 ||
                   !is.null(style[["rowspan"]]) && style[["rowspan"]] > 1 ||
                   !is.null(style[["cell_align"]]) ||
                   !is.null(style[["inset"]]) ||
                   !is.null(style[["stroke"]])

  # Apply text formatting
  formatted <- content
  if (!is.null(style[["content"]])) {
    # Override content
    formatted <- style[["content"]]
    if (table$escape) {
      formatted <- .escape_typst(formatted)
    }
  }

  formatted <- .format_text(
    formatted,
    bold = style[["bold"]],
    italic = style[["italic"]],
    color = style[["color"]],
    size = style[["font_size"]],
    rotate = style[["rotate"]]
  )

  if (needs_wrapper) {
    cell_args <- character()

    if (!is.null(style[["colspan"]]) && style[["colspan"]] > 1) {
      cell_args <- c(cell_args, paste0("colspan: ", style[["colspan"]]))
    }
    if (!is.null(style[["rowspan"]]) && style[["rowspan"]] > 1) {
      cell_args <- c(cell_args, paste0("rowspan: ", style[["rowspan"]]))
    }
    if (!is.null(style[["cell_align"]])) {
      cell_args <- c(cell_args, paste0("align: ", .to_typst_align(style[["cell_align"]])))
    }
    if (!is.null(style[["fill"]])) {
      cell_args <- c(cell_args, paste0("fill: ", .to_typst_color(style[["fill"]])))
    }
    if (!is.null(style[["inset"]])) {
      cell_args <- c(cell_args, paste0("inset: ", .to_typst_length(style[["inset"]])))
    }
    if (!is.null(style[["stroke"]])) {
      cell_args <- c(cell_args, paste0("stroke: ", .to_typst_stroke(style[["stroke"]])))
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

  for (attr in c("fill", "color", "bold", "italic", "font_size", "rotate", "inset", "stroke")) {
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
          # Propagate sequence from *_col to resolved attribute
          col_seq_key <- paste0(".seq_", attr, "_col")
          if (!is.null(resolved[[col_seq_key]])) {
            resolved[[paste0(".seq_", attr)]] <- resolved[[col_seq_key]]
          }
        }
      }
    }
  }

  resolved
}

#' Merge two style lists using last-write-wins sequence numbers
#' @noRd
.merge_styles <- function(base, override) {
  if (is.null(base)) return(override)
  if (is.null(override)) return(base)

  result <- base
  for (name in names(override)) {
    if (startsWith(name, ".seq_")) next
    if (is.null(override[[name]])) next
    seq_key <- paste0(".seq_", name)
    override_seq <- override[[seq_key]] %||% 0L
    base_seq <- result[[seq_key]] %||% 0L
    if (override_seq >= base_seq) {
      result[[name]] <- override[[name]]
      result[[seq_key]] <- override_seq
    }
  }
  result
}

#' Render horizontal lines
#' @noRd
.render_hlines <- function(table) {
  result <- list(after_header = NULL)
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
      args <- c(args, paste0("end: ", vline$end + 1))
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
  group_styles <- vector("list", table$nrow)

  for (group in table$row_groups) {
    if (!is.null(group$start_row) && group$start_row <= table$nrow) {
      labels[group$start_row] <- group$group_label
      group_styles[[group$start_row]] <- group

      # Indent rows in group
      if (isTRUE(group$indent)) {
        end_row <- min(group$end_row %||% table$nrow, table$nrow)
        indent[group$start_row:end_row] <- TRUE
      }
    }
  }

  list(labels = labels, indent = indent, group_styles = group_styles)
}

#' Render group label row
#' @noRd
.render_group_label_row <- function(table, label, group_style) {
  if (table$escape) {
    label <- .escape_typst(label)
  }

  bold <- group_style$bold %||% TRUE
  formatted <- .format_text(
    label,
    bold = bold,
    italic = group_style$italic,
    color = group_style$color,
    size = group_style$font_size,
    rotate = group_style$rotate
  )

  cell_args <- paste0("colspan: ", table$ncol)

  if (!is.null(group_style$align)) {
    cell_args <- c(cell_args, paste0("align: ", .to_typst_align(group_style$align)))
  }
  if (!is.null(group_style$fill)) {
    cell_args <- c(cell_args, paste0("fill: ", .to_typst_color(group_style$fill)))
  }
  if (!is.null(group_style$inset)) {
    cell_args <- c(cell_args, paste0("inset: ", .to_typst_length(group_style$inset)))
  }
  if (!is.null(group_style$stroke)) {
    cell_args <- c(cell_args, paste0("stroke: ", .to_typst_stroke(group_style$stroke)))
  }

  paste0("table.cell(", paste(cell_args, collapse = ", "), ")[", formatted, "]")
}

#' Get hline at position
#' @noRd
.get_hline_at <- function(table, row, position = "above") {
  for (i in rev(seq_along(table$hlines))) {
    hline <- table$hlines[[i]]
    if ((position == "above" && hline$y == row) ||
        (position == "below" && hline$y == row + 1)) {
      args <- character()

      if (!is.null(hline$start)) {
        args <- c(args, paste0("start: ", hline$start))
      }
      if (!is.null(hline$end)) {
        args <- c(args, paste0("end: ", hline$end + 1))
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

#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
