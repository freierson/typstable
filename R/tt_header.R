#' Add grouped header row above column names
#'
#' Adds a spanning header row above the existing column names to group related
#' columns together.
#'
#' @param table A `typst_table` object.
#' @param header Named vector specifying header groups. Names are the group labels,
#'   values are the number of columns each group spans. Use empty string `""` for
#'   columns without a group header.
#' @param bold Logical. Make header text bold (default TRUE).
#' @param align Header alignment (default "center").
#' @param color Text color.
#' @param background Background fill color.
#' @param line Logical. Add horizontal line below the header (default TRUE).
#'
#' @return The modified `typst_table` object.
#'
#' @examples
#' # Group columns under headers
#' tt(mtcars[1:5, 1:6]) |>
#'   tt_header_above(c("Performance" = 3, "Design" = 3))
#'
#' # Leave some columns ungrouped
#' tt(mtcars[1:5, 1:6]) |>
#'   tt_header_above(c(" " = 1, "Group A" = 2, "Group B" = 3))
#'
#' @export
tt_header_above <- function(table,
                            header,
                            bold = TRUE,
                            align = "center",
                            color = NULL,
                            background = NULL,
                            line = TRUE) {
  .check_typst_table(table)
  table <- .copy_table(table)

  if (missing(header) || length(header) == 0) {
    rlang::abort("`header` must be a named vector specifying column spans")
  }

  # Validate that spans sum to number of columns
  total_span <- sum(header)
  if (total_span != table$ncol) {
    rlang::abort(paste0(
      "Header spans must sum to ", table$ncol, " (number of columns), got ", total_span
    ))
  }

  # Ensure all values are named
  if (is.null(names(header))) {
    rlang::abort("`header` must be a named vector (e.g., c('Group A' = 2, 'Group B' = 3))")
  }

  # Store header specification
  header_spec <- list(
    header = header,
    bold = bold,
    align = align,
    color = color,
    background = background,
    line = line
  )

  # Prepend to headers_above (most recent goes first)
  table$headers_above <- c(list(header_spec), table$headers_above)

  table
}
