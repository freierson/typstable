#' Add footnotes to table
#'
#' Adds footnotes below the table. Supports general notes, numbered notes,
#' and symbol-marked notes.
#'
#' @param table A `typst_table` object.
#' @param general Character vector of general footnotes (no markers).
#' @param number Character vector of numbered footnotes (1, 2, 3...).
#' @param symbol Character vector of symbol footnotes (*, †, ‡...).
#'
#' @return The modified `typst_table` object.
#'
#' @examples
#' # Add a general note
#' tt(mtcars[1:5, 1:3]) |>
#'   tt_footnote(general = "Source: Motor Trend magazine, 1974.")
#'
#' # Add numbered footnotes
#' tt(mtcars[1:5, 1:3]) |>
#'   tt_footnote(number = c("Miles per gallon", "Number of cylinders"))
#'
#' @export
tt_footnote <- function(table,
                        general = NULL,
                        number = NULL,
                        symbol = NULL) {
  .check_typst_table(table)
  table <- .copy_table(table)

  if (!is.null(general)) {
    if (!is.character(general)) {
      rlang::abort("`general` must be a character vector")
    }
    table$footnotes$general <- c(table$footnotes$general, general)
  }

  if (!is.null(number)) {
    if (!is.character(number)) {
      rlang::abort("`number` must be a character vector")
    }
    table$footnotes$number <- c(table$footnotes$number, number)
  }

  if (!is.null(symbol)) {
    if (!is.character(symbol)) {
      rlang::abort("`symbol` must be a character vector")
    }
    table$footnotes$symbol <- c(table$footnotes$symbol, symbol)
  }

  table
}
