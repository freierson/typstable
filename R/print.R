#' Print a typst_table object
#'
#' Prints the Typst markup for a table to the console.
#'
#' @param x A `typst_table` object.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the Typst code as a character string.
#'
#' @export
print.typst_table <- function(x, ...) {
  code <- tt_render(x)
  cat(code, "\n")
  invisible(code)
}

#' Knit print method for typst_table
#'
#' Renders a `typst_table` for use in Quarto/knitr documents with Typst output.
#'
#' @param x A `typst_table` object.
#' @param ... Additional arguments (ignored).
#'
#' @return A `knitr::asis_output` object containing the Typst markup.
#'
#' @keywords internal
knit_print.typst_table <- function(x, ...) {
  code <- tt_render(x)

 # Wrap in raw Typst block for Quarto/Pandoc
  raw_block <- paste0("\n```{=typst}\n", code, "\n```\n")

  knitr::asis_output(raw_block)
}

# Register S3 methods for knitr
.onLoad <- function(libname, pkgname) {
  # Register knit_print method if knitr is available
  if (requireNamespace("knitr", quietly = TRUE)) {
    # Use vctrs-style registration that works even when knitr isn't loaded yet
    register_s3_method <- function(pkg, generic, class, fun = NULL) {
      stopifnot(is.character(pkg), length(pkg) == 1)
      stopifnot(is.character(generic), length(generic) == 1)
      stopifnot(is.character(class), length(class) == 1)

      if (is.null(fun)) {
        fun <- get(paste0(generic, ".", class), envir = parent.frame())
      }
      stopifnot(is.function(fun))

      if (pkg %in% loadedNamespaces()) {
        registerS3method(generic, class, fun, envir = asNamespace(pkg))
      }

      # Always register a hook for when the package loads
      setHook(
        packageEvent(pkg, "onLoad"),
        function(...) {
          registerS3method(generic, class, fun, envir = asNamespace(pkg))
        }
      )
    }

    register_s3_method("knitr", "knit_print", "typst_table")
  }
}
