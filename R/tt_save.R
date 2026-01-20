#' Save a typst_table to SVG, PNG, PDF, or Typst source
#'
#' Exports a `typst_table` object to various output formats. For SVG, PNG, and PDF
#' output, the Typst CLI must be installed and available on the system PATH.
#'
#' @param table A `typst_table` object created with `tt()`.
#' @param file Output file path. The format is determined by the file extension:
#'   `.svg`, `.png`, `.pdf`, or `.typ` (Typst source).
#' @param width Page width. Use `"auto"` (default) for automatic sizing based on
#'   table content, or specify a Typst length like `"6in"`, `"15cm"`, `"400pt"`.
#' @param height Page height. Use `"auto"` (default) for automatic sizing, or
#'   specify a Typst length.
#' @param margin Page margin around the table. Default is `"0.5em"`. Use `"0pt"`
#'   for no margin.
#' @param ppi Pixels per inch for PNG output. Default is 144. Higher values
#'   produce larger, higher-resolution images.
#' @param typst_path Path to the Typst executable. If `NULL` (default), searches
#'   for `typst` on the system PATH.
#' @param overwrite Logical. If `TRUE` (default), overwrites existing files.
#'   If `FALSE`, throws an error when the output file already exists.
#'
#' @return Invisibly returns the output file path.
#'
#' @details
#' For `.typ` output, only the Typst source code is written and the Typst CLI
#' is not required.
#'
#' For `.svg`, `.png`, and `.pdf` output, the function:
#' 1
#' 1. Generates a complete Typst document with appropriate page settings
#' 2. Writes it to a temporary `.typ` file
#' 3. Calls `typst compile` to render the output
#' 4. Cleans up the temporary file
#'
#' The `width` and `height` parameters control the page size. Using `"auto"`
#' for both (the default
#' for both (the default) creates a page that fits the table content exactly.
#'
#' @examples
#' \dontrun{
#' # Save as SVG (requires Typst CLI)
#' tt(mtcars[1:5, 1:3]) |> tt_save("table.svg")
#'
#' # Save as PNG with higher resolution
#' tt(mtcars[1:5, 1:3]) |> tt_save("table.png", ppi = 300)
#'
#' # Save as PDF with specific page size
#' tt(mtcars[1:5, 1:3]) |>
#'   tt_style(stroke = TRUE) |>
#'   tt_save("table.pdf", width = "6in", height = "4in")
#'
#' # Save Typst source (no CLI required)
#' tt(mtcars[1:5, 1:3]) |> tt_save("table.typ")
#' }
#'
#' @export
tt_save <- function(table, file, width = "auto", height = "auto",
                    margin = "0.5em", ppi = 144, typst_path = NULL,
                    overwrite = TRUE) {
  # Validate inputs
  .check_typst_table(table)

  if (!is.character(file) || length(file) != 1 || nchar(file) == 0) {
    rlang::abort("`file` must be a non-empty character string")
  }

  if (!is.numeric(ppi) || length(ppi) != 1 || ppi <= 0) {
    rlang::abort("`ppi` must be a positive number")
  }

  if (!is.logical(overwrite) || length(overwrite) != 1) {
    rlang::abort("`overwrite` must be TRUE or FALSE")
  }

  # Check if file exists
  if (!overwrite && file.exists(file)) {
    rlang::abort(paste0("File '", file, "' already exists. Use `overwrite = TRUE` to overwrite."))
  }

  # Determine output format from extension
  ext <- tolower(tools::file_ext(file))
  valid_exts <- c("svg", "png", "pdf", "typ")

  if (!ext %in% valid_exts) {
    rlang::abort(paste0(
      "Unsupported file extension '.", ext, "'. ",
      "Supported formats: .svg, .png, .pdf, .typ"
    ))
  }

  # Generate Typst code
  typst_code <- tt_render(table)

  # For .typ output, just write the source

  if (ext == "typ") {
    writeLines(typst_code, file)
    return(invisible(file))
  }

  # For rendered output, we need the Typst CLI
  typst_exe <- .find_typst(typst_path)
  if (is.null(typst_exe)) {
    rlang::abort(c(
      "Typst CLI not found",
      i = "Install Typst from https://typst.app/ or https://github.com/typst/typst",
      i = "Or specify the path with `typst_path` argument"
    ))
  }

  # Create a complete Typst document with page settings
  doc <- .create_typst_document(typst_code, width, height, margin)

  # Write to temp file
  temp_typ <- tempfile(fileext = ".typ")
  on.exit(unlink(temp_typ), add = TRUE)
  writeLines(doc, temp_typ)

  # Build typst compile arguments
  args <- c("compile")

  # Add format-specific options
  if (ext == "png") {
    args <- c(args, "--format", "png", "--ppi", as.character(ppi))
  } else if (ext == "svg") {
    args <- c(args, "--format", "svg")
  } else if (ext == "pdf") {
    args <- c(args, "--format", "pdf")
  }

  # Input and output files
  args <- c(args, temp_typ, file)

  # Run typst compile
  result <- system2(typst_exe, args, stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status")

  if (!is.null(status) && status != 0) {
    rlang::abort(c(
      "Typst compilation failed",
      i = paste(result, collapse = "\n")
    ))
  }

  invisible(file)
}

#' Check if Typst CLI is available
#'
#' Tests whether the Typst command-line interface is installed and accessible.
#'
#' @param typst_path Optional path to the Typst executable. If `NULL`, searches
#'   for `typst` on the system PATH.
#'
#' @return `TRUE` if Typst is available, `FALSE` otherwise.
#'
#' @examples
#' # Check if Typst is installed
#' if (tt_typst_available()) {
#'   message("Typst is available")
#' } else {
#'   message("Typst not found - install from https://typst.app/")
#' }
#'
#' @export
tt_typst_available <- function(typst_path = NULL) {
  !is.null(.find_typst(typst_path))
}

#' Find the Typst executable
#'
#' @param typst_path Optional explicit path to typst
#' @return Path to typst executable, or NULL if not found
#' @noRd
.find_typst <- function(typst_path = NULL) {
  # If explicit path provided, check it exists

if (!is.null(typst_path)) {
    if (file.exists(typst_path)) {
      return(typst_path)
    }
    return(NULL)
  }

  # Search PATH for typst
  typst <- Sys.which("typst")
  if (nchar(typst) > 0) {
    return(typst)
  }

  NULL
}

#' Create a complete Typst document with page settings
#'
#' @param table_code The rendered table code
#' @param width Page width
#' @param height Page height
#' @param margin Page margin
#' @return Complete Typst document as character string
#' @noRd
.create_typst_document <- function(table_code, width, height, margin) {
  # Build page settings
  page_args <- character()

  if (width != "auto") {
    page_args <- c(page_args, paste0("width: ", width))
  } else {
    page_args <- c(page_args, "width: auto")
  }

  if (height != "auto") {
    page_args <- c(page_args, paste0("height: ", height))
  } else {
    page_args <- c(page_args, "height: auto")
  }

  page_args <- c(page_args, paste0("margin: ", margin))

  # Create document
  paste0(
    "#set page(", paste(page_args, collapse = ", "), ")\n\n",
    table_code
  )
}
