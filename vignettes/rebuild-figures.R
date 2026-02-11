# Regenerate pre-built vignette figures from chunk labels in typstable.Rmd
# Run from the package root: source("vignettes/rebuild-figures.R")
# Requires the Typst CLI to be installed.

library(typstable)

if (!tt_typst_available()) {
  stop("Typst CLI is required to rebuild figures. Install from https://typst.app/")
}

fig_dir <- "vignettes/figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir)

table_width <- "6.5in"
rmd_file <- "vignettes/typstable.Rmd"

# Parse the Rmd to populate knitr::knit_code
knitr::pat_md()
invisible(knitr:::split_file(readLines(rmd_file)))

table_labels <- paste0("table-", 1:6)

# Evaluate table chunks in a shared environment so variables carry over
env <- new.env(parent = globalenv())

for (i in seq_along(table_labels)) {
  label <- table_labels[i]
  code <- knitr::knit_code$get(label)
  if (is.null(code)) {
    warning("Chunk '", label, "' not found in ", rmd_file)
    next
  }
  eval(parse(text = code), envir = env)
  f <- file.path(fig_dir, paste0("table-", i, ".svg"))
  tt_save(env$tbl, f, width = table_width)
  message("Saved ", f)
}

message("Done. Rebuilt ", length(list.files(fig_dir, pattern = "\\.svg$")), " figures.")
