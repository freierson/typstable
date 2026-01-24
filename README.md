
# [typstable](https://freierson.github.io/typstable/) <img src="" width="160" align="right" alt="hex sticker"/>

<div id="badges">

[![R-CMD-check](https://github.com/freierson/typstable/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/freierson/typstable/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/freierson/typstable/graph/badge.svg)](https://app.codecov.io/gh/freierson/typstable)

</div>

The purpose of typstable /ˈtaɪps.tə.bᵊl/ is to produce publication-ready
tables for Quarto documents targeting the Typst format. Typst is a
modern, open-source, markup-based typesetting system that provides an
alternative to LaTeX for rendering PDF documents. This package allows R
users to create Typst tables with elaborate formatting.

## Installation

``` r
pak::pak("freierson/typstable")
```

## Example

``` r
# Create table with data-driven styling
tbl <- tt(cars, cols = c(.rownames, all_of(style_cols))) |>
  tt_style(header_separate = TRUE) |>
  tt_header_above(c(" " = 1, "Performance" = 2, "Characteristics" = 4)) |>
  tt_column(-1, color = "color_{col}", background = "bg_{col}") |>
  tt_pack_rows(index = table(cars$brand)) |>
  tt_column(1, width = "20%")
```

<img src="man/figures/readme-example.svg" alt="" width="100%" />
