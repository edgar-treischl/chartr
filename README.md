
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chartr

<!-- badges: start -->

<!-- badges: end -->

The goal of chartr is to …

Helper

I am building a reusable `ggplot2` component in R that should be
pipe-friendly (`data |> my_component(...)`), fully namespaced
(`ggplot2::`, `dplyr::`, `rlang::`), and flexible for aesthetics,
grouping, faceting, labels, stacking, orientation, colors, or theme
options. The component should work on raw data (summarize/transform
internally if needed), provide sensible defaults, fail clearly if
inputs/columns are missing, and allow customization of legends, facets,
labels, colors, and theme settings. Ensure all parameter combinations
either work or fail clearly, support expressions like `as.factor(...)`,
and keep code clean, modular, and readable.

## Installation

You can install the development version of chartr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("edgar-treischl/chartr")
```
