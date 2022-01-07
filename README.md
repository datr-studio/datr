
<!-- README.md is generated from README.Rmd. Please edit that file -->

# commonr

<!-- badges: start -->
<!-- badges: end -->

The goal of commonr is to provide a set of common functions that appear
in pretty much all R projects.

## Installation

You can install the development version of commonr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("firthj/commonr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(commonr)

## Source all R files in a folder, rather than one by one:
source_folder("R")
#> 1 R file in R was sourced.
```
