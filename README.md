
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `datr` is to create a reproducible, standardised workflow
for data science projects that solves the following issues:

-   Standardise folder setup to create consistent workflows.
-   Manage data associated with a project, including keep sources
    organised.
-   Standardise notebooks in function and form.
-   Provide a set of helper functions akin to the brilliant `usethis`
    package, but for data science rather than package development.

This package enforces a particular way of working that is, above all,
tidy and organised. Because of the structured organisation of files, a
set of helper functions can eliminate the need to reference paths for
data and source files.

Ultimately, this package’s primary aim is to ensure that projects work
easily and consistently, and that they continue to do over time.

## Installation

You can install the development version of datr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("datr-studio/datr")
```

## Getting Started

A basic tidy datr structure includes the following folders:

–data —-raw —-tidy –R DESCRIPTION

It also must have a DESCRIPTION file, like an R package.

To create these, simply load `datr` as you would a normal package, and
you will be guided through the prompts to set this up.

``` r
library(datr)
```

## Notebooks

Once your project is set up tidily, you can reload the `datr` package
and you’ll gain access to a variety of helpers. One of these is the
`use_nb()` function, which sets up a new notebook.

On your first usage, your project will be given a “notebooks” folder,
within which to store your new notebook. The function will also create a
new Rmd file with a file safe name based off the title, and fill it in
with some standardised setup code for each notebook.

Check out the help file for `use_nb()` to see more of its features.

``` r
use_nb("My New Notebook")
```

## Sourcing code

`datr` also includes `source_folder()`, which can source all of the R
files in a given folder (and optionally matching a regex pattern).

There are also more specific versions of this, which are useful if you
use common data science folder names within your project. Currently this
includes: “R”, “model”, “math”, “functions”, or “Utils” (the latter will
be found in the R folder).

``` r
source_folder("some_folder")
source_r() # source the R folder
source_model() # source the model folder
source_math() # source the math folder
source_utils() # source R/utils

# And with regex matching
source_r("helpers") # only files in R containing 'helpers' in their filename
```

## Managing data

`datr` also includes an extensive system for managing data. More about
this can be found in the help files, but the essence is that this
ensures that:

-   data is always registered in a central database
-   data always includes a source and version
-   data is distinguished between raw and tidy

A key advantage to this system is the removal of the need for absolute
or relative file paths.

``` r
# instead of this...
read_csv("path/to/my/data/datafile1.csv")

# use this to import it
save_raw("path/to/my/data/datafile.csv", version = 1, source = "My Source")

# and thereafter this to reference it
load_raw("datafile-1")

# Once it has been cleaned, it can then be saved as a tidy data frame
load_raw("datafile-1") %>%
  mutate(x = "some fancy stuff") %>%
  save_tidy("datafile", version = 1.1)

# Thereafter the tidy version is now also available
load_tidy("datafile-1")
```

In addition, a wide variety of raw data files are able to be managed,
including:

-   csv
-   tsv
-   feather
-   excel types
-   excel cells using `tidyxl`
-   pdf
-   yaml

The database of data can also be searched, to help you remember what
you’ve got:

``` r
search_data("datafile")
```

## Using tests

Finally, `datr` supports the use of test-driven development by adapting
the very useful `testthat` package for use in data science projects.

``` r
use_tests() # set up the test folder

test() # run tests
```
