rm(list = ls())
library(testthat)

# Dependencies



# Run Tests


tests <- list.files("tests/testthat", pattern = ".\\.R")
walk(tests, ~ test_file(test_path(.x)))
