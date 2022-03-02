#' @export
use_desc <- function(proj_name, path = ".") {
  get_template("DESCRIPTION") %>%
    replace_line(1, paste0("Package: ", proj_name)) %>%
    write_file(file.path(path, "DESCRIPTION"))
}

#' @export
use_tidy_data <- function(path = ".") {
  mkdir(file.path(path, "data", "raw"))
  mkdir(file.path(path, "data", "tidy"))
  reg <- tibble::tibble(
    name = character(),
    version = character(),
    source = character(),
    ext = character()
  )
  feather::write_feather(
    reg, file.path(path, "data", ".raw.fe")
  )
  feather::write_feather(
    reg, file.path(path, "data", ".tidy.fe")
  )
}


#' @export
use_validation <- function(path = ".") {
  mkdir(file.path(path, "validation", "analysis"))
  mkdir(file.path(path, "validation", "outputs"))
  validate <- c("#!/usr/bin/env Rscript", "library(bladerunr)")
  write_file(validate, file.path(path, "validation", "validate.R"))
}

#' @export
use_notebooks <- function(path = ".") {
  mkdir(file.path(path, "notebooks", "EDA"))
}