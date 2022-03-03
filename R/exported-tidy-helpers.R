#' Load utils folder
#'
#' `load_utils()` is another standardisation method from the datr library intended
#' to help keep data science projects organised and repeatable.
#'
#' It makes the assumption that there is a code/utils directory, which will then
#' be sourced.
#'
#' @export
load_utils <- function(...) {
  source_folder(file.path(get_root(), "R", "utils"), ...)
}


#' Get file path of tidy data
#'
#' Function returns the absolute path to a tidy data file.
#'
#' @param name Name of data.
#' @param reg_type Whether it is stored in raw or tidy.
#'
#' @return Character vector of absolute filepath.
#' @export
get_filepath <- function(name, reg_type) {
  metadata <- get_metadata(name, reg_type)
  source_dir <- get_source_dir(name, reg_type)
  filepath <- file.path(
    get_root(), "data", reg_type, source_dir, paste0(name, ".", metadata$ext)
  )
  filepath
}