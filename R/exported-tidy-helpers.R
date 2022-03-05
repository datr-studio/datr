#' Source Helpers
#'
#' `source_utils()`, `source_model()`, and `source_r()`. Under the hood, these
#' function use `source_folder()`.
#'
#' These functions assume that a tidy project structure according to datr principles.
#'
#' @name source_helpers
#'
#' @param subdir If required, a subdir can be specified.
#' @param ... Optional arguments to pass on to source_folder().


#' @rdname source_helpers
#' @export
source_utils <- function(subdir = "", ...) {
  source_helper(file.path("R", "utils"), subdir, ...)
}

#' @rdname source_helpers
#' @export
source_r <- function(subdir = "", ...) {
  source_helper(file.path("R"), subdir, ...)
}

#' @rdname source_helpers
#' @export
source_model <- function(subdir = "", ...) {
  source_helper(file.path("model"), subdir, ...)
}

source_helper <- function(dir, subdir, ...) {
  source_folder(file.path(get_root(), dir, subdir), ...)
}

load_as_root <- function(path) file.path()

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