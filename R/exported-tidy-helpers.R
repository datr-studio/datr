#' Source Helpers
#'
#' `source_xx()` are helper functions that assist with quickly sourcing the content of a
#' folder with a tidy project structure. Under the hood, these function use `source_folder()`.
#'
#' @name source_helpers
#'
#' @param subdir If required, a subdir can be specified.
#' @inheritDotParams source_folder


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

#' @rdname source_helpers
#' @export
source_math <- function(subdir = "", ...) {
  source_helper(file.path("math"), subdir, ...)
}


source_helper <- function(dir, subdir, ...) {
  subpath <- ifelse(nchar(subdir) > 0, file.path(dir, subdir), dir)
  source_folder(file.path(get_root(), subpath), ...)
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