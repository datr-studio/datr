
#' Load Clean/Raw get your data smartly
#'
#' These functions assume that your project includes a data/clean and data/raw directory.
#' They can handle file types of .csv, .tsv, .fe, & excel.
#'
#' @name loaders
#'
#' @param filename Name of filename to extract. File extensions are unneccesary and will be inferred. Use only if filenames are ambiguous.
#'
#' @return A tibble.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #' load_clean("cleanfile")
#' load_clean("subfolder/to/cleansubfile")
#' load_raw("rawfile")
#' }
load_clean <- function(filename) {
  load_file(file.path("data", "clean", filename))
}


#' @export
#' @rdname loaders
load_raw <- function(filename) {
  load_file(file.path("data", "raw", filename))
}

#' @importFrom stringr str_detect
#' @importFrom tools file_ext
#' @import cli
load_file <- function(filename, ...) {
  root_path <- Sys.getenv("ROOT_PATH")
  project <- Sys.getenv("PROJECT_NAME")
  filepath <- ifelse(root_path != "" && project != "",
    file.path(root_path, project, filename),
    filename
  )
  has_ext <- stringr::str_detect(filepath, "\\.")
  if (has_ext) {
    ext <- tools::file_ext(filepath)
  } else {
    ext <- find_ext(filepath)
    filepath <- paste0(filepath, ".", ext)
  }
  reader <- get_reader(ext)
  df <- reader(filepath, ...)
  cli::cli_alert_success("Loaded {filepath}")
  df
}

#' @import feather
#' @import readxl
#' @import vroom
get_reader <- function(ext) {
  switch(ext,
    "csv" = function(f, ...) vroom::vroom(f, show_col_types = FALSE, ...),
    "tsv" = function(f, ...) vroom::vroom(f, show_col_types = FALSE, ...),
    "xlsx" = function(f, ...) readxl::read_excel(f, ...),
    "xlsm" = function(f, ...) readxl::read_excel(f, ...),
    "fe" = function(f, ...) feather::read_feather(f, ...)
  )
}


#' @importFrom stringr str_extract str_remove
#' @importFrom tools file_ext
find_ext <- function(full_filename) {
  pattern <- "[a-z]([a-z]+|[a-z]+_[a-z]+|[a-z]+-[a-z]+)$"
  filename <- stringr::str_extract(full_filename, pattern)
  leading_dirs <- stringr::str_remove(full_filename, filename)
  f <- list.files(file.path(leading_dirs), pattern = paste0("^", filename))
  if (length(f) > 1) {
    stop("Filename is ambiguous. Use a file extension when more than one file share the same name.", call. = FALSE)
  } else if (length(f) == 0) {
    stop("Cannot find a file with the name ", filename, " in ", leading_dirs, call. = FALSE)
  }
  tools::file_ext(f)
}