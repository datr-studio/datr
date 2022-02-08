
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
load_clean <- function(filename, ...) {
  load_file(file.path("data", "clean", filename))
}


#' @export
#' @rdname loaders
load_raw <- function(filename, ...) {
  load_file(file.path("data", "raw", filename), ...)
}

#' @importFrom stringr str_detect
#' @importFrom tools file_ext
#' @importFrom janitor clean_names
#' @import cli
load_file <- function(filename, ...) {
  # Get filepaths
  filepath <- file.path(get_root_dir(), filename)
  has_ext <- stringr::str_detect(filepath, "\\.")
  if (has_ext) {
    ext <- tools::file_ext(filepath)
  } else {
    ext <- find_ext(filepath)
    filepath <- paste0(filepath, ".", ext)
  }
  # browser()
  tryCatch(
    unsupported_file_error = function(cnd) {
      cli::cli_abort(cnd$message, call = parent.env(rlang::current_env()))
    },
    reader <- get_reader(ext)
  )

  df <- reader(filepath, ...)
  df <- janitor::clean_names(df)
  cli::cli_alert_success("Loaded {.path {basename(filename)}.}")
  df
}


#' Save clean data in a standardised way
#'
#' This function standardises the saving of dataframes into the data/clean dir as a
#' feather file
#'
#' This group of functions assumes that you have a `ROOT_PATH` and `PROJECT_NAME` envir
#' var set. If these are not found, they'll try to find your data path but may fail.
#'
#' @param data A dataframe.
#' @param name Filename to save on disk. Can include subdirectories.
#' @param force Logical. Whether to prompt before overwrite.
#'
#' @export
#' @import feather
save_clean <- function(data, filename, force = FALSE) {
  # Arg checking
  check_type(data, "data.frame")
  check_type(filename, "character")

  # Make paths
  root_path <- find_root_data_path()
  full_filename <- paste0(filename, ".fe")
  file_path <- file.path(root_path, full_filename)
  short_path <- file.path("data", "clean", full_filename)

  # Confirm and create Sub Directory exists (if required)
  if (stringr::str_detect(filename, "\\/")) {
    subdirs <- stringr::str_extract(filename, "([a-zA-Z]+\\/)+")
    subdirs <- substr(subdirs, 1, nchar(subdirs) - 1)
    if (!dir.exists(file.path(root_path, subdirs))) {
      if (force || confirm_dir_create(file.path("data", "clean", subdirs))) {
        dir.create(file.path(root_path, subdirs), recursive = TRUE)
      }
    }
  }

  # Confirm overwrite (if required)
  if (!force && file.exists(file_path)) {
    confirm_overwrite(short_path)
  }

  # Write
  feather::write_feather(data, file_path)
  show_data_save_success(short_path)
  invisible(data)
}

#' @import feather
#' @import readxl
#' @import vroom
get_reader <- function(ext) {
  switch(ext,
    "fe" = function(f, ...) feather::read_feather(f, ...),
    "csv" = function(f, ...) vroom::vroom(f, show_col_types = FALSE, ...),
    "tsv" = function(f, ...) vroom::vroom(f, show_col_types = FALSE, ...),
    "xlsx" = function(f, ...) readxl::read_excel(f, ...),
    "xlsm" = function(f, ...) readxl::read_excel(f, ...),
    stop(unsupported_file_error(ext))
  )
}


#' @importFrom stringr str_extract str_remove
#' @importFrom tools file_ext
find_ext <- function(full_filename) {
  filename <- basename(full_filename)
  leading_dirs <- stringr::str_remove(full_filename, filename)
  f <- list.files(file.path(leading_dirs), pattern = paste0("^", filename))
  if (length(f) > 1) {
    show_ambiguous_filename()
  } else if (length(f) == 0) {
    show_file_not_found(filename, leading_dirs)
  }
  tools::file_ext(f)
}


find_root_data_path <- function() {
  root_path <- get_root_dir()
  if (is.null(root_path)) {
    cli::cli_alert_danger("Unable to save data. {.path data/clean} doesn't exist!")
    stop_quietly()
  } else {
    data_path <- file.path(root_path, "data", "clean")
  }

  data_path
}

#' Find data directory
#'
#' `get_root_dir()` recursively searches backwards through the working directories to find
#'  a directory named 'data/clean'. Will try up to 4 times before giving up.
#'
#' @export
#'
#' @return root path as character or NULL.
get_root_dir <- function() {
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  for (attempt in 1:4) {
    if (dir.exists("data/clean")) {
      return(getwd())
    } else {
      setwd("..")
    }
  }
  invisible(NULL)
}




#' Load utils folder
#'
#' `load_utils()` is another standardisation method from the datr library intended
#' to help keep data science projects organised and repeatable.
#'
#' It makes the assumption that there is a code/utils directory, which will then
#' be sourced.
#'
#' @export
load_utils <- function() {
  saucer::source_folder(file.path(get_root_dir(), "code", "utils"))
}