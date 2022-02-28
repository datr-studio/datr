#' Save Raw Data
#'
#' Registers raw data into datrstudio project and saves to file structure.
#'
#' @param filepath Path to file to add.
#' @param source Source name.
#'
#' @import tools vroom
#' @export
save_raw <- function(filepath, source) {
  check_type(filepath, "character")
  check_type(source, "character")
  check_exists(filepath)
  name <- standardise_filename(tools::file_path_sans_ext(basename(filepath)))
  ext <- tools::file_ext(filepath)
  append_to_register_raw(name, source, ext)
  organise_file_tree("raw")
  move_to_raw(filepath, name, source)
}

# Append to register ---------------------------------------------------------------
#' @import tibble
append_to_register_raw <- function(name, source, ext) {
  reg <- get_register("raw")
  if (name %in% reg$name) abort_non_unique_name(name, reg_type)
  reg %>%
    tibble::add_row(name = name, source = source, ext = ext) %>%
    update_register("raw")
}

append_to_register_tidy <- function(name, source) {
  reg <- get_register("tidy")
  if (name %in% reg$name) abort_non_unique_name(name, reg_type)
  reg %>%
    tibble::add_row(name = name, source = source) %>%
    update_register("tidy")
}

#' Deregister data from data registry
#'
#' This function will delete the named file from the data register, and will remove the file from disk as well (unless this is turned off)
#'
#' @param name Name of data file.
#' @param rm Logical. Whether to remove the file as well.
#'
#' @import dplyr
#'
#' @export
deregister <- function(name, rm = TRUE) {
  deregister_raw(name)
  deregister_tidy(name)
  organise_file_tree("raw")
  organise_file_tree("tidy")
}

deregister_raw <- function(x) {
  get_register("raw") %>%
    dplyr::filter(name != x) %>%
    update_register("raw")
}

deregister_tidy <- function(x) {
  get_register("tidy") %>%
    dplyr::filter(name != x) %>%
    update_register("tidy")
}

# Path helpers ---------------------------------------------------------------
#'
standardise_filename <- function(x) {
  strepl(x, " ", "-") %>%
    strepl("_", "-") %>%
    tolower()
}

#' @importFrom purrr walk map_chr
organise_file_tree <- function(reg_type, source) {
  # Make dir for each source in register
  sources <- get_register(reg_type) %>%
    pull(source) %>%
    unique()
  purrr::walk(sources, create_source_dir, reg_type)
  # Remove any dirs not associated with a source
  dirs <- list.dirs(file.path(get_root_dir(), "data", reg_type), full.names = FALSE, recursive = FALSE)
  source_dirs <- purrr::map_chr(sources, standardise_filename)
  obsolete_dirs <- setdiff(dirs, source_dirs)
  purrr::walk(obsolete_dirs, remove_source_dir, reg_type)
}

create_source_dir <- function(source, reg_type) {
  source <- standardise_filename(source)
  dirpath <- file.path(get_root_dir(), "data", reg_type, source)
  if (!dir.exists(dirpath)) dir.create(dirpath)
}

remove_source_dir <- function(source, reg_type) {
  source <- standardise_filename(source)
  unlink(
    file.path(get_root_dir(), "data", reg_type, source),
    recursive = TRUE, force = TRUE
  )
}

get_source_dir <- function(name, reg_type) {
  get_register(reg_type) %>%
    filter(.data$name == .env$name) %>%
    pull(source) %>%
    standardise_filename()
}

get_ext <- function(name, reg_type) {
  get_register(reg_type) %>%
    filter(.data$name == .env$name) %>%
    pull(ext)
}

move_to_raw <- function(from, name, source) {
  ext <- tools::file_ext(from)
  source <- standardise_filename(source)
  to <- file.path(get_root_dir(), "data", "raw", source, paste0(name, ".", ext))
  file.copy(from, to)
  unlink(from)
}

is_registered <- function(name, reg_type) {
  name %in% get_register(reg_type)$name
}


# Loaders ---------------------------------------------------------------
#'
#'
#'


#' Load Tidy/Raw get your data smartly
#'
#' These functions assume that your data has been registered into the datrstudio data management system. If not, do that first. They can handle file types of .csv, .tsv, .fe, & excel.
#'
#' @name loaders
#'
#' @param name Name of filename to extract. File extensions are unneccesary and will be inferred. Use only if filenames are ambiguous.
#'
#' @return A tibble.
#'
#' @export
load_tidy <- function(name, ...) {
  if (!is_registered(name, "tidy")) abort_unregistered(name, "tidy")
  load_file(name, "tidy")
}


#' @export
#' @rdname loaders
load_raw <- function(name, ...) {
  if (!is_registered(name, "raw")) abort_unregistered(name, "raw")
  load_file(name, "raw", ...)
}

#' @importFrom tools file_ext
#' @importFrom janitor clean_names
#' @import cli
load_file <- function(name, reg_type, ...) {
  file <- get_register(reg_type) %>%
    dplyr::filter(.data$name == .env$name)
  reader <- get_reader(file$ext)
  source_dir <- get_source_dir(name, reg_type)
  filepath <- file.path(
    get_root_dir(), "data", reg_type, source_dir, paste0(name, ".", file$ext)
  )
  df <- reader(filepath, ...) %>%
    janitor::clean_names()
  df$source <- file$source
  cli::cli_alert_success("Loaded {.path {name}} (source: {file$source}).")
  df
}


#' @import feather
#' @import readxl
#' @import vroom
get_reader <- function(ext) {
  switch(ext,
    "fe" = function(f, ...) feather::read_feather(f, ...),
    "csv" = ,
    "tsv" = function(f, ...) vroom::vroom(f, show_col_types = FALSE, ...),
    "xls" = ,
    "xlsx" = ,
    "xlsm" = function(f, ...) readxl::read_excel(f, ...),
    stop(abort_unsupported_file(ext))
  )
}


# Save Tidy ---------------------------------------------------------------
#'
#'
#'
save_tidy <- function(data, name, source = NULL, force = FALSE) {
  # Arg checking
  check_type(data, "data.frame")
  check_type(name, "character")
  source <- source %||% check_source(data, name)
  append_to_register_tidy(name, source)
  organise_file_tree("tidy")
  sourcedir <- standardise_filename(source)
  filepath <- file.path(get_root_dir(), "data", "tidy", sourcedir, paste0(name, ".fe"))
  feather::write_feather(data, filepath)
}

check_source <- function(x, name) {
  if ("source" %in% colnames(x)) {
    sources <- unique(x$source)
    if (length(sources) == 1) {
      source <- sources
    } else {
      abort_multiple_sources(name)
    }
  } else {
    abort_no_source(name)
  }
  source
}