# Save Data ---------------------------------------------------------------
#'
#'
#'

#' Save Raw Data
#'
#' Registers raw data into datrstudio project and saves to file structure.
#'
#' @param filepath Path to file to add.
#' @param source Source name.
#'
#' @importFrom tools file_ext
#' @export
save_raw <- function(filepath, version, source) {
  check_type(filepath, "character")
  check_type(source, "character")

  version <- as.character(version)
  if (!length(version) == 1) abort_incorrect_version(version)

  check_exists(filepath)
  if (is_dir(filepath)) {
    warn_skipping_dir(filepath)
  } else {
    name <- standardise_filename(rem_ext(basename(filepath)))
    name <- paste0(name, "-", standardise_filename(version))
    ext <- tools::file_ext(filepath)
    append_to_register("raw", name, version, source, ext)
    organise_file_tree("raw")
    move_to_raw(filepath, name, source)
    cli::cli_alert_success("{.val {name}} has been saved to raw data.")
  }
}


#' @export
save_raw_folder <- function(folder, version, source) {
  check_type(folder, "character")
  check_type(source, "character")

  version <- as.character(version)
  if (!length(version) == 1) abort_incorrect_version(version)
  if (!is_dir(folder)) abort_folder_not_found(folder)
  files <- list.files(folder, full.names = TRUE, recursive = TRUE)
  purrr::walk(files, ~ save_raw(.x, version, source))
}

#' Save Tidy
#'
#' Save a tidy copy of your data according to the datrstudio data management system.
#'
#' @param data A data.frame
#' @param name Name of the data frame
#' @param source Name of the source for reference. Can be inferred from a column called `source` if there is one.
#'
#' @export
#'
save_tidy <- function(data, name, version, source = NULL) {
  # Arg checking
  check_type(data, "data.frame")
  check_type(name, "character")
  version <- as.character(version)
  if (!length(version) == 1) abort_incorrect_version(version)
  source <- source %||% check_source(data, name)
  name <- standardise_filename(name)
  name <- paste0(name, "-", standardise_filename(version))
  append_to_register("tidy", name, version, source, "fe")
  organise_file_tree("tidy")
  source_dir <- standardise_filename(source)
  filepath <- file.path(get_root(), "data", "tidy", source_dir, paste0(name, ".fe"))
  feather::write_feather(data, filepath)
  cli::cli_alert_success("{.val {name}} saved to tidy data.")
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

#' @export
replace_raw <- function(filepath, name) {
  if (!is_registered(name, "raw")) abort_unregistered(name, "raw")
  metadata <- get_metadata_raw(name)
  deregister_raw(name)
  save_raw(filepath, name, metadata$version, metadata$source)
}

#' @export
replace_tidy <- function(data, name) {
  if (!is_registered(name, "tidy")) abort_unregistered(name, "tidy")
  metadata <- get_metadata_tidy(name)
  deregister_tidy(name)
  save_tidy(data, name, metadata$version, metadata$source)
}




# Append to register ---------------------------------------------------------------
#' @import tibble
append_to_register <- function(reg_type, name, version, source, ext) {
  reg <- get_register(reg_type)
  if (name %in% reg$name) abort_non_unique_name(name, reg_type)
  reg %>%
    tibble::add_row(name = name, version = version, source = source, ext = ext) %>%
    update_register(reg_type)
}

#' Deregister data from data registry
#'
#' This function will delete the named file from the data register, and will
#' remove the file from disk as well (unless this is turned off)
#'
#' @param name Name of data file.
#' @param reg_type Data types to remove: raw, tidy, both.
#'
#' @importFrom dplyr filter
#'
#' @export
deregister <- function(name, reg_type) {
  if (!is_registered(name, reg_type)) abort_unregistered(name, reg_type)
  unlink(get_filepath(name, reg_type))
  get_register(reg_type) %>%
    dplyr::filter(.data$name != .env$name) %>%
    update_register(reg_type)
  cli::cli_alert_success("{.val {name}} has been removed from {reg_type} data.")
  organise_file_tree(reg_type)
}


# Loaders ---------------------------------------------------------------
#'
#'
#'


#' Load Tidy/Raw get your data smartly
#'
#' These functions assume that your data has been registered into the datrstudio
#' data management system. If not, do that first. They can handle file types of
#' .csv, .tsv, .fe, & excel.
#'
#' Note that the `load_raw_cells()` variant enables an excel type to be loaded
#'  as xlsx_cells for further manipulation (e.g. via the brilliant unpivotr package).
#'
#' @name loaders
#'
#' @param name Name of filename to extract. File extensions are unnecessary and
#' will be inferred. Use only if filenames are ambiguous.
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

#' @export
#' @rdname loaders
#' @importFrom tidyxl xlsx_cells
load_raw_cells <- function(name, ...) {
  if (!is_registered(name, "raw")) abort_unregistered(name, "raw")
  file <- get_register("raw") %>%
    dplyr::filter(.data$name == .env$name)
  source_dir <- get_source_dir(name, "raw")
  filepath <- file.path(
    get_root(), "data", "raw", source_dir, paste0(name, ".", file$ext)
  )
  tidyxl::xlsx_cells(filepath, ...)
}

#' @importFrom tools file_ext
#' @importFrom janitor clean_names
#' @import cli
load_file <- function(name, reg_type, ...) {
  file <- get_register(reg_type) %>%
    dplyr::filter(.data$name == .env$name)
  source_dir <- get_source_dir(name, reg_type)
  filepath <- file.path(
    get_root(), "data", reg_type, source_dir, paste0(name, ".", file$ext)
  )
  if (file$ext %in% .state$external_file_types) {
    open_external_file(filepath, file.ext)
  } else {
    reader <- get_reader(file$ext)
    df <- reader(filepath, ...) %>%
      janitor::clean_names()
    df$source <- file$source
    cli::cli_alert_success("Loaded {.path {name}} (Source: {file$source}).")
    df
  }
}


#' @import feather
#' @importFrom readxl read_excel
#' @importFrom vroom vroom
get_reader <- function(ext) {
  switch(ext,
    "fe" = function(f, ...) feather::read_feather(f, ...),
    "csv" = , # nolint
    "tsv" = function(f, ...) vroom::vroom(f, show_col_types = FALSE, ...),
    "xls" = , # nolint
    "xlsx" = , # nolint
    "xlsm" = function(f, ...) readxl::read_excel(f, ...),
    stop(abort_unsupported_file(ext))
  )
}


# Renaming Functions ---------------------------------------------------------------
#'
#'
#'



#' Rename a source
#'
#' Rename all sources in the register, applied to either raw, tidy, or both.
#'
#' @param original_name Original source name.
#' @param new_name New source name.
#' @param reg_type Register type. Options: raw, tidy, both.
#'
#' @export
rename_source <- function(original_name, new_name, reg_type) {
  reg_type <- match.arg(reg_type, c("raw", "tidy", "both"))
  apply_func_either_both(process_source_rename, reg_type, original_name, new_name)
  cli::cli_alert_success("{original_name} has been renamed to {.val {new_name}}.")
}

process_source_rename <- function(from, to, reg_type) {
  if (!is_valid_source(from, reg_type)) abort_no_such_source(from, reg_type)
  reg <- get_register(reg_type)
  reg$source <- ifelse(reg$source == from, to, reg$source)
  update_register(reg, reg_type)
  to <- standardise_filename(to)
  from <- standardise_filename(from)
  root <- get_root()
  from_path <- file.path(root, "data", reg_type, from)
  to_path <- file.path(root, "data", reg_type, to)
  rename_dir(from_path, to_path)
}



#' Rename data
#'
#' Rename a data file and keep the files organised.
#'
#' @param original_name Current name.
#' @param new_name Name to change to.
#' @param reg_type Which register to change it in. Either raw, tidy, or both.
#'
#' @export
rename_data <- function(original_name, new_name, reg_type) {
  reg_type <- match.arg(reg_type, c("raw", "tidy", "both"))
  apply_func_either_both(process_data_rename, reg_type, original_name, new_name)
  cli::cli_alert_success("{original_name} has been renamed to {.val {new_name}}.")
}

process_data_rename <- function(reg_type, from, to) {
  browser()
  if (!is_registered(from, "raw")) abort_unregistered(from)
  original_meta <- get_metadata(from, reg_type)
  source <- standardise_filename(original_meta$source)
  version <- standardise_filename(original_meta$version)
  to <- standardise_filename(str_rem(to, version))
  to <- paste0(to, "-", version)
  reg <- get_register("raw")
  reg$name <- ifelse(reg$name == from, to, reg$name)
  update_register(reg, reg_type)
  from <- paste0(from, ".", original_meta$ext)
  to <- paste0(to, ".", original_meta$ext)
  old_filename <- file.path(file.path(get_root(), "data", "raw", source, from))
  new_filename <- file.path(file.path(get_root(), "data", "raw", source, to))
  file.copy(old_filename, new_filename)
  unlink(old_filename)
}



# Path helpers ---------------------------------------------------------------
#'
standardise_filename <- function(x) {
  str_repl(x, " ", "-") %>%
    str_repl("_", "-") %>%
    tolower()
}

#' @importFrom purrr walk map_chr
organise_file_tree <- function(reg_type, source) {
  # Make dir for each source in register
  sources <- get_register(reg_type) %>%
    dplyr::pull(source) %>%
    unique()
  purrr::walk(sources, create_source_dir, reg_type)
  # Remove any dirs not associated with a source
  dirs <- list.dirs(file.path(get_root(), "data", reg_type), full.names = FALSE, recursive = FALSE)
  source_dirs <- purrr::map_chr(sources, standardise_filename)
  obsolete_dirs <- setdiff(dirs, source_dirs)
  purrr::walk(obsolete_dirs, remove_source_dir, reg_type)
}

create_source_dir <- function(source, reg_type) {
  source <- standardise_filename(source)
  path <- file.path(get_root(), "data", reg_type, source)
  if (!dir.exists(path)) dir.create(path)
}

remove_source_dir <- function(source, reg_type) {
  source <- standardise_filename(source)
  unlink(
    file.path(get_root(), "data", reg_type, source),
    recursive = TRUE, force = TRUE
  )
}

get_source_dir <- function(name, reg_type) {
  get_register(reg_type) %>%
    filter(.data$name == .env$name) %>%
    dplyr::pull(source) %>%
    standardise_filename()
}
#' @importFrom dplyr pull
get_ext <- function(name, reg_type) {
  get_register(reg_type) %>%
    filter(.data$name == .env$name) %>%
    dplyr::pull(ext)
}

move_to_raw <- function(from, name, source) {
  ext <- tools::file_ext(from)
  source <- standardise_filename(source)
  to <- file.path(get_root(), "data", "raw", source, paste0(name, ".", ext))
  file.copy(from, to)
  unlink(from)
}

is_registered <- function(name, reg_type) {
  name %in% get_register(reg_type)$name
}

is_valid_source <- function(source, reg_type) {
  source %in% get_register(reg_type)$source
}

get_metadata <- function(name, reg_type) {
  get_register(reg_type) %>%
    dplyr::filter(.data$name == .env$name)
}



open_external_file <- function(path, ext) {
  if (Sys.info()["sysname"] == "Windows") {
    stop("Opening external files is not yet implemented on Windows.")
  } else {
    system(paste("open", path))
  }
  cli::cli_alert_success("{.path {path}} has been opened with the system viewer.")
}