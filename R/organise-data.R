# Save Data ---------------------------------------------------------------
#'
#'
#'

#' Save Raw Data
#'
#' Registers raw data into project and saves to file structure.
#'
#' @param filepath Path to file to add.
#' @param source Source name.
#'
#' @importFrom tools file_ext
#' @export
save_raw <- function(filepath, source, url = NA_character_, force = FALSE, from_dls = TRUE) {
  check_type(filepath, "character")
  check_type(source, "character")

  if (from_dls) filepath <- from_dls(filepath)

  if (file_has_no_ext(filepath)) {
    basename <- basename(filepath)
    dir <- str_rem(filepath, basename)
    filepath <- guess_full_filename(basename, dir)
  }

  if (!file.exists(filepath)) {
    if (file.exists(from_dls(filepath))) filepath <- from_dls(filepath)
  }
  check_exists(filepath)
  if (is_dir(filepath)) {
    warn_skipping_dir(filepath)
  } else {
    name <- standardise_filename(rem_ext(basename(filepath)))
    ext <- tools::file_ext(filepath)
    append_to_register("raw", name, source, ext, url, force = force)
    organise_file_tree("raw")
    move_to_raw(filepath, name, source)
    cli::cli_alert_success("{.val {name}} has been saved to raw data.")
  }
}


#' @export
save_raw_folder <- function(folder, source, force = FALSE) {
  check_type(folder, "character")
  check_type(source, "character")

  if (!is_dir(folder)) abort_folder_not_found(folder)
  files <- list.files(folder, full.names = TRUE, recursive = TRUE)
  purrr::walk(files, ~ save_raw(.x, source))
}

#' Save Tidy
#'
#' Save a tidy copy of your data according to the data management system.
#'
#' @param data A data.frame
#' @param name Name of the data frame
#' @param source Name of the source for reference. Can be inferred from a column called `source` if there is one.
#' @param ext Filetype. Defaults to feather.
#' @param force If true, datr will replace existing data with the same name without prompting.
#'
#' @export
#'
save_tidy <- function(data, name, source = NULL, ext = "fe", force = FALSE) {
  # Arg checking
  check_type(data, "data.frame")
  check_type(name, "character")

  source <- source %||% check_source(data, name)
  name <- standardise_filename(name)
  append_to_register("tidy", name, source, ext, force = force)
  organise_file_tree("tidy")
  source_dir <- standardise_filename(source)
  filepath <- file.path(get_root(), "data", "tidy", source_dir, paste0(name, ".", ext))
  writer <- get_writer(ext)
  writer(data, filepath)
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



#' Replace a data frame in the raw register.
#'
#' Removes the registered data with the same name and replaces it with the specified file.
#'
#' @param data A data frame. Either this or a filename.
#' @param name Name of data file to replace.
#' @param filepath Path to replacement file (Must have either this or a dataframe).
#'
#' @export
update_raw <- function(data = NULL, name, filepath = NULL) {
  update_data(name, data, filepath, "raw")
}


#' Replace a data frame in the tidy register.
#'
#' UPdates the registered data with updated data
#'
#' @param data A data frame.
#' @param name Name of data file to replace.
#'
#' @export
update_tidy <- function(data, name) {
  update_data(name, data = data, type = "tidy")
}

update_data <- function(name, data = NULL, filepath = NULL, type) {
  if (!is_registered(name, type)) abort_unregistered(name, type)
  if (is.null(filepath) && is.null(data)) abort_no_data()
  metadata <- get_metadata(name, type)
  if (!is.null(filepath)) {
    check_exists(filepath)
    if (is_dir(filepath)) {
      warn_skipping_dir(filepath)
    } else {
      move_to_raw(filepath, name, metadata$source)
    }
  } else if (!is.null(data)) {
    check_type(data, "data.frame")
    ext <- metadata$ext
    writer <- get_writer(ext)
    filename <- file.path(
      get_root(), "data", type, metadata$source, paste0(name, ".", metadata$ext)
    )
    writer(data, filename)
  }

  cli::cli_alert_success("{.val {name}} has been updated.")
}




# Append to register ---------------------------------------------------------------
#' @import tibble
append_to_register <- function(reg_type, name, source, ext, url = NA_character_, force = FALSE) {
  reg <- get_register(reg_type)
  if (name %in% reg$name) {
    if (!force) {
      abort_non_unique_name(name, reg_type)
    } else {
      # Check that the sources match. If not, this is accidental.
      current_source <- reg |>
        dplyr::filter(.data$name == .env$name) |>
        dplyr::pull(source)
      if (current_source != source) {
        abort_non_unique_name_diff_sources(name, reg_type, current_source, source)
      }
      current_ext <- reg |>
        dplyr::filter(.data$name == .env$name) |>
        dplyr::pull(ext)
      if (current_ext != ext) {
        change_tidy_ext(name, ext)
        remove_old_version("tidy", name, source, current_ext)
      }
    }
  } else {
    if (reg_type == "raw") {
      reg |>
        tibble::add_row(name = name, source = source, ext = ext, url = url) |>
        update_register(reg_type)
    } else {
      reg |>
        tibble::add_row(name = name, source = source, ext = ext) |>
        update_register(reg_type)
    }
  }
}

#' remove data from data registry
#'
#' This function will delete the named file from the data register, and will
#' remove the file from disk as well (unless this is turned off)
#' @name deregister
#' @param name Name of data file.
#' @param reg_type Data types to remove: raw, tidy, both.
#'
#' @importFrom dplyr filter


remove_data <- function(name, reg_type) {
  if (!is_registered(name, reg_type)) {
    warn_unregistered(name, reg_type)
  } else {
    unlink(get_filepath(name, reg_type))
    get_register(reg_type) |>
      dplyr::filter(.data$name != .env$name) |>
      update_register(reg_type)
    cli::cli_alert_success("{.val {name}} has been removed from {reg_type} data.")
    organise_file_tree(reg_type)
  }
}

#' @rdname deregister
#' @export
remove_tidy <- function(name) remove_data(name, "tidy")

#' @rdname deregister
#' @export
remove_raw <- function(name) remove_data(name, "raw")

change_tidy_ext <- function(data_name, new_ext) {
  get_register("tidy") |>
    mutate(ext = ifelse(name == data_name, new_ext, ext)) |>
    update_register("tidy")
}

remove_old_version <- function(reg_type, name, source, ext) {
  unlink(file.path(get_root(), "data", reg_type, source, paste0(name, ".", ext)))
}

# Loaders ---------------------------------------------------------------
#'
#'
#'


#' Load Tidy/Raw get your data smartly
#'
#' These functions assume that your data has been registered into the datrstudio
#' data management system. If not, do that first. They can handle file types of
#' .csv, .tsv, .fe, yaml, & excel.
#'
#' Note that the `read_raw_cells()` variant enables an excel type to be loaded
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
read_tidy <- function(name, append_source = FALSE, ...) {
  if (!is_registered(name, "tidy")) abort_unregistered(name, "tidy")
  read_file(name, "tidy", append_source, ...)
}


#' @export
#' @rdname loaders
read_raw <- function(name, append_source = FALSE, ...) {
  if (!is_registered(name, "raw")) abort_unregistered(name, "raw")
  read_file(name, "raw", append_source, ...)
}

#' @export
#' @rdname loaders
#' @importFrom tidyxl xlsx_cells
read_raw_cells <- function(name, ...) {
  if (!is_registered(name, "raw")) abort_unregistered(name, "raw")
  file <- get_register("raw") |>
    dplyr::filter(.data$name == .env$name)
  source_dir <- get_source_dir(name, "raw")
  filepath <- file.path(
    get_root(), "data", "raw", source_dir, paste0(name, ".", file$ext)
  )
  tidyxl::xlsx_cells(filepath, ...)
}

#' @importFrom tools file_ext
#' @import cli
read_file <- function(name, reg_type, append_source, ...) {
  file <- get_register(reg_type) |>
    dplyr::filter(.data$name == .env$name)
  source_dir <- get_source_dir(name, reg_type)
  filepath <- file.path(
    get_root(), "data", reg_type, source_dir, paste0(name, ".", file$ext)
  )
  if (file$ext %in% .state$external_file_types) {
    open_external_file(filepath)
  } else {
    reader <- get_reader(file$ext)
    df <- reader(filepath, ...)
    if (inherits(df, "data.frame")) {
      if (append_source) df$source <- file$source
    }
    cli::cli_alert_success("Loaded {.path {name}} (Source: {file$source}).")
    df
  }
}


#' @import feather
#' @importFrom readxl read_excel
#' @importFrom vroom vroom
#' @importFrom yaml read_yaml
#' @importFrom rvest read_html
get_reader <- function(ext) {
  vroom <- if (Sys.info()[["sysname"]] == "Windows") {
    function(f, ...) {
      vroom::vroom(f,
        show_col_types = FALSE, altrep = use_altrep, ...
      )
    }
  } else {
    function(f, ...) {
      vroom::vroom(f,
        show_col_types = FALSE, ...
      )
    }
  }

  switch(ext,
    "fe" = function(f, ...) feather::read_feather(f, ...),
    "csv" = , # nolint,
    "dat" = , # nolint,
    "tsv" = function(f, ...) vroom::vroom(f, ...),
    "xls" = , # nolint
    "xlsx" = , # nolint
    "xlsm" = function(f, ...) readxl::read_excel(f, ...),
    "yaml" = , # nolint
    "yml" = function(f, ...) yaml::read_yaml(f, ...),
    "html" = function(f, ...) rvest::read_html(f, ...),
    stop(abort_unsupported_file(ext))
  )
}

#' @importFrom vroom vroom_write
#' @importFrom yaml write_yaml
get_writer <- function(ext) {
  switch(ext,
    "fe" = function(data, filename) feather::write_feather(data, filename),
    "csv" = function(data, filename) vroom::vroom_write(data, filename, delim = ","),
    "dat" = function(data, filename) vroom::vroom_write(data, filename, delim = "\t"),
    "tsv" = function(data, filename) vroom::vroom_write(data, filename, delim = "\t"),
    "yaml" = , # nolint
    "yml" = function(data, filename) yaml::write_yaml(data, filename),
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

process_source_rename <- function(reg_type, from, to) {
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
}

process_data_rename <- function(reg_type, from, to) {
  if (!is_registered(from, reg_type)) abort_unregistered(from, reg_type)
  original_meta <- get_metadata(from, reg_type)
  source <- standardise_filename(original_meta$source)
  reg <- get_register(reg_type)
  reg$name <- ifelse(reg$name == from, to, reg$name)
  update_register(reg, reg_type)
  from <- paste0(from, ".", original_meta$ext)
  to <- paste0(to, ".", original_meta$ext)
  old_filename <- file.path(file.path(get_root(), "data", reg_type, source, from))
  new_filename <- file.path(file.path(get_root(), "data", reg_type, source, to))
  file.copy(old_filename, new_filename)
  unlink(old_filename)
  cli::cli_alert_success("{from} has been renamed to {.val {to}}.")
}

# Open files externally ---------------------------------------------------------------
#'
#' @export
open_raw <- function(name) {
  open_data_with_system(name, "raw")
}

#' @export
open_config <- function(name) {
  dir <- file.path(get_root(), "model", "config")
  if (file_has_no_ext(name)) name <- guess_full_filename(name, dir)
  path <- file.path(dir, name)
  if (!file.exists(path)) abort_file_not_found(path)
  open_external_file(path)
}

open_data_with_system <- function(name, reg_type) {
  if (!is_registered(name, reg_type)) abort_unregistered(name, reg_type)
  meta <- get_metadata(name, reg_type)
  path <- file.path(
    get_root(), "data", reg_type, meta$source, paste0(name, ".", meta$ext)
  )
  open_external_file(path)
}



# Path helpers ---------------------------------------------------------------
#'
#' @export
standardise_filename <- function(x) {
  str_repl(x, " ", "-") |>
    str_repl("_", "-") |>
    tolower()
}

#' @importFrom purrr walk map_chr
organise_file_tree <- function(reg_type, source) {
  # Make dir for each source in register
  sources <- get_register(reg_type) |>
    dplyr::pull(source) |>
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
  get_register(reg_type) |>
    filter(.data$name == .env$name) |>
    dplyr::pull(source) |>
    standardise_filename()
}
#' @importFrom dplyr pull
get_ext <- function(name, reg_type) {
  get_register(reg_type) |>
    filter(.data$name == .env$name) |>
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
  get_register(reg_type) |>
    dplyr::filter(.data$name == .env$name)
}



open_external_file <- function(path) {
  if (Sys.info()["sysname"] == "Windows") {
    stop("Opening external files is not yet implemented on Windows.")
  } else {
    system(paste("open", path))
  }
  cli::cli_alert_success("{.path {path}} has been opened with the system viewer.")
}

#' @export
refresh_data_records <- function() {
    .state$raw <- load_register("raw")
    .state$tidy <- load_register("tidy")
}
