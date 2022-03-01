# Operators ---------------------------------------------------------------
#'

`%+%` <- function(a, b) {
  if (is.character(a) && is.character(b)) {
    paste0(a, b)
  } else {
    stop("%+% requires two strings")
  }
}

`%||%` <- function(a, b) ifelse(!is.null(a), a, b)

# Regex ---------------------------------------------------------------
#'

strext <- function(x, y) regmatches(x, regexpr(y, x))

strepl <- function(x, y, z) gsub(y, z, x)

strem <- function(x, y) gsub(y, "", x)

strdetc <- function(x, y) grepl(y, x)

# Paths ---------------------------------------------------------------
#'

rem_ext <- function(x) {
  x <- sub("[.](gz|bz2|xz)$", "", x)
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}


#' @export
#' @import rlang
rstudio_stfu <- function(f, ...) {
  withCallingHandlers(
    warning = function(cnd) {
      if (str_detect(cnd$message, "Please install a newer")) rlang::cnd_muffle(cnd)
    },
    f(...)
  )
}

check_type <- function(arg, exp_type) {
  arg_name <- rlang::expr_text(rlang::enexpr(arg))
  if (!inherits(arg, exp_type)) {
    abort_arg_wrong_type(arg, arg_name, exp_type)
  }
}

check_exists <- function(filepath) {
  if (!file.exists(filepath)) abort_file_not_found(filepath)
}

is_dir <- function(path) dir.exists(path)

mkdir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    cli::cli_alert_success("Creating {.path {relative_path(path)}}")
  }
}

write_file <- function(content, path, open = TRUE) {
  con <- file(path)
  writeLines(content, con)
  close(con)
  cli::cli_alert_success("Writing {.path {relative_path(path)}}")
  if (open) open_file(path)
}


#' @importFrom rstudioapi navigateToFile
open_file <- function(path) rstudioapi::navigateToFile(path)

relative_path <- function(path) {
  strem(path, paste0(get_root(), "/"))
}

#' @importFrom  tools file_path_sans_ext
base_name <- function(path) tools::file_path_sans_ext(basename(path))

base_project_dir <- function(path) {
  sans_root <- strem(path, paste0(get_root(), "/"))
  sans_tail <- strem(sans_root, "\\/.+")
  sans_tail
}

base_subdir <- function(path) {
  sans_root <- strem(path, paste0(get_root(), "/"))
  sans_base <- strem(sans_root, paste0(base_project_dir(path), "/"))
  sans_tail <- strem(sans_base, "\\/.+")
  sans_tail
}

rm_trailing_slash <- function(x) strem(x, "\\/$")