# Regex ---------------------------------------------------------------
#'

str_ext <- function(x, y) regmatches(x, regexpr(y, x))

str_repl <- function(x, y, z) gsub(y, z, x)

str_rem <- function(x, y) gsub(y, "", x)

in_str <- function(x, y) grepl(y, x)

# Paths ---------------------------------------------------------------
#'

rem_ext <- function(x) {
  x <- sub("[.](gz|bz2|xz)$", "", x)
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}


#' @importFrom rlang cnd_muffle
rstudio_stfu <- function(f, ...) {
  withCallingHandlers(
    warning = function(cnd) {
      if (str_detect(cnd$message, "Please install a newer")) rlang::cnd_muffle(cnd)
    },
    f(...)
  )
}

#' @importFrom rlang expr_text enexpr
check_type <- function(arg, exp_type) {
  arg_name <- rlang::expr_text(rlang::enexpr(arg))
  classes <- paste0(class(arg), collapse = " ")
  if (!in_str(classes, exp_type)) {
    abort_arg_wrong_type(arg, arg_name, exp_type)
  }
}

check_len <- function(arg, exp_len) {
  arg_name <- rlang::expr_text(rlang::enexpr(arg))
  if (!length(arg) == exp_len) abort_arg_wrong_len(arg, arg_name, exp_len)
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

rename_dir <- function(from, to) {
  if (dir.exists(from)) {
    if (!dir.exists(to)) dir.create(to)
    files <- list.files(from, full.names = FALSE, recursive = TRUE)
    purrr::walk(files, ~ file.copy(
      file.path(from, .x), file.path(to, .x)
    ))
    unlink(from, recursive = TRUE, force = TRUE)
  }
}


#' @importFrom rstudioapi navigateToFile
open_file <- function(path) rstudioapi::navigateToFile(path)

relative_path <- function(path) {
  str_rem(path, paste0(get_root(), "/"))
}

#' @importFrom  tools file_path_sans_ext
base_name <- function(path) tools::file_path_sans_ext(basename(path))

base_project_dir <- function(path) {
  sans_root <- str_rem(path, paste0(get_root(), "/"))
  sans_tail <- str_rem(sans_root, "\\/.+")
  sans_tail
}

base_subdir <- function(path) {
  sans_root <- str_rem(path, paste0(get_root(), "/"))
  sans_base <- str_rem(sans_root, paste0(base_project_dir(path), "/"))
  sans_tail <- str_rem(sans_base, "\\/.+")
  sans_tail
}

rm_trailing_slash <- function(x) str_rem(x, "\\/$")


apply_func_either_both <- function(f, reg_type, ...) {
  if (!reg_type == "both") {
    f(reg_type, ...)
  } else {
    f("raw", ...)
    f("tidy", ...)
  }
}

`%||%` <- function(a, b) ifelse(!is.null(a), a, b) # nolint