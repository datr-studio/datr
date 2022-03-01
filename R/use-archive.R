
use_archive <- function() {
  mkdir(file.path(get_root(), "archive"))
}



#' Archive file or directory
#'
#' Simple interface to create tarball and save it to archive folder within project.
#'
#' @param path Path to file or dir to archive.
#' @export
archive <- function(path) {
  old <- getwd()
  on.exit(setwd(old))
  set_root()
  use_archive()
  path <- rm_trailing_slash(path)
  check_exists(path)
  if (!is_dir(path)) archive_file(path) else archive_dir(path)
}

archive_file <- function(path) {
  path <- relative_path(path)
  name <- basename(path)
  base_dir <- paste0(base_project_dir(path), "-")
  base_subdir <- base_subdir(path)
  base_subdir <- ifelse(!strdetc(base_subdir, "\\."), paste0(base_subdir, "-"), "")
  to <- file.path("archive", paste0(base_dir, base_subdir, name))
  system(paste0("tar -czf ", to, ".tar.gz ", path))
  if (file.exists(paste0(to, ".tar.gz"))) unlink(path)
  cli::cli_alert_success("File archived to {.path {to}}.")
}

archive_dir <- function(path) {
  path <- relative_path(path)
  base_dir <- paste0(base_project_dir(path), "-")
  base_subdir <- base_subdir(path)
  to <- file.path("archive", paste0(base_dir, base_subdir))
  system(paste0("tar -czf ", to, ".tar.gz ", path))
  if (file.exists(paste0(to, ".tar.gz"))) unlink(path, recursive = TRUE, force = TRUE)
  cli::cli_alert_success("Directory archived to {.path {to}}.")
}



#' Unarchive file or directory
#'
#' Simple helper to extract tarball from project archive and put it back where it was.
#'
#' @param name Name of tarball, sans ".tar.gz".
#' @export
unarchive <- function(name) {
  old <- getwd()
  on.exit(setwd(old))
  set_root()
  name <- ifelse(!strdetc(name, "\\.tar\\.gz"), paste0(name, ".tar.gz"), name)
  path <- file.path("archive", name)
  system(paste0("tar -xzf ", path))
  unlink(path)
  cli::cli_alert_success("{.path {name}} unarchived to original location.")
  rm_archive_if_empty()
}

#' Unarchive file(s) by regex pattern
#'
#' Simple helper to extract tarball from project archive using regex pattern matching.
#'
#' @param pattern Regex pattern to use.
#' @export
unarchive_pattern <- function(pattern) {
  files <- list.files("archive", pattern = pattern)
  purrr::walk(files, unarchive)
}


rm_archive_if_empty <- function() {
  archive_path <- file.path(get_root(), "archive")
  if (length(list.files(archive_path)) == 0) unlink(archive_path, recursive = TRUE, force = TRUE)
}