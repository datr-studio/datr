#' Recursively source all R files in a folder and its subfolders
#'
#' Recursively source a folder, loading all files with .R ext into the global environment.
#'
#' @param folder The directory to source.
#' @param silent Logical. If `TRUE`, information is given about the number of files sourced. Defaults to `FALSE`
#' @param pattern An optional regex pattern to use to filter the path.
#' @param excl An optional regex pattern to use to exclude files or folders from the search.
#'
#' @export
#'
source_folder <- function(folder, silent = TRUE, pattern = NULL, excl = NULL) {
  stopifnot(is.character(folder), length(folder) == 1, dir.exists(folder))
  paths <- list.files(folder, full.names = T, pattern = pattern)
  if (!is.null(excl)) {
    paths <- paths[stringr::str_detect(paths, paste0(folder, "\\/", excl), negate = TRUE)]
  }
  n <- 0
  for (path in paths) {
    if (dir.exists(path)) {
      source_folder(path, silent)
    } else if (stringr::str_detect(path, pattern = "[a-z|A-Z|_]+\\.R$")) {
      source(path)
      n <- n + 1
    }
  }
  if (!silent) {
    if (n == 0) {
      warning("No R files could be found in ", folder)
    } else if (n == 1) {
      message("1 R file in ", folder, " was sourced.")
    } else {
      message(paste0(n, " R files in ", folder, " were sourced."))
    }
  }
  invisible()
}
