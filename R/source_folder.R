#' Recursively source all R files in a folder and its subfolders
#'
#' Recursively source a folder, loading all files with .R ext into the global environment.
#'
#' @param folder the directory to source.
#' @param silent logical. If `TRUE`, information is given about the number of files sourced. Defaults to `FALSE`
#'
#' @export
#' @examples
#' \dontrun{
#' source_folder("path/to/myfolder")
#' }
source_folder <- function(folder, silent = FALSE) {
  stopifnot(is.character(folder), length(folder) == 1, dir.exists(folder))
  paths <- list.files(folder, full.names = T)
  n <- 0
  for (path in paths) {
    if (dir.exists(path)) {
      source_folder(path)
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