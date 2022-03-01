#' Find data directory
#'
#' `get_root()` recursively searches backwards through the working directories to find
#'  a file named DESCRIPTION. Will try up to 4 times before giving up.
#'
#' @export
#'
#' @return root path as character or NULL.
get_root <- function() {
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  for (attempt in 1:4) {
    if (file.exists("DESCRIPTION")) {
      return(getwd())
    } else {
      setwd("..")
    }
  }
  invisible(NULL)
}


#' @export
set_root <- function() setwd(get_root())