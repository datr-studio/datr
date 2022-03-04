get_root <- function() {
  old <- getwd()
  on.exit(setwd(old))
  for (attempt in 1:4) {
    if (file.exists("DESCRIPTION")) {
      return(getwd())
    } else {
      setwd("..")
    }
  }
  invisible(NULL)
}


set_root <- function() setwd(get_root())

