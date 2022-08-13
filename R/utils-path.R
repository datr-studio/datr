#' @export
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


#' @export
from_dls <- function(f) {
  if (!Sys.info()[["sysname"]] == "Windows") {
    paste0("~/Downloads/", f)
  } else {
    file.path("C:/Users", Sys.info()[["user"]], "Downloads",f)
  }
}
