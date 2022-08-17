#' @export
get_root <- function() {
  if (is.null(.state$root)) {
    old <- getwd()
    on.exit(setwd(old))
    for (attempt in 1:4) {
      if (file.exists("DESCRIPTION")) {
        .state$root <- getwd()
        return(getwd())
      } else {
        setwd("..")
      }
    }
  } else {
    .state$root
  }
}


set_root <- function() setwd(get_root())


#' @export
from_dls <- function(f) {
  if (!Sys.info()[["sysname"]] == "Windows") {
    paste0("~/Downloads/", f)
  } else {
    file.path("C:/Users", Sys.info()[["user"]], "Downloads", f)
  }
}
