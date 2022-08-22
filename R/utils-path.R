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


file_has_no_ext <- function(x) stringr::str_detect(x, "\\..+$", negate = TRUE)

guess_full_filename <- function(name, dir) {
  raw_files <- list.files(dir)
  file_names <- map_chr(raw_files, tools::file_path_sans_ext)
  search_name <- paste0("^", name, "$")
  if (sum(str_detect(file_names, search_name)) == 1) {
    loc <- match(name, file_names)
    return(raw_files[[loc]])
  } else {
    abort_non_unique_filename(name, dir)
  }
}
