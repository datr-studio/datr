# Data management ---------------------------------------------------------------
#'
#'
#'

.state <- new.env(parent = emptyenv())
.state$raw <- NULL
.state$tidy <- NULL
.state$external_file_types <- c("doc", "docx", "pdf")
.state$root <- NULL
.state$rmd_dir <- NULL

.onLoad <- function(libname, pkgname) { # nolint
  if (!has_desc()) warn_no_desc()
  if (!has_tidy_data()) warn_no_data()
  .state$raw <- load_register("raw")
  .state$tidy <- load_register("tidy")
}


has_desc <- function() {
  !is.null(get_root())
}

has_tidy_data <- function() {
  root <- get_root()
  has_raw_reg <- file.exists(file.path(root, "data", ".raw.csv"))
  has_tidy_reg <- file.exists(file.path(root, "data", ".tidy.csv"))
  has_raw_reg && has_tidy_reg
}

load_register <- function(reg_type) {
  reg <- paste0(".", reg_type, ".csv")
  filename <- file.path(get_root(), "data", reg)
  vroom::vroom(filename, show_col_types = F)
}

get_register <- function(reg_type) {
  switch(reg_type,
    raw = .state$raw,
    tidy = .state$tidy,
    stop("Unknown register type")
  )
}

update_register <- function(data, reg_type) {
  .state[[reg_type]] <- data
  save_register(data, reg_type)
}

save_register <- function(data, reg_type) {
  reg <- paste0(".", reg_type, ".csv")
  filename <- file.path(get_root(), "data", reg)
  vroom::vroom_write(data, filename, delim = ",")
}
