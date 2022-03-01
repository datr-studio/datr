.state <- new.env(parent = emptyenv())
.state$raw <- NULL
.state$tidy <- NULL

.onAttach <- function(libname, pkgname) { # nolint
  if (has_desc() && has_tidy_data()) {
    .state$raw <- load_register("raw")
    .state$tidy <- load_register("tidy")
  } else {
    warn_not_tidy()
  }
}


has_desc <- function() {
  !is.null(get_root())
}

has_tidy_data <- function() {
  root <- get_root()
  has_raw_reg <- file.exists(file.path(root, "data", "raw-register.csv"))
  has_tidy_reg <- file.exists(file.path(root, "data", "tidy-register.csv"))
  has_raw_reg && has_tidy_reg
}

load_register <- function(reg_type) {
  reg <- paste0(reg_type, "-register.csv")
  filename <- file.path(get_root(), "data", reg)
  vroom::vroom(filename, delim = ",", col_types = "ccc")
}

get_register <- function(reg_type) {
  switch(reg_type,
    raw = .state$raw,
    tidy = .state$tidy,
    stop("Unknown register type")
  )
}

update_register <- function(data, reg_type) {
  switch(reg_type,
    raw = update_register_raw(data),
    tidy = update_register_tidy(data),
    stop("Unknown register type")
  )
}


update_register_raw <- function(data) {
  .state$raw <- data
  save_register(data, "raw")
}

update_register_tidy <- function(data) {
  .state$tidy <- data
  save_register(data, "tidy")
}

save_register <- function(data, reg_type) {
  reg <- paste0(reg_type, "-register.csv")
  filename <- file.path(get_root(), "data", reg)
  vroom::vroom_write(data, filename, delim = ",")
}

#' @import magrittr