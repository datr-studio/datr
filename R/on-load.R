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
  has_raw_reg_as_fe <- file.exists(file.path(root, "data", ".raw.fe"))
  has_tidy_reg_as_fe <- file.exists(file.path(root, "data", ".tidy.fe"))
  # Interim automatic update
  if (has_raw_reg_as_fe && has_tidy_reg_as_fe) {
    original <- feather::read_feather(file.path(root, "data", ".raw.fe"))
    original$version <- NULL
    vroom::vroom_write(original, file.path(root, "data", ".raw.csv"), delim = ",")
    original <- feather::read_feather(file.path(root, "data", ".tidy.fe"))
    original$version <- NULL
    vroom::vroom_write(original, file.path(root, "data", ".tidy.csv"), delim = ",")
    unlink(file.path(root, "data", ".raw.fe"))
    unlink(file.path(root, "data", ".tidy.fe"))
  }
  has_raw_reg <- file.exists(file.path(root, "data", ".raw.csv"))
  if (has_raw_reg) {
    f <- file.path(root, "data", ".raw.csv")
    data <- vroom::vroom(f, show_col_types = F)
    if (!"url" %in% colnames(data)) data$url <- NA_character_
    vroom::vroom_write(data, f, delim = ",")
  }
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
