#' @import cli
abort_non_unique_name <- function(name, reg_type) {
  cli::cli_alert_danger("Error: {.val {name}} already exists in the {reg_type} data register.")
  stop_quietly()
}

abort_unregistered <- function(name, reg_type) {
  cli::cli_alert_danger("Error: {.val {name}} is not registered in the {reg_type} data stop_quietly()register.")
}

abort_unsupported_file <- function(ext) {
  cli::cli_alert_danger("Error: {.path .{ext}} is not a supported file type.")
  stop_quietly()
}

abort_file_not_found <- function(filename) {
  cli::cli_alert_danger("Error: the file {.path {filename}} doesn't exist.")
  stop_quietly()
}

abort_arg_wrong_type <- function(arg, arg_name, exp_type) {
  cli::cli_alert_danger("Error: {.var {arg_name}} must be a {exp_type}")
  cli::cli_text(cli::col_grey("You've supplied a {.cls {class(x)}} type."))
  stop_quietly()
}

abort_multiple_sources <- function(name) {
  cli::cli_alert_danger("Error: A source for {.val {name}} cannot be determined.")
  cli::cli_text(cli::col_grey("The dataframe contains multiple sources."))
  stop_quietly()
}

abort_no_source <- function(name) {
  cli::cli_alert_danger("Error: A source for {.val {name}} cannot be determined.")
  cli::cli_text(
    cli::col_grey(
      "The dataframe does not contains a source column and non was specified."
    )
  )
  stop_quietly()
}

warn_not_tidy <- function() {
  cli::cli_alert_warning("Warning: {.pkg datr} requires a tidy project structure to work correctly.")
  cli::cli_text(
    cli::col_silver(
      "Try `datr::use_desc()` and `datr::use_data()` to set your project up correctly."
    )
  )
}

stop_quietly <- function() {
  options(show.error.messages = FALSE)
  on.exit(options(show.error.messages = TRUE))
  stop()
}