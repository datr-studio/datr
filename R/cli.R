abort_theme <- list(.alert = list(color = "red"))

show_var_wrong_type_error <- function(x, var_name, exp_type) {
  cli::cli_div(theme = abort_theme)
  cli::cli_alert_danger("Error: {.var {var_name}} must be a {exp_type}")
  cli::cli_end()
  cli::cli_text("You've supplied a {.cls {class(x)}} type.")
}

show_data_save_success <- function(path) {
  cli::cli_alert_success("Data saved to {.path {path}}")
}

show_ambiguous_filename <- function() {
  cli::cli_div(theme = abort_theme)
  cli::cli_alert_danger("Error: Filename is ambiguous.")
  cli::cli_end()
  cli::cli_text(cli::col_grey("Use a file extension when more than one file share the same name."))
  stop_quietly()
}


show_file_not_found <- function(filename, leading_dirs) {
  cli::cli_div(theme = abort_theme)
  cli::cli_alert_danger("Error: Cannot find anything named {.arg {filename}} in the directory.")
  cli::cli_end()
  cli::cli_text("Are you sure it's located in {.path {leading_dirs}}?")
  stop_quietly()
}
stop_quietly <- function() {
  options(show.error.messages = FALSE)
  on.exit(options(show.error.messages = TRUE))
  stop()
}