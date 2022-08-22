# Handle Abort ---------------------------------------------------------------
#'

#' @import cli
abort_non_unique_name <- function(name, reg_type) {
  cli::cli_alert_danger("Error: {.val {name}} already exists in the {reg_type} data register.")
  cli::cli_text(cli::col_grey("Use {.code force = TRUE} if you want to overwrite it."))
  stop_quietly()
}

abort_non_unique_name_diff_sources <- function(name, reg_type, current_source, new_source) {
  cli::cli_alert_danger("Error: {.val {name}} already exists in the {reg_type} data register.")
  cli::cli_text(cli::col_grey("The version that exists has the source: {current_source}"))
  cli::cli_text(cli::col_grey("The version you're trying to save has the source: {new_source}"))
  stop_quietly()
}

abort_non_unique_filename = function(name, dir) {
    cli::cli_alert_danger("Error: Can't guess file extension for {.val {name}} ")
  cli::cli_text(cli::col_grey("There are multiple names in {.path {dir}} matching this name."))
  stop_quietly()
}

abort_unregistered <- function(name, reg_type) {
  cli::cli_alert_danger("Error: {.val {name}} is not registered in the {reg_type} data register.")
  stop_quietly()
}

abort_unsupported_file <- function(ext) {
  cli::cli_alert_danger("Error: {.path .{ext}} is not a supported file type.")
  stop_quietly()
}

abort_file_not_found <- function(filename) {
  cli::cli_alert_danger("Error: the file {.path {filename}} doesn't exist.")
  stop_quietly()
}

abort_folder_not_found <- function(folder) {
  cli::cli_alert_danger("Error: the folder {.path {folder}} doesn't exist.")
  stop_quietly()
}

abort_arg_wrong_len <- function(arg, arg_name, exp_len) {
  cli::cli_alert_danger("Error: {.var {arg_name}} must contain only {exp_len} element{?s}")
  cli::cli_text("You've supplied {.val {length(arg)}} element{?s}: {arg}")
  stop_quietly()
}

abort_arg_wrong_type <- function(arg, arg_name, exp_type) {
  exp_type <- str_repl(exp_type, "\\|", " or ")
  cli::cli_alert_danger("Error: {.var {arg_name}} must be a {exp_type}")
  cli::cli_text("You've supplied a {.cls {class(arg)}} type.")
  stop_quietly()
}

abort_incorrect_version <- function(version) {
  cli::cli_alert_danger("Error: {.arg version} must be a character or numeric of length 1.")
  cli::cli_text("You've given the following: {.val {version}}.")
  stop_quietly()
}

abort_multiple_sources <- function(name) {
  cli::cli_alert_danger("Error: A source for {.val {name}} cannot be determined.")
  cli::cli_text(cli::col_grey("The data frame contains multiple sources."))
  stop_quietly()
}

abort_no_source <- function(name) {
  cli::cli_alert_danger("Error: A source for {.val {name}} cannot be determined.")
  cli::cli_text(
    cli::col_grey(
      "The data frame does not contains a source column and non was specified."
    )
  )
  stop_quietly()
}

abort_no_such_source <- function(name, reg_type) {
  cli::cli_alert_danger("Error: A source named {.val {name}} could not be found in the {reg_type} register.")
  stop_quietly()
}

abort_no_file_ext <- function(name) {
  cli::cli_alert_danger("Error: No valid file extension was found in {.val {name}}.")
  stop_quietly()
}

abort_no_data <- function(name) {
  cli::cli_alert_danger("Error: The update function requires either a dataframe or a filepath to one.")
  stop_quietly()
}

stop_quietly <- function() {
  options(show.error.messages = FALSE)
  on.exit(options(show.error.messages = TRUE))
  stop()
}

# Handle Warnings ---------------------------------------------------------------
#'
#'
#'


warn_unregistered <- function(name, reg_type) {
  cli::cli_alert_warning("{.val {name}} was not found in {reg_type} data.")
}

warn_no_desc <- function() {
  cli::cli_alert_danger("{.pkg datr} requires a tidy project structure to work correctly.")
  msg <- cli::format_inline("Continuing will set up a `DESCRIPTION` file.")
  if (confirm_action(msg)) {
    use_desc(get_proj_name())
  } else {
    stop_quietly()
  }
}

warn_no_data <- function() {
  cli::cli_alert_danger("{.pkg datr} requires a tidy project structure to work correctly.")
  msg <- cli::format_inline("Continuing will set up tidy data now.")
  if (confirm_action(msg)) {
    use_tidy_data()
  } else {
    stop_quietly()
  }
}

warn_skipping_dir <- function(name) {
  cli::cli_alert_warning("Warning: {.path {name}} can't be added to the register as it is a directory not a file")
}

# User Input ---------------------------------------------------------------
#'

confirm_action <- function(msg, default_yes = TRUE) {
  q <- ifelse(default_yes, "[Y/n]", "[y/N]")

  confirmation_prompt <- cli::format_inline(
    "{cli::symbol$pointer}{cli::symbol$pointer}{cli::symbol$pointer} {.strong Do you want to continue {q} ?}"
  )
  cat(
    cli::col_yellow("!"), cli::format_inline(msg), "\n"
  )
  if (interactive()) {
    res <- readline(confirmation_prompt)
  } else {
    cat(confirmation_prompt)
    res <- readLines("stdin", n = 1)
  }
  parse_res(res, default_yes)
}

parse_res <- function(res, default_yes) {
  res <- tolower(res)
  if (nchar(res) == 0) {
    return(default_yes)
  } else if (res == "y") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

get_proj_name <- function() {
  confirmation_prompt <- "Enter your project name: "
  if (interactive()) {
    res <- readline(confirmation_prompt)
  } else {
    cat(confirmation_prompt)
    res <- readLines("stdin", n = 1)
  }
  res
}
