#' Recursively source all R files in a folder and its subfolders
#'
#' Recursively source a folder, loading all files with .R ext into the global environment.
#'
#' @param folder The directory to source.
#' @param incl An optional regex pattern to use to filter what is included.
#' @param excl An optional regex pattern to use to exclude files or folders from the search.
#' @param stop_on_error If `TRUE` (default), code will stop on error, with info. If false, only a warning will be issued.
#' @param silent If `TRUE` (default), information is given about the number of files sourced.
#' @param verbose If true, each file will be printed to console on sourcing.
#'
#'
#' @export
#' @import cli
#' @importFrom stringr str_detect
#' @importFrom rlang expr_text
#'
#' @examples
#' \dontrun{
#' # Source everything in R folder
#' source_folder("R")
#'
#' # Source everything that starts with "app_"
#' source_folder("R", incl = "app_")
#'
#' # Source everything that doesn't start with "app_"
#' source_folder("R", excl = "app_")
#'
#' # Source with full output (useful for debuging)
#' source_folder("R", verbose = TRUE)
#'
#' # Source and warn only if errors
#' source_folder("R", stop_on_error = FALSE)
#' }
#'
source_folder <- function(folder, incl = NULL, excl = NULL, silent = FALSE, verbose = FALSE, stop_on_error = TRUE) {
  reset_state()
  stopifnot(is.character(folder), length(folder) == 1)
  if (!dir.exists(folder)) {
    cli::cli_abort("The folder '{folder}' doesn't seem to exist. Check your working directory and spelling.")
  }

  if (is.null(incl)) {
    paths <- list.files(folder, full.names = T)
  } else {
    paths <- list.files(folder, full.names = T, pattern = incl)
  }
  if (!is.null(excl)) {
    paths <- paths[stringr::str_detect(paths, paste0(folder, "\\/", excl), negate = TRUE)]
  }

  for (path in paths) {
    if (dir.exists(path)) {
      source_folder(path, incl, excl, silent = TRUE, verbose, stop_on_error)
      increment_ndirs()
    } else if (stringr::str_detect(path, pattern = ".\\.R$")) {
      if (!is.null(excl)) {
        if (stringr::str_detect(path, pattern = excl)) next
      }


      if (verbose) cli::cli_alert_info("Sourcing {path}")

      tryCatch(
        {
          source(path)
          increment_nfiles()
        },
        error = function(e) {
          cli::cli_alert_danger("Unable to source {cli::col_yellow(path)}")
          msg <- conditionMessage(e)
          call <- rlang::expr_text(unclass(e)$call)
          cli::cli_text(
            cli::col_silver("A {.emph {msg}} error occurred while attempting '{call}'.")
          )
          if (stop_on_error) {
            stop_quietly()
          }
        }
      )
    }
  }

  if (!silent) {
    if (nfiles() == 0) {
      cli::cli_alert_warning("No R files could be found in {.path {folder}}.")
    } else {
      if (ndirs() > 0) {
        cli::cli_alert_success(
          "Sourced {.val {nfiles()}} file{?s} from {.path {folder}} (including {.val {ndirs()}} subdirector{?y/ies})."
        )
      } else {
        cli::cli_alert_success("Sourced {.val {nfiles()}} file{?s} from {.path {folder}}.")
      }
    }
  }
  invisible()
}

state <- new.env(parent = emptyenv())
state$nfiles <- 0
state$ndirs <- 0

reset_state <- function() {
  state$nfiles <- 0
  state$ndirs <- 0
}

nfiles <- function() {
  state$nfiles
}

ndirs <- function() {
  state$ndirs
}

increment_nfiles <- function() {
  state$nfiles <- state$nfiles + 1
}

increment_ndirs <- function() {
  state$ndirs <- state$ndirs + 1
}