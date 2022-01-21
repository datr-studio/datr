
#' Input helper
#'
#' Provides appropriate input function depending on whether session is interactive
#'
#' @param prompt message to display.
#'
#' @return Text that was inputed
#'
#' @export
#' @import cli
#'
#' @examples
#' input("What do you want to ask? ")
input <- function(prompt) {
  if (interactive()) {
    return(readline(cli::col_yellow(prompt)))
  } else {
    cli::cli_text(cli::col_yellow(prompt))
    return(readLines("stdin", n = 1))
  }
}