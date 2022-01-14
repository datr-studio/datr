
#' Input helper
#'
#' Provides appropriate input function depending on whether session is interactive
#'
#' @param prompt message to display.
#'
#' @return Text that was inputed
#'
#' @export
#'
#' @examples
#' input("What do you want to ask? ")
input <- function(prompt) {
  if (interactive()) {
    return(readline(prompt))
  } else {
    cat(prompt)
    return(readLines("stdin", n = 1))
  }
}