
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
    return(readline(crayon::yellow$bold(prompt)))
  } else {
    cat(crayon::yellow$bold(prompt))
    return(readLines("stdin", n = 1))
  }
}
