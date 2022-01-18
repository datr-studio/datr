#' @import crayon
.onAttach <- function(libname, pkgname) { # nolint

  cat(crayon::cyan(phrases[round(runif(1, 1, length(phrases)))]))
  cat(crayon::blurred$italic("\nHave you activated a stretch timer?"))
}


phrases <- c(
  "Right me lovers, get to work",
  "Let's get shit done!",
  "You are one determined motherfucker",
  "Go be a badass",
  "Patience, ruthless determination, and power."
)
