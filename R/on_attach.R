#' @import crayon
#' @importFrom stats runif
.onAttach <- function(libname, pkgname) { # nolint
  packageStartupMessage(crayon::blue(phrases[round(stats::runif(1, 1, length(phrases)))]))
  packageStartupMessage(crayon::silver$italic("Have you activated a stretch timer?"))
}


phrases <- c(
  "Right me lovers, get to work",
  "Let's get shit done!",
  "You are one determined motherfucker",
  "Go be a badass",
  "Patience, ruthless determination, and power.",
  "True love is a precious gift."
)
