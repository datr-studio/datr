# Operators ---------------------------------------------------------------
#'
#' @export
`%+%` <- function(a, b) { # nolint
  if (is.character(a) && is.character(b)) {
    paste0(a, b)
  } else {
    stop("%+% requires two strings")
  }
}

#' @export
`%||%` <- function(a, b) ifelse(!is.null(a), a, b) # nolint

#' @export
relative_change <- function(from, to) (to - from) / from