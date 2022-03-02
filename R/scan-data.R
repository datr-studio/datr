#' @export
search_raw <- function(name) {
  .state$raw %>%
    dplyr::filter(strdetc(tolower(.data$name), .env$name))
}

#' @export
search_tidy <- function(name) {
  .state$tidy %>%
    dplyr::filter(strdetc(tolower(.data$name), .env$name))
}

#' @export
search_source_raw <- function(source) {
  .state$raw %>%
    dplyr::filter(strdetc(tolower(.data$source), .env$source))
}

#' @export
search_source_tidy <- function(source) {
  .state$raw %>%
    dplyr::filter(strdetc(tolower(.data$source), .env$source))
}