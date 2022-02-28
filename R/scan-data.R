#' @export
search_raw <- function(name) {
  .state$raw %>%
    dplyr::filter(strdetc(.data$name, .env$name))
}

#' @export
search_tidy <- function(name) {
  .state$tidy %>%
    dplyr::filter(strdetc(.data$name, .env$name))
}

#' @export
raw_by_source <- function(source) {
  .state$raw %>%
    dplyr::filter(strdetc(.data$source, .env$source))
}

#' @export
tidy_by_source <- function(source) {
  .state$raw %>%
    dplyr::filter(strdetc(.data$source, .env$source))
}