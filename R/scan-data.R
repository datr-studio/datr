


#' Search the registers
#'
#' Look up a key word in both registers
#'
#' @param  name A keyword to search the name fields.
#' @export
#' @importFrom dplyr bind_rows
#'
#' @return A tibble.
search_data <- function(name) {
  raw <- search_raw(name)
  raw$register <- "raw"
  tidy <- search_tidy(name)
  tidy$register <- "tidy"
  dplyr::bind_rows(raw, tidy)
}

search_raw <- function(name) {
  .state$raw %>%
    dplyr::filter(in_str(tolower(.data$name), .env$name))
}


search_tidy <- function(name) {
  .state$tidy %>%
    dplyr::filter(in_str(tolower(.data$name), .env$name))
}

#' Search the registers for sources
#'
#' Look up a key word in both registers for matching sources
#'
#' @param  name A keyword to search the source fields.
#' @export
#' @importFrom dplyr bind_rows
#'
#' @return A tibble.
search_source <- function(source) {
  raw <- search_source_raw(source)
  raw$register <- "raw"
  tidy <- search_source_tidy(source)
  tidy$register <- "tidy"
  dplyr::bind_rows(raw, tidy)
}

search_source_raw <- function(source) {
  .state$raw %>%
    dplyr::filter(in_str(tolower(.data$source), .env$source))
}


search_source_tidy <- function(source) {
  .state$tidy %>%
    dplyr::filter(in_str(tolower(.data$source), .env$source))
}