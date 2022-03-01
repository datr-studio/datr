#' @export
relative_change <- function(from, to) (to - from) / from

#' Load utils folder
#'
#' `load_utils()` is another standardisation method from the datr library intended
#' to help keep data science projects organised and repeatable.
#'
#' It makes the assumption that there is a code/utils directory, which will then
#' be sourced.
#'
#' @export
load_utils <- function(pattern = "") {
  saucer::source_folder(file.path(get_root(), "R", "utils"), pattern = "")
}