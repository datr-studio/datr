


#' Add source column to dataframe
#'
#' Simply adds a new `source` column to the dataframe with the value you specify.
#'
#' @param data A dataframe.
#' @param source_name Name of the value you want in the new `source` column.
#'
#' @return Dataframe
#'
#' @export
#'
#' @examples
#' add_source(mtcars, "R")
add_source <- function(data, source_name) {
  data$source <- source_name
}



#' Add Proportion
#'
#' Adds a proportion column to dataframe
#'
#' @param data A dataframe
#' @param by column to use for proportions, defaults to `n`.
#'
#' @importFrom dplyr mutate
#'
#' @export
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
add_prop <- function(data, by = "n") {
  dplyr::mutate(data, prop = .data[[by]] / sum(.data[[by]]))
}