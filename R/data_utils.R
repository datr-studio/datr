


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
  data
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
  .data <- NULL
  data <- dplyr::mutate(data, prop = .data[[by]] / sum(.data[[by]]))
  data
}




#' Calculate relative change
#'
#' \eqn{\frac{after-before}{before}}
#'
#' `relative_change()` can also optionally round, convert to percentage,
#' and even convert to str in the form of 'XX%', if required.
#'
#' Note that rounding is performed after conversion to percentage, if both options are selected.
#'
#' @param before Numeric vector of before values.
#' @param after Numeric vector of after values.
#' @param digits Number of digits to round to, if desired.
#' @param as_perc Return proportion as percentage (i.e. * 100).
#' @param as_str Return as a character vector with percent symbol.
#'
#'
#' @export
#'
#' @return Numeric vector
#' @examples
#' x <- 1:10
#' y <- x * 2
#' relative_change(x, y)
relative_change <- function(before, after, digits = NULL, as_perc = FALSE, as_str = FALSE) {
  stopifnot(is.numeric(before), is.numeric(after))
  res <- (after - before) / before
  if (as_perc) {
    res <- res * 100
  }
  if (!is.null(digits)) {
    stopifnot(is.numeric(digits))
    res <- round(res, digits)
  }
  if (as_str) {
    res <- paste0(res, "%")
  }
  res
}
