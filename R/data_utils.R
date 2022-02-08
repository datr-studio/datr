


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
#' @export
#'
#' @return A dataframe, with added prop column
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(datr)
#' tibble(a = runif(1e3)) %>%
#'   count(a > 0.5) %>%
#'   add_prop()
#' # A tibble: 2 × 3
#' #   `a > 0.5`     n  prop
#' #   <lgl>     <int> <dbl>
#' # 1 FALSE       512 0.512
#' # 2 TRUE        488 0.488
#' }
add_prop <- function(data, by = "n") {
  col_name <- ensure_unique(data, "prop")
  data[col_name] <- data[[by]] / sum(data[[by]])
  data
}


#' @export
relative_change <- function(from, to) (to - from) / from


ensure_unique <- function(data, x) ifelse(any(names(data) == x), paste0(x, "..1"), x)

#' @export
get_final_ys <- function(data, .x, .y, names) {
  data %>%
    ungroup() %>%
    filter({{ .x }} == max({{ .x }})) %>%
    select({{ names }}, {{ .y }}) %>%
    deframe()
}