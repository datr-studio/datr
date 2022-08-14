#' @export
relative_change <- function(from, to) (to - from) / from

#' @export
as_perc <- function(x, digits = 1) paste0(round(x * 100, digits), "%")

#' @export
add_source = function(data, name) {
  check_type(data, "data.frame")
  data %>% mutate(source = name)
}