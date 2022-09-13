#' @export
relative_change <- function(from, to) (to - from) / from

#' @export
as_perc <- function(x, digits = 1) paste0(round(x * 100, digits), "%")

#' @export
add_source = function(data, name) {
  check_type(data, "data.frame")
  data %>% mutate(source = name)
}

#' @export
get_points = function(data, time_var = "year") {
    filter(data, .data[[time_var]] == min(.data[[time_var]]) | .data[[time_var]] == max(.data[[time_var]]))
}

