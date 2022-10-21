#' @export
relative_change <- function(from, to, as_perc = TRUE, invert = FALSE, ...) {
  invert_mod <- ifelse(invert, -1, 1)
  out <- (to - from) / from * invert_mod
  if (as_perc) out <- as_perc(out, ...)
  out
}

#' @export
as_perc <- function(x, digits = 1) paste0(round(x * 100, digits), "%")

#' @export
add_source <- function(data, name) {
  check_type(data, "data.frame")
  data %>% mutate(source = name)
}

#' @export
get_points <- function(data, time_var = "year") {
  filter(data, .data[[time_var]] == min(.data[[time_var]]) | .data[[time_var]] == max(.data[[time_var]]))
}

#' @export
mmd_to_pdf <- function(f, out_path = ".") {
  basename <- tools::file_path_sans_ext(basename(f))
  f_out <- paste0(basename, ".pdf")
  system(glue::glue("mmdc -i {f} -o {file.path(out_path, f_out)} -f"))
}


#' @export
on_windows <- function() Sys.info()[["sysname"]] == "Windows"
