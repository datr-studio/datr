#' @export
add_source <- function(data, name) {
  check_type(data, "data.frame")
  data |> mutate(source = name)
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
