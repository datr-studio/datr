#' @export
render_fig <- function(name, ext = "png") {
  knitr::include_graphics(
    paste0(.state$fig_dir, "/", name, ".", ext),
    rel_path = FALSE
  )
}
