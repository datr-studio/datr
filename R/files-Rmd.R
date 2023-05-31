#' Setup Rmarkdown consistently
#'
#' Ensures that Rmarkdown is setup the same, even if the setup itself changes in the future.
#'
#' @importFrom knitr opts_chunk
#' @importFrom ggplot2 theme_set
#' @importFrom zibas theme_ziba
#' @importFrom knitr opts_chunk
#' @import glue
#' @export
rmd_setup <- function(subdir = "", export = FALSE, validation = FALSE,
                      figdir = "figures", png = TRUE, pdf = FALSE, ppt = FALSE,
                      echo = FALSE) {

  # Rmd should always be in the folder "notebooks" under the main project folder, but can include a subdir
  root_dir <- get_root()
  if (export) {
    folder <- "export"
  } else if (validation) {
    folder <- "validation/analysis"
  } else {
    folder <- "notebooks"
  }
  working_dir <- file.path(root_dir, folder, subdir)

  .state$rmd_dir <- working_dir

  options(fig_path = file.path(working_dir, figdir))

  if (getwd() != working_dir) {
    if (interactive()) cli::cli_alert_info("Notebook path was changed to {.path {working_dir}}")
    setwd(working_dir)
  }


  options(pillar.print_min = 5)

  # Plot Settings
  library(zibas)
  make_beautiful_plots()
  png_output <- function(..., res = 1080) {
    ragg::agg_png(..., res = res, units = "in")
  }

  if (png) {
    device <- "png_output"
    ext <- "png"
    out_width <- "75%"
    assign("png_output", png_output, envir = globalenv())
  } else if (pdf) {
    device <- "tikz"
    ext <- "png"
    out_width <- "90%"
    knitr::opts_chunk$set(echo = FALSE)
    options("zibas.format" = "pdf")
  } else {
    device <- "svglite"
    ext <- "svg"
    out_width <- "100%"
  }

  knitr::opts_chunk$set(
    warning = FALSE,
    message = FALSE,
    echo = echo,
    out.width = out_width,
    dev = device,
    fig.ext = ext,
    fig.width = 8,
    fig.height = 6
  )

  if (!ppt) knitr::opts_chunk$set(
    fig.align = "center",
  )

  if (interactive()) {
    cli::cli_inform(
      cli::col_grey(
        "{.emph Taking time to write your thoughts now saves you from having to figure them out again later.}"
      )
    )
  }
}
