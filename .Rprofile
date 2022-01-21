options(repos = "https://cran.stat.auckland.ac.nz/")

if (interactive()) {
  suppressMessages(require(devtools))
}

.pkg_name <- "datr"
reinstall <- function() {
  if (.pkg_name %in% installed.packages()) {
    remove.packages(.pkg_name)
  }
  devtools::load_all()
  devtools::check()
  devtools::install()
}

devtools::load_all()