#' @export
use_tests <- function() {
  root <- get_root()
  mkdir(file.path("tests", "testthat"))
  get_template("tests-run-tests.R") %>%
    write_file(file.path("tests", "run-tests.R"))
}