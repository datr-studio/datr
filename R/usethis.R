#' @export
use_data <- function(path = ".") {
  dir.create(file.path(path, "data", "raw"), recursive = TRUE)
  dir.create(file.path(path, "data", "tidy"), recursive = TRUE)
  vroom::vroom_write(
    tibble::tibble(name = character(), source = character(), ext = character()),
    file.path(path, "data", "raw-register.csv"),
    delim = ","
  )
  vroom::vroom_write(
    tibble::tibble(name = character(), source = character()),
    file.path(path, "data", "tidy-register.csv"),
    delim = ","
  )
}

#' @export
use_desc <- function(proj_name, path = ".") {
  desc <- c(
    paste0("Package: ", proj_name),
    "Title: What the Package Does (One Line, Title Case)",
    "Version: 0.0.0.9000",
    "Authors@R: person(\"Josh\", \"Firth\", , \"drjoshfirth@gmailcom\", role = c(\"aut\", \"cre\"),",
    "           comment = c(ORCID = \"0000-0003-3815-8789\"))",
    "Description: What the package does (one paragraph).",
    "License: ????",
    "Encoding: UTF-8",
    "Roxygen: list(markdown = TRUE)",
    "RoxygenNote: ????",
    "Suggests: ",
    "    testthat (>= 3.0.0)",
    "Config/testthat/edition: 3"
  )
  con <- file(file.path(path, "DESCRIPTION"))
  writeLines(desc, con)
  close(con)
}