get_template <- function(name) {
  templates[[name]]
}

replace_line <- function(template, index, new_line) {
  template[[index]] <- new_line
  template
}

remove_lines <- function(template, lines) {
  template[-lines]
}

append_lines <- function(template, new_lines) {
  c(template, new_lines)
}

templates <- new.env(parent = emptyenv())


load_template <- function(name, templates) {
  templates[[name]] <- readLines(file.path("templates", name))
}



purrr::walk(list.files("templates"), load_template, templates)