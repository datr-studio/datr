get_template <- function(name) {
  readLines(file.path(get_root_dir(), "templates", name))
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