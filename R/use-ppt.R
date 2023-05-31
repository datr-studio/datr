#' @export
use_ppt <- function(title, subdir = "EDA", filename = NULL, use_func = FALSE) {
  #  check args
  check_type(title, "character")
  check_type(subdir, "character")
  if (!is.null(filename)) check_type(filename, "character")
  filename <- paste0(filename %||% make_filename(title), ".Rmd")

  # Confirm base path exists
  root <- get_root()
  path <- file.path(root, "notebooks")
  if (!dir.exists(path)) use_notebooks(root)
  rmd_setup_params <- paste0("\"", subdir, "\"")

  path <- file.path(path, subdir)
  mkdir(path)


  # Build subdir context
  if (use_func) mkdir(file.path(path, "functions"))
  mkdir(file.path(path, "figures"))
  mkdir(file.path(path, "ppt"))

  # build nb
  nb <- get_template("rmd-ppt-start.Rmd")

  nb <- nb %>%
    replace_line(2, paste0("title: ", title)) %>%
    append_lines(get_template("rmd-setup.Rmd") %>%
      replace_line(7, paste0("rmd_setup(", rmd_setup_params, ", pdf = T)")))

  if (use_func) nb <- append_lines(nb, get_template("rmd-func.Rmd"))
  nb <- append_lines(nb, get_template("rmd-base-end.Rmd"))
  write_file(nb, file.path(path, filename))
}

make_filename <- function(title) standardise_filename(title)
