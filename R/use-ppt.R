#' @export
use_ppt <- function(title, subdir = "", filename = NULL) {
  #  check args
  check_type(title, "character")
  check_type(subdir, "character")
  if (!is.null(filename)) check_type(filename, "character")
  filename <- paste0(filename %||% make_filename(title), ".Rmd")

  # Confirm base path exists
  root <- get_root()
  path <- file.path(root, "presos")
  if (!dir.exists(path)) use_presos(root)

  rmd_setup_params <- ifelse(subdir != "", paste0("\"", subdir, "\""), "")
  path <- file.path(path, subdir)
  mkdir(path)


  # Build subdir context
  mkdir(file.path(path, "figures"))
  mkdir(file.path(path, "ppt"))

  # build nb
  nb <- get_template("rmd-ppt-start.Rmd")

  nb <- nb %>%
    replace_line(2, paste0("title: ", title)) %>%
    append_lines(get_template("rmd-setup-ppt.Rmd") %>%
      replace_line(8, paste0("rmd_setup(", rmd_setup_params, ", ppt = T)")))

  nb <- append_lines(nb, get_template("rmd-base-end.Rmd"))
  write_file(nb, file.path(path, filename))
}

make_filename <- function(title) standardise_filename(title)
