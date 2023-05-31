#' @export
use_nb <- function(title, subdir = "EDA", filename = NULL, pdf = FALSE,
                   doc = FALSE, export = FALSE, use_func = FALSE) {
  #  check args
  check_type(title, "character")
  check_type(subdir, "character")
  if (!is.null(filename)) check_type(filename, "character")
  filename <- paste0(filename %||% make_filename(title), ".Rmd")

  # Confirm base path exists
  root <- get_root()
  if (export) {
    path <- file.path(root, "export")
    mkdir(path)
    subdir <- ifelse(subdir == "EDA", "", subdir)
    rmd_setup_params <- "export = TRUE"
  } else {
    path <- file.path(root, "notebooks")
    if (!dir.exists(path)) use_notebooks(root)
    rmd_setup_params <- paste0("\"", subdir, "\"")
  }
  path <- file.path(path, subdir)
  mkdir(path)

  # Build subdir context
  if (use_func) mkdir(file.path(path, "functions"))
  mkdir(file.path(path, "figures"))
  if (pdf) {
    mkdir(file.path(path, "pdf"))
  } else if (doc) {
    mkdir(file.path(path, "docx"))
  } else {
    mkdir(file.path(path, "html"))
  }


  # build nb

  if (pdf) {
    nb <- get_template("rmd-pdf-start.Rmd")
    rmd_setup_params <- paste0(rmd_setup_params, ", pdf = TRUE")
  } else if (doc) {
    nb <- get_template("rmd-word-start.Rmd")
  } else {
    css <- "    css: \"~/Projects/PROJECT_NAME/templates/nb.css\"" %>%
      str_repl("PROJECT_NAME", basename(root))
    nb <- get_template("rmd-html-start.Rmd") %>%
      replace_line(9, css)
  }
  nb <- nb %>%
    replace_line(2, paste0("title: ", title)) %>%
    append_lines(get_template("rmd-setup.Rmd") %>%
      replace_line(7, paste0("rmd_setup(", rmd_setup_params, ")")))
  if (use_func) nb <- append_lines(nb, get_template("rmd-func.Rmd"))
  nb <- append_lines(nb, get_template("rmd-base-end.Rmd"))
  write_file(nb, file.path(path, filename))
}

make_filename <- function(title) standardise_filename(title)
