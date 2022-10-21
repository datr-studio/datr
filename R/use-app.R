structure <- c(
  "environment",
  "content/helpers",
  "content/plots",
  "environment",
  "ui/components",
  "ui/interface",
  "ui/layout",
  "www"
)

core_files <- c(
  "app",
  "environment/jsServers",
  "environment/servers",
  "environment/state",
  "environment/urlHandler",
  "ui/layout/body",
  "ui/layout/header",
  "ui/layout/sidebar"
)


#' @import purrr
#' @export
use_app <- function() {
  mkdir("app")
  purrr::walk(structure, ~ mkdir(paste0("app/", .x)))
  purrr::walk(core_files, ~ {
    filename <- paste0(basename(.x), ".R")
    content <- get_template(filename)
    write_file(content, paste0("app/", .x, ".R"))
  })
  write_file("", "app/www/styles.css")
  write_file("id,type,func\n", "app/environment/servers.csv")
}
