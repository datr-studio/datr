condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}

unsupported_file_error <- function(ext) {
  msg <- paste0("`.", ext, "` is not a supported file type.")
  msg <- cli::format_inline("{.path .{ext}} is not a supported file type.")
  condition(c("unsupported_file_error", "error"),
    message = msg, ext = ext
  )
}

#' @export
rstudio_stfu <- function(f, ...) {
  withCallingHandlers(
    warning = function(cnd) {
      if (str_detect(cnd$message, "Please install a newer")) rlang::cnd_muffle(cnd)
    },
    f(...)
  )
}