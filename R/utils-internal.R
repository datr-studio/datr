# Operators ---------------------------------------------------------------
#'

`%+%` <- function(a, b) {
  if (is.character(a) && is.character(b)) {
    paste0(a, b)
  } else {
    stop("%+% requires two strings")
  }
}

`%||%` <- function(a, b) ifelse(!is.null(a), a, b)

# Type Checking ---------------------------------------------------------------
#'

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
        paste0(y, collapse = ","),
        call. = FALSE
      )
    }
  }
}


# Lists ---------------------------------------------------------------
#'

compact <- function(l) Filter(Negate(is.null), l)

# Regex ---------------------------------------------------------------
#'

strext <- function(x, y) regmatches(x, regexpr(y, x))

strepl <- function(x, y, z) gsub(y, z, x)

strem <- function(x, y) gsub(y, "", x)

strdetc <- function(x, y) grepl(y, x)

# Paths ---------------------------------------------------------------
#'

rem_ext <- function(x) {
  x <- sub("[.](gz|bz2|xz)$", "", x)
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}



#' @export
#' @import rlang
rstudio_stfu <- function(f, ...) {
  withCallingHandlers(
    warning = function(cnd) {
      if (str_detect(cnd$message, "Please install a newer")) rlang::cnd_muffle(cnd)
    },
    f(...)
  )
}

check_type <- function(arg, exp_type) {
  arg_name <- rlang::expr_text(rlang::enexpr(arg))
  if (!inherits(arg, exp_type)) {
    abort_arg_wrong_type(arg, arg_name, exp_type)
  }
}

check_exists <- function(filepath) {
  if (!file.exists(filepath)) abort_file_not_found(filepath)
}