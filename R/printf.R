#' A fancy print function
#'
#' `printf()` is reminiscent of python's f-strings, where prefacing a string with `f` enables the user to evaluate code within braces before printing to the console. This functionality already exists in the glue package; so this is wrapper around that. However, what it also brings is an additional wrapper around the crayon package at the same time. You can therefore colour and evaluate print statements easily.
#'
#' ### How to use printf
#' In short, use `$$` to signal a call to the crayon library. Calls can be nested with further single `$` symbols. Close the call with `$$x` (Optional for the final closing brace).
#'
#' @param string String to format.
#'
#' @import stringr
#' @import crayon
#' @import glue
#' @export
#' @examples
#' var1 <- 1
#' var2 <- 2
#' var3 <- "world"
#' var4 <- "this example"
#'
#'
#' printf("{var1} added to {var2} equals {var1 + var2}")
#' printf("Hello $$strikethrough mum $$x world!")
#' printf("$$blue this example shows that `printf()` will auto close final braces")
#' printf("This example shows that you can $$cyan$bold combine $$x formating")
#' printf("$$magenta$italic {var4} $$x $$green$underline brings it $$x $$blurred all together")
printf <- function(string) {
  # First Evaluate String as String
  output <- glue::glue(string) %>%
    stringr::str_split(" ", simplify = TRUE) %>%
    replace_with_f() %>%
    paste0(collapse = " ") %>%
    remove_spaces_after_quotes() %>%
    add_closing_braces() %>%
    glue::glue()
  cat(output)
  invisible()
}


as_symbol <- function(x) paste0("$", x)

as_regex <- function(x) paste0(x, collapse = "|")

make_f <- function(x) ifelse(x == "x", "')}", paste0("{crayon::", x, "('"))

replace_with_f <- function(x) {
  ifelse(stringr::str_detect(x, "\\$\\$"), make_f(stringr::str_remove(x, "\\$\\$")), x)
}

add_closing_braces <- function(x) {
  num_opening_braces <- stringr::str_count(x, "\\{")
  num_closing_braces <- stringr::str_count(x, "\\}")
  if (num_closing_braces < num_opening_braces) {
    return(paste0(x, "')}"))
  } else {
    return(x)
  }
}

remove_spaces_after_quotes <- function(x) {
  x %>%
    stringr::str_replace_all("\\(' ", "\\('") %>%
    stringr::str_replace_all(" '\\)", "'\\)")
}
