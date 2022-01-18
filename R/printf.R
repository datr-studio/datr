#' A fancy print function
#'
#' `printf()` provides an easy way to print fancy output without needing to
#' splice up your string with different function calls.
#'
#' `printf()` provides a simultaneous wrapper around both the \code{\link[glue]{glue}} and
#' \code{\link[crayon]{crayon}} libraries in a single string. This enables you to specify formtating (using crayons'
#' colour and emphasis functions) whilst also combining text with code to evaluation
#' (using glue).
#'
#'
#' @section How to use printf:
#' In short, use `$$` to signal a call to the crayon library. Calls can be nested with
#' further single `$` symbols. Close the call with `$$` (Optional for the final closing brace).
#'
#' Meanwhile, anything inside of curly braces (e.g. \{ \}) is evaluated first, enabling you
#' to use variables, function calls, and basically anything you can do in short R code.
#'
#' @param string String to format
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
#' printf("{var1} added to {var2} equals {var1 + var2}")
#' printf("Hello $$strikethrough mum $$ world!")
#' printf("$$blue this example shows that `printf()` will auto close final braces")
#' printf("This example shows that you can $$cyan$bold combine $$ formating")
#' printf("$$magenta$italic {var4} $$ $$green$underline brings it $$ $$blurred all together")
printf <- function(string) {
  # First Evaluate String as String
  output <- glue::glue(string, .envir = parent.frame()) %>%
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

make_f <- function(x) {
  # Determine whether it is an open or close call
  ifelse(stringr::str_detect(x, "^\\$\\$$"),
    "\")}", paste0("{crayon::", stringr::str_remove(x, "\\$\\$"), "(\"")
  )
}

replace_with_f <- function(x) {
  ifelse(stringr::str_detect(x, "\\$\\$"), make_f(x), x)
}

add_closing_braces <- function(x) {
  num_opening_braces <- stringr::str_count(x, "\\{")
  num_closing_braces <- stringr::str_count(x, "\\}")
  if (num_closing_braces < num_opening_braces) {
    return(paste0(x, "\")}"))
  } else {
    return(x)
  }
}

remove_spaces_after_quotes <- function(x) {
  x %>%
    stringr::str_replace_all("\\(\" ", "\\(\"") %>%
    stringr::str_replace_all(" \"\\)", "\"\\)")
}

# remove_extra_spaces <- function(x) stringr::str_replace_all(x, "\\s\\s", " ")
