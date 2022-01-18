test_that("printf captures environments correctly", {
  f <- function(x) printf("$$magenta Testing {x}")
  expect_output(f("environments"))
})

test_that("Tricky printf scenarios", {
  var1 <- 1
  var2 <- 2
  var3 <- "world"
  var4 <- "this example"
  expect_output(printf("{var1} added to {var2} equals {var1 + var2}"))
  expect_output(printf("Hello $$strikethrough mum $$ world!"))
  expect_output(printf("$$blue this example shows that `printf()` will auto close final braces"))
  expect_output(printf("This example shows that you can $$cyan$bold combine $$ formating"))
  expect_output(printf("$$magenta$italic {var4} $$ $$green$underline brings it $$ $$blurred all together"))
  expect_output(printf("$$blue this contains a '{1+1}' example"))
})

test_that("make_f correctly opens and closes functions", {
  expect_equal(make_f("$$"), "\")}")
  expect_equal(make_f("$$bold"), "{crayon::bold(\"")
})
