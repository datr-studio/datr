test_that("printf captures environments correctly", {
  # printf fails to capture arguments
  f <- function(x) printf("$$magenta Testing {x}")
  expect_output(f("environments"))
})
