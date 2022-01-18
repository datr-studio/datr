test_that("printf captures environments correctly", {
  f <- function(x) printf("$$magenta Testing {x}")
  expect_output(f("environments"))
})

test_that("Tricky printf scenarios", {
  skip("Haven't fixed these bugs yet")
  quoted <- "quoted text"
  expect_output(printf("$$blue this contains a '{quoted}' example"))
})
