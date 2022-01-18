test_that("relative_change() works", {
  before <- 1
  after <- 2
  expect_equal(relative_change(before, after), 1)
  expect_equal(relative_change(before, after, as_perc = TRUE), 100)
  expect_equal(relative_change(before, after, as_perc = TRUE, as_str = TRUE), "100%")
})
