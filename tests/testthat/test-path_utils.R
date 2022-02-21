test_that("load_clean/raw nicely opens a file", {
  reference <- tibble::tribble(
    ~test, ~value,
    "abc", 1
  )
  oldpath <- getwd()
  setwd("../../")
  expect_equal(load_tidy("cleanfile.csv"), reference)
  expect_equal(load_tidy("cleanfile"), reference)
  expect_equal(load_tidy("subfolder/to/cleansubfile"), reference)
  expect_equal(load_tidy("cleanfeather"), reference)
  expect_equal(load_raw("subfolder/to/rawsubfile"), reference)
  expect_equal(load_raw("rawfile"), reference)
  expect_equal(load_raw("rawexcel"), reference)
  setwd(oldpath)
})

