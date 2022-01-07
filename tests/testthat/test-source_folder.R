test_that("source_folder() throws error on empty folder name", {
  expect_error(source_folder(""))
})

test_that("source_folder() throws error on more than one dir", {
  expect_error(source_folder(c("a", "b")))
})

test_that("source_folder() throws error on non-existent dir", {
  expect_error(source_folder("doesnt_exist"))
})