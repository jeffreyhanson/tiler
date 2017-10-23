context("is_valid_zoom")

test_that("valid zoom data", {
  expect_true(is_valid_zoom("10"))
  expect_true(is_valid_zoom("1-10"))
})

test_that("invalid zoom data", {
  expect_false(is_valid_zoom("A"))
  expect_false(is_valid_zoom("10.1"))
  expect_false(is_valid_zoom("1-10.1"))
})
