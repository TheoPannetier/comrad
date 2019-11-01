context("is_prime")

test_that("use", {
  expect_true(is_prime(2))
  expect_false(is_prime(4))
})

test_that("abuse", {
  expect_error(
    object = is_prime("hello"),
    regexp = "'x' should be numeric"
  )
})
