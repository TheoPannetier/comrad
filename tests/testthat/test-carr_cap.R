context("test-carr_cap")

test_that("use", {
  expect_is(carr_cap(0.5, 0, 1000, 0.5), "numeric")
  expect_equal(carr_cap(0.5, 0.5, 1000, 0.5), 1000)
  expect_equal(carr_cap(20, 0.5, 1000, 0.5), 0)
  expect_equal(carr_cap(0.5, 0, 1000, 0.5), carr_cap(-0.5, 0, 1000, 0.5))

})

test_that("abuse", {
  expect_error(
    object = carr_cap("lysenko"),
    regexp = "'trait' must be numeric"
  )
  expect_error(
    object = carr_cap(TRUE),
    regexp = "'trait' must be numeric"
  )
})
