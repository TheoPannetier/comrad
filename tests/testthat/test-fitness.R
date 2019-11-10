context("test-fitness")

test_that("use", {

})

test_that("abuse", {
  expect_is(fitness(0.5, 0, 1000, 0.5), "numeric")

})
