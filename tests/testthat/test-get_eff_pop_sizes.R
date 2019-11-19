context("test-get_eff_pop_sizes")

test_that("use", {
  # Ordinary cases

  # Border cases
  expect_equal(get_eff_pop_sizes(rep(Inf, 2), 0), rep(2, 2))
  expect_equal(get_eff_pop_sizes(c(Inf, -Inf), 0), rep(1, 2))
  expect_equal(get_eff_pop_sizes(rep(Inf, 2), Inf), rep(2, 2))
  expect_equal(get_eff_pop_sizes(c(Inf, -Inf), Inf), rep(2, 2))
})

test_that("abuse", {

})
