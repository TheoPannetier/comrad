context("test-get_n_eff")

test_pop <- c(-0.39, 0.25, 1.3)

test_that("use", {
  # Ordinary cases
  expect_equal(
    get_n_eff(test_pop, comp_width = sqrt(0.5)),
    c(1.7214083, 1.995956, 1.389532), tolerance = 1e-6
    )
  # Border cases
  expect_equal(get_n_eff(rep(Inf, 2), 0), rep(2, 2))
  expect_equal(get_n_eff(c(Inf, -Inf), 0), rep(1, 2))
  expect_equal(get_n_eff(rep(Inf, 2), Inf), rep(2, 2))
  expect_equal(get_n_eff(c(Inf, -Inf), Inf), rep(2, 2))
})

test_that("abuse", {
  expect_error(get_n_eff("1.6"), "'traits_pop' must be numeric")
  expect_error(get_n_eff(NA), "'traits_pop' must be numeric")
  expect_error(get_n_eff(NaN), "'traits_pop' contains one or more NaNs")
  expect_error(get_n_eff(integer()), "'traits_pop' is empty")
})
