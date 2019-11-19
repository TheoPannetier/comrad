context("test-get_n_eff")

test_that("use", {
  # Ordinary cases

  # Border cases
  expect_equal(get_n_eff(rep(Inf, 2), 0), rep(2, 2))
  expect_equal(get_n_eff(c(Inf, -Inf), 0), rep(1, 2))
  expect_equal(get_n_eff(rep(Inf, 2), Inf), rep(2, 2))
  expect_equal(get_n_eff(c(Inf, -Inf), Inf), rep(2, 2))
})

test_that("abuse", {

})
