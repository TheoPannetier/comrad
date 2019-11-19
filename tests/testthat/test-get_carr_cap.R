context("test-get_carr_cap")

test_that("use", {
  # Ordinary cases
  expect_equal(get_carr_cap(0, 0, 1000, 0.5), 1000)
  expect_equal(get_carr_cap(20, 0.5, 1000, 0.5), 0)
  expect_equal(
    get_carr_cap(0.5, 0, 1000, 0.5),
    get_carr_cap(-0.5, 0, 1000, 0.5)
    )
  # Border cases of trait_dist and get_carr_cap_var
  expect_equal(get_carr_cap(rep(0, 5), 0, 1000, 0), rep(1000, 5))
  expect_equal(get_carr_cap(rep(0, 5), 0, 1000, Inf), rep(1000, 5))
  expect_equal(get_carr_cap(rep(Inf, 5), 0, 1000, 0), rep(0, 5))
  expect_equal(get_carr_cap(rep(Inf, 5), 0, 1000, Inf), rep(1000, 5))
  # Border cases of get_carr_cap_opt
  expect_equal(get_carr_cap(rep(0, 5), 0, 0, 0), rep(0, 5))
  expect_equal(get_carr_cap(rep(0, 5), 0, Inf, Inf), rep(Inf, 5))
  # Border case of z - z_opt
  expect_equal(get_carr_cap(rep(Inf, 5), Inf, 1000, 0), rep(1000, 5))
})

test_that("abuse", {
  expect_error(
    object = get_carr_cap("lysenko"),
    regexp = "'trait_ind' must be numeric"
  )
  expect_error(
    object = get_carr_cap(TRUE),
    regexp = "'trait_ind' must be numeric"
  )
})
