context("test-get_comp_coeffs")

test_pop <- c(-0.39, 0.25, 1.3)

test_that("use", {
  # Ordinary cases
  expect_equal(
    get_comp_coeffs(0.5, 0.7, 0.2),
    0.9048374,
    tolerance = 1e-07
    )
  expect_equal(get_comp_coeffs(0.5, 0.5, 0.2), 1) # same trait case
  expect_equal(get_comp_coeffs(-5, 5, 0.2), 0) # distant trait case
  expect_equal(
    get_comp_coeffs(5, -1, 0.2),
    get_comp_coeffs(-5, 1, 0.2)
    ) # symmetry
  expect_equal(get_comp_coeffs(3.5, -0.39, 1.2), 0.00182707)
  # with test_pop
  expect_equal(
    get_comp_coeffs(test_pop[1], test_pop, comp_width = 0.5),
    c(1, 0.6639158, 0.05749254), tolerance = 1e-7
    )
  expect_equal(
    get_comp_coeffs(test_pop[2], test_pop, comp_width = 0.5),
    c(0.6639158, 1, 0.3320399), tolerance = 1e-7
  )
  expect_equal(
    get_comp_coeffs(test_pop[3], test_pop, comp_width = 0.5),
    c(0.05749254, 0.3320399, 1), tolerance = 1e-7
  )

  # Border cases of trait_dist and sigma_comp
  expect_equal(get_comp_coeffs(0, rep(0, 5), 0), rep(1, 5))
  expect_equal(get_comp_coeffs(0, rep(0, 5), Inf), rep(1, 5))
  expect_equal(get_comp_coeffs(Inf, rep(0, 5), 0), rep(0, 5))
  expect_equal(get_comp_coeffs(Inf, rep(0, 5), Inf), rep(1, 5))
})

test_that("abuse", {

  expect_error(
    object = get_comp_coeffs(TRUE, -0.3, 0.2),
    regexp = "'trait_ind' must be numeric"
  )
  expect_error(
    object = get_comp_coeffs(0.5, -0.3, NaN),
    regexp = "'comp_width' contains one or more NaNs"
  )
  expect_error(
    object = get_comp_coeffs(0.5, "my_old_man", 0.2),
    regexp = "'traits_pop' must be numeric"
  )
  expect_error(
    object = get_comp_coeffs(0.5, 0.1, "this_old_dog"),
    regexp = "'comp_width' must be numeric"
  )
  expect_error(
    object = get_comp_coeffs(0.5, 0.7, -1),
    regexp = "'comp_width' must be a positive numeric"
  )
})
