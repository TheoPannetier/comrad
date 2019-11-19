context("test-comp_coeff")

test_that("use", {
  # Ordinary cases
  expect_equal(
    comp_coeff(0.5, 0.7, 0.2),
    0.9048374,
    tolerance = 1e-07
    )
  expect_equal(comp_coeff(0.5, 0.5, 0.2), 1) # same trait case
  expect_equal(comp_coeff(-5, 5, 0.2), 0) # distant trait case
  expect_equal(comp_coeff(5, -1, 0.2), comp_coeff(-5, 1, 0.2)) # symmetry
  expect_equal(comp_coeff(3.5, -0.39, 1.2), 0.00182707)
  # Border cases of trait_dist and sigma_comp
  expect_equal(comp_coeff(rep(0, 5), rep(0, 5), 0), rep(1, 5))
  expect_equal(comp_coeff(rep(0, 5), rep(0, 5), Inf), rep(1, 5))
  expect_equal(comp_coeff(rep(Inf, 5), rep(0, 5), 0), rep(0, 5))
  expect_equal(comp_coeff(rep(Inf, 5), rep(0, 5), Inf), rep(1, 5))
})

test_that("abuse", {

  expect_error(
    object = comp_coeff(TRUE, -0.3, 0.2),
    regexp = "'trait_ind' must be numeric"
  )
  expect_error(
    object = comp_coeff(0.5, -0.3, NaN),
    regexp = "'sigma_comp' contains one or more NaNs"
  )
  expect_error(
    object = comp_coeff(0.5, "my_old_man", 0.2),
    regexp = "'trait_comp' must be numeric"
  )
  expect_error(
    object = comp_coeff(0.5, 0.1, "this_old_dog"),
    regexp = "'sigma_comp' must be numeric"
  )
  expect_error(
    object = comp_coeff(0.5, 0.7, -1),
    regexp = "'sigma_comp' must be a positive numeric"
  )
})
