context("test-comp_coeff")

test_that("use", {
  expect_equal(comp_coeff(0.5, 0.7, 0.2), 0.9048374, tolerance = 1e-07) # basic case #1
  expect_equal(comp_coeff(3.5, -0.39, 1.2), 0.00182707) # basic case #2
  expect_equal(comp_coeff(0.5, 0.5, 0.2), 1) # same trait case
  expect_equal(comp_coeff(-5, 5, 0.2), 0) # distant trait case
  expect_equal(comp_coeff(5, -1, 0.2), comp_coeff(-5, 1, 0.2)) # symmetry
  expect_equal(comp_coeff(5, -1, 0.2), comp_coeff(-1, 5, 0.2)) # symmetry
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
  expect_error(
    object = comp_coeff(Inf, Inf, 0.2),
    regexp = "'trait_ind' and 'trait_comp' cannot both be set to Inf or -Inf"
  )
  expect_error(
    object = comp_coeff(0.5, -0.3, 0),
    regexp = paste("'sigma_comp' contains forbidden values: 0")
  )
})
