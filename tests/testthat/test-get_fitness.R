context("test-get_fitness")

test_that("use", {
  # Regular cases
  # Border cases
})

test_that("abuse", {
  expect_error(
    object = get_fitness("doom"),
    regexp = "'traits_pop' must be numeric",
  )
  expect_error(
    object = get_fitness(NaN),
    regexp = "'traits_pop' contains one or more NaNs",
  )
  expect_error(
    object = get_fitness(rep(0.5, 3), growth_rate = TRUE),
    regexp = "'growth_rate' must be numeric",
  )
  expect_error(
    object = get_fitness(rep(0.5, 3), sigma_comp = TRUE),
    regexp = "'sigma_comp' must be numeric",
  )
  expect_error(
    object = get_fitness(rep(0.5, 3), carr_cap_pars = c("x", 1000, 0.5)),
    regexp = "'carr_cap_pars' must be numeric",
  )
  expect_error(
    object = get_fitness(rep(0.5, 3), carr_cap_pars = c(0, -1, 0.5)),
    regexp = "'carr_cap_opt' must be a positive numeric",
  )
  expect_error(
    object = get_fitness(rep(0.5, 3), carr_cap_pars = c(0, 1000, "elmo")),
    regexp = "'carr_cap_pars' must be numeric",
  )


})
