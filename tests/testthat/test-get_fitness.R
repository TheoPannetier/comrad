context("test-get_fitness")

test_pop <- c(-0.39, 0.25, 1.3)

test_that("use", {
  # Regular cases
  expect_equal(get_fitness(test_pop, comp_width = 0.5),
               c(2.711946, 2.712141, 2.609578), tolerance = 1e-6)
  # Border cases
  expect_equal(get_fitness(c(Inf, -Inf), carr_cap_opt = 0), c(0, 0))
  expect_equal(get_fitness(rep(0.5, 5), carr_cap_opt = 0), rep(0, 5))
  expect_equal(get_fitness(c(Inf, -Inf), carr_cap_opt = Inf), rep(exp(1), 2))
  expect_equal(get_fitness(rep(0.5, 5), carr_cap_opt = Inf), rep(exp(1), 5))
  expect_equal(
    get_fitness(rep(0.5, 5), carr_cap_opt = 0, growth_rate = 0), rep(0, 5)
    )
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
    object = get_fitness(rep(0.5, 3), comp_width = TRUE),
    regexp = "'comp_width' must be numeric",
  )
  expect_error(
    object = get_fitness(rep(0.5, 3), trait_opt = "x"),
    regexp = "'trait_opt' must be numeric",
  )
  expect_error(
    object = get_fitness(rep(0.5, 3), carr_cap_opt = -1),
    regexp = "'carr_cap_opt' must be a positive numeric",
  )
  expect_error(
    object = get_fitness(rep(0.5, 3), carr_cap_width = "elmo"),
    regexp = "'carr_cap_width' must be numeric",
  )


})
