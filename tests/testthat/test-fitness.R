context("test-fitness")

zi <- 0.4
zj_pop <- c(0.3, 0.6, 0.8)

test_that("use", {
  expect_equal(fitness(zi, zj_pop), 2.710158, tolerance = 1e-07) # manual result
  expect_equal(fitness(0, rep(0, 1000)), 1) # 1000 competitors with same z
  expect_equal(fitness(0, rep(0, 5), carr_cap_pars = c(0, 5, 0.5)), 1) # less
})

test_that("abuse", {
  expect_error(
    object = fitness("doom", zj_pop),
    regexp = "'trait_ind' must be numeric",
  )
  expect_error(
    object = fitness(NaN, zj_pop),
    regexp = "'trait_ind' contains one or more NaNs",
  )
  expect_error(
    object = fitness(zi, "cirice"),
    regexp = "'traits_pop' must be numeric",
  )
  expect_error(
    object = fitness(zi, zj_pop, growth_rate = TRUE),
    regexp = "'growth_rate' must be numeric",
  )
  expect_error(
    object = fitness(zi, zj_pop, sigma_comp = TRUE),
    regexp = "'sigma_comp' must be numeric",
  )
  expect_error(
    object = fitness(zi, zj_pop, carr_cap_pars = c("x", 1000, 0.5)),
    regexp = "'carr_cap_pars' must be numeric",
  )
  expect_error(
    object = fitness(zi, zj_pop, carr_cap_pars = c(0, -1, 0.5)),
    regexp = "'carr_cap_opt' must be a positive numeric",
  )
  expect_error(
    object = fitness(zi, zj_pop, carr_cap_pars = c(0, 1000, "elmo")),
    regexp = "'carr_cap_pars' must be numeric",
  )


})
