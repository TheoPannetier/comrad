context("test-get_n_eff")

competition_sd <- sqrt(0.5) # so denom ~= 1

test_that("use", {
  # Ordinary cases
  expect_equal(
    get_n_eff(rep(0.8, 27), competition_sd = competition_sd),
    rep(27, 27)
  )
  z1 <- 0.1
  z2 <- 0.2
  z3 <- 0.4
  z1_z2 <- exp(-0.01)
  z2_z3 <- exp(-0.04)
  z1_z3 <- exp(-0.09)
  expect_equal(
    get_n_eff(c(z1, z2, z3), competition_sd = competition_sd),
    c(
      1 + z1_z2 + z1_z3,
      z1_z2 + 1 + z2_z3,
      z1_z3 + z2_z3 + 1
    ), tolerance = 1e-07
  )

})
