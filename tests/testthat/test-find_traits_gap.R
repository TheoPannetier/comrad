context("test-find_trait_gaps")

trait_dist_sp <- 0.1

test_that("use", {
  expect_equal(
    find_trait_gaps(rep(0.1, 10), trait_dist_sp), integer(0)
  )
  expect_equal(
    find_trait_gaps(seq(-0.5, 0.5, by = 0.05), trait_dist_sp), integer(0)
  )

  z <- seq(-0.5, 0.5, by = 0.1)
  tol <- 1e09
  expect_equal(
    # ok
    find_trait_gaps(z, 0.099999999999), 1:10
  )
  expect_equal(
    # ok
    find_trait_gaps(z, 0.1), 1:10
  )
  expect_equal(
    # ok
    find_trait_gaps(z, 0.1 - tol), 1:10
  )
  expect_equal(
    # ok
    find_trait_gaps(z, 0.1 + tol), integer(0)
  )
  expect_equal(
    find_trait_gaps(c(rep(-Inf, 3), rep(Inf, 3)), trait_dist_sp), 3
  )
  # no split from rounding distances
  expect_equal(
    find_trait_gaps(c(0, 0.06), 0.1), integer(0)
  )
})
