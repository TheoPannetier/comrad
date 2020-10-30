context("test-rate_estim")

test_that("simultaneous speciation", {

  #                       |----2000----|
  #           |---2500----|
  #           |           |----2000----|
  # --1000----|
  #           |           |----2000----|
  #           |---2500----|
  #                       |----2000----|

  phylo <- ape::read.tree(
    text = "(((A:2000, B:2000):2500, (C:2000, D:2000):2500):1000);"
  )
  # Five placeholder replicates
  multi_phylo <- purrr::map(1:5, function(x) phylo)
  rates_tbl <- multi_phylo %>% estimate_dd_rates()
  exptd_tbl <- tibble::tibble(
    "N" = 1:3,
    "speciation_rate" = c(1e-03, 2e-04, NA),
    "extinction_rate" = rep(as.numeric(NA), 3)
  )
  expect_equal(
    rates_tbl, exptd_tbl
  )
})
