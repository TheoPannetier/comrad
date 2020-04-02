context("test-rate_estim")

test_that("waiting_times", {
  # Fixed string, fixed tree
  # "Small" CR tree with 4 final tips, includes an extinction event
  # Simulated with DDD::dd_sim(c(0.2, 0.1, Inf), 5)
  newick_string <- "(((t1:0.2965557774,t5:0.2965557774):4.105982616,t3:
  2.675245559):0.5974616064,(t2:0.5634109445,t4:0.5634109445):4.436589055):0;"
  phylo <- ape::read.tree(text = newick_string)

  wt <- waiting_times(phylo)
  # Expected output
  wt_expected <- tibble::tibble(
    "time" = c(0, 0.5974616, 3.2727072, 4.4365891),
    "N" = c(2, 3, 2, 3),
    "waiting_time" = c(0.5974616, 2.6752456, 1.1638819, 0.2668552),
    "event" = c(
      "speciation", "extinction", "speciation", "speciation"
    )
  )
  # waiting_times abuse
  expect_error(
    waiting_times("not_a_phylo"),
    "'phylo' must be a binary 'phylo' object."
  )
  # polytomic tree
  polyphylo <- ape::read.tree(text = "((t1:2,t2:3,t3:2),t4:1);")
  expect_error(
    waiting_times(polyphylo),
    "'phylo' must be a binary 'phylo' object."
  )

})

