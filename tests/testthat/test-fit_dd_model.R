context("test-fit_dd_model")

# Test optimisation runs normally for simple DD tree
set.seed(359)
tol_rel_err_lambda_0 <- 1
tol_rel_err_mu_0 <- 1
tol_rel_err_k <- 2

## DD model LC
dd_model <- dd_model_lc()
init_params <- c("lambda_0" = 1.5, "mu_0" = 0.8, "k" = 20)

expect_silent(
  # simulation returns without error and produces phylo
  dd_phylo <- simulate_dd_phylo(
    params = init_params,
    nb_gens = 100,
    dd_model = dd_model,
    stem_or_crown = "crown"
  )
)

expect_true(ape::is.rooted.phylo(dd_phylo))
#dd_sim_output <- DDD::dd_sim(pars = init_params, age = 10, ddmodel = 1)

test_that("fit DD with fossil lineages", {
  wt_tbl <- comrad::waiting_times(dd_phylo)
  ml_tbl <- comrad::fit_dd_model_with_fossil(
    waiting_times_tbl = wt_tbl,
    dd_model = dd_model,
    init_params = init_params
  ) %>%
    dplyr::mutate(
      "rel_err_lambda_0" = abs(init_lambda_0 - ml_lambda_0) / init_lambda_0,
      "rel_err_mu_0" = abs(init_mu_0 - ml_mu_0) / init_mu_0,
      "rel_err_k" = abs(init_k - ml_k) / init_k
    )

  expect_lt(ml_tbl$rel_err_lambda_0, tol_rel_err_lambda_0)
  expect_lt(ml_tbl$rel_err_mu_0, tol_rel_err_mu_0)
  expect_lt(ml_tbl$rel_err_k, tol_rel_err_k)
})

test_that("fit DD without fossil lineages", {
  branching_times <- dd_phylo %>%
    ape::drop.fossil() %>%
    ape::branching.times()

  ml_tbl <- fit_dd_model_without_fossil(
    branching_times = branching_times,
    dd_model = dd_model,
    init_params = init_params
  ) %>%
    dplyr::mutate(
      "rel_err_lambda_0" = abs(init_lambda_0 - ml_lambda) / init_lambda_0,
      "rel_err_mu_0" = abs(init_mu_0 - ml_mu) / init_mu_0,
      "rel_err_k" = abs(init_k - ml_k) / init_k,
    )
  expect_lt(ml_tbl$rel_err_lambda_0, tol_rel_err_lambda_0)
  expect_lt(ml_tbl$rel_err_mu_0, tol_rel_err_mu_0)
  expect_lt(ml_tbl$rel_err_k, tol_rel_err_k)
})
