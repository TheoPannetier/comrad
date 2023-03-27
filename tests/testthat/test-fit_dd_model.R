context("test-fit_dd_model")

# Test optimisation runs normally for simple DD tree
set.seed(359)
tol_rel_err_lambda_0 <- 2
tol_rel_err_mu_0 <- 3
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

test_that("All DD models are correct", {

  dd_models <- dd_models()

  params <- c("lambda_0" = 0.7, "mu_0" = 0.3, "k" = 30, "alpha" = 0.5)
  cat("Checking DD models:\n")
  dd_models() %>% purrr::walk(function(dd_model) {

    dd_name <- dd_model$name
    cat(dd_name, " \n")

    # Check parameter format
    both_rates_vary <- dd_model_to_extinction_func(dd_name) != "constant"
    if (!both_rates_vary) {
      params <- params[1:3]
    }
    dd_model$params_check(params)

    sp_rate_eq <- dd_model$speciation_func(params = params, N = params["k"])
    ext_rate_eq <- dd_model$extinction_func(params = params, N = params["k"])
    expect_equivalent(sp_rate_eq, ext_rate_eq)

    if (both_rates_vary) {
      exptd_eq_rate <- params["lambda_0"] * (1 - params["alpha"]) +
        params["mu_0"] * params["alpha"]
      expect_equivalent(sp_rate_eq, exptd_eq_rate)
    } else {
      expect_equivalent(sp_rate_eq, params["mu_0"])
    }

    are_constraints_ok <- function(constraints, params, ...) {
      constraints %>%
        purrr::map_lgl(
          function(func, params, ...) {
            func(params, ...)
          },
          params,
          N_max
        ) %>%
        all()
    }

    # Randomly drawn param values respect constraints
    N_max <- params["k"]
    random_init_params <- draw_init_params_dd_ml(
      nb_sets = 1,
      phylos = list(dd_phylo),
      dd_model = dd_model
    )[[1]]
    expect_true(
      are_constraints_ok(
        constraints = dd_model$constraints,
        params = random_init_params,
        N_max = N_max
      )
    )
  })
})

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

test_that("abuse initial parameter values", {
  wrong_params <- c("lambda_0" = -0.7, "mu_0" = 0.3, "k" = 30)
  wt_tbl <- comrad::waiting_times(dd_phylo)
  exptd_msg <- "The constraints of the model are not satisfied for the initial parameter values."
  expect_warning(comrad::fit_dd_model_with_fossil(
    waiting_times_tbl = wt_tbl,
    dd_model = dd_model,
    init_params = wrong_params
  ), exptd_msg)
})

test_that("fit DD without fossil lineages", {
  skip_if(Sys.getenv("CI") == "", message = "Run only on CI")

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

## DD model LL
dd_model <- dd_model_ll()
init_params <- c("lambda_0" = 1.5, "mu_0" = 0.8, "k" = 20, "alpha" = 0.5)
expect_silent(
  # simulation returns without error and produces phylo
  dd_phylo <- simulate_dd_phylo(
    params = init_params,
    nb_gens = 100,
    dd_model = dd_model,
    stem_or_crown = "crown"
  )
)
test_that("fit DD model LL", {
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
