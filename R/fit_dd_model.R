#' Fit a diversity-dependent diversification model to phylogenies
#'
#' Fits the parameters of a specified diversity-dependent model to  a set of
#' full phylogenies produced by `comrad` simulations
#'
#' @param phylos a list of `phylo` objects, typically the output of
#' [sim_to_phylo()]. The phylogenies must be complete, i.e extinct species must
#' be included.
#' @param init_params named vector, initial values of the parameters to optimise.
#' Names must match the parameters called in `speciation_func` and
#' `extinction_func`.
#' @param speciation_func a function describing how the rate of speciation varies
#' with the number of species `N`. Must take arguments `params` and `N`. Inside
#' the function, parameters must be referred as `params$param_name`.
#' @param extinction_func a function describing how the rate of extinction varies
#' with the number of species `N`. Must take arguments `params` and `N`. Inside
#' the function, parameters must be referred as `params$param_name`.
#'
#' @return a one-row table with maximum likelihood estimates for the values of
#' the parameters, the maximum likelihood and a convergence code (see
#' [subplex::subplex()] for the meaning of this code)
#'
#' @author Theo Pannetier
#' @export
#'
fit_dd_model <- function(phylos, init_params, speciation_func, extinction_func) {
  # Assemble data set
  times_tbl <- phylos %>%
    purrr::map_dfr(comrad::waiting_times, .id = "replicate")

  trparsopt <- init_params %>% DDD::transform_pars()

  # Declare function to optimise
  fun <- function(trparsopt) {
    if (min(trparsopt) < 0 || max(trparsopt) > 1) return(-Inf)
    params <- DDD::untransform_pars(trparsopt)
    loglik <- dd_loglik_func(
      times_tbl = times_tbl,
      params = params,
      speciation_func = speciation_func,
      extinction_func = extinction_func
    )
    return(loglik)
  }

  # Run maximum likelihood optimisation
  ml_output <- DDD::optimizer(
    optimmethod = 'subplex',
    fun = fun,
    trparsopt = trparsopt
  )

  # Format output
  loglik_tbl <- ml_output %>%
    tibble::as_tibble() %>%
    dplyr::select(par) %>%
    dplyr::mutate(
      "par" = par %>% DDD::untransform_pars(),
      "params" = names(par)) %>%
    tidyr::pivot_wider(names_from = params, values_from = par) %>%
    dplyr::mutate(
      "loglik" = ml_output$fvalues,
      "conv" = ml_output$conv
    )

  return(loglik_tbl)
}
