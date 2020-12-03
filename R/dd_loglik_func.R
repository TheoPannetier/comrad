#' Log-likelihood function of a diversity-dependent model given waiting times
#'
#' Compute the log-likelihood of athe diversity-dependent model specified by
#' `speciation_func`, `extinction_func`, and `params` given a set of waiting
#' times to next events and associated diversity.
#' Meant to be called internally by [fit_dd_model()].
#'
#' @param times_tbl the output of [waiting_times()], a table containing the
#' waiting times to a next event, type of events, and species diversity.
#' @param params a named vector containing the values of the parameters of
#' `speciation_func` and `extinction_func`.
#' @param speciation_func a function describing how the rate of speciation varies
#' with the number of species `N`. Must take arguments `params` and `N`. Inside
#' the function, parameters must be referred as `params$param_name`.
#' @param extinction_func a function describing how the rate of extinction varies
#' with the number of species `N`. Must take arguments `params` and `N`. Inside
#' the function, parameters must be referred as `params$param_name`.
#'
#'@return a unique log-likelihood value
#'
#'@author Theo Pannetier
#'@export
#'
dd_loglik_func <- function(params,
                           times_tbl,
                           speciation_func,
                           extinction_func) {

  times_tbl$lambda_d <- speciation_func(params = params, N = times_tbl$N)
  times_tbl$mu_d <- extinction_func(params = params, N = times_tbl$N)

  times_tbl$log_lambda <- times_tbl$is_speciation * log(times_tbl$lambda_d)
  # if NaN (not speciation and lambda_d=0), should still be 0 bc not speciation
  times_tbl$log_lambda[is.nan(times_tbl$log_lambda)] <- 0

  times_tbl$log_mu <- (!times_tbl$is_speciation) * log(times_tbl$mu_d)
  # if NaN (not extinction and mu_d=0), should still be 0 bc not extinction
  times_tbl$log_mu[is.nan(times_tbl$log_mu)] <- 0

  times_tbl$probs <- times_tbl$log_lambda + times_tbl$log_mu -
    (times_tbl$lambda_d + times_tbl$mu_d) * times_tbl$N * times_tbl$waiting_time

  loglik <- sum(times_tbl$probs)

  return(loglik)
}
