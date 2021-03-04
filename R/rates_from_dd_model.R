#' Get speciation and extinction rates values given a DD model and parameters
#'
#' @param N_seq numeric vector, the values for which rate values are desired
#' @param dd_model a list with five named elements that together specify the
#' diversity-dependent model:
#' * `name` a two-letter code, the name of the model. First letter specifies the
#' form of the speciation function, second letter the form of the extinction
#' function: "l" for "linear", "x" for exponential, "c" for constant.
#' * `speciation_func`, a function specifying the diversity-dependent speciation
#' rate. Must take arguments `params` and `N`.
#' * `extinction_func`, a function specifying the diversity-dependent extinction
#' rate. Must take arguments `params` and `N`.
#' * `constraints` a list of conditions that parameter values must satisfy. Each
#' element is a function that takes arguments `params` and `...`, and returns
#' `TRUE` if the constraint is satisfied, `FALSE` if it isn't.
#' * `params_check` a function that controls the format of `params`. Returns an
#' error if the elements of `params` are named differently from what is expected
#' or if the length differs from the expectation.
#'
#' `comrad` contains several `dd_model` functions, see for example
#' [comrad::dd_model_lc()].
#'
#' @param params a named vector containing the values of the parameters of
#' `speciation_func` and `extinction_func`.
#'
#' @return a `tibble` with variables `N`, `speciation_rate` and `extinction_rate`
#' @author Théo Pannetier
#' @export
#'
rates_from_dd_model <- function(N_seq, dd_model, params) {
  dd_model$params_check(params)
  rates_tbl <- tibble::tibble(
    "N" = N_seq,
    "speciation_rate" = dd_model$speciation_func(params, N_seq),
    "extinction_rate" = dd_model$extinction_func(params, N_seq)
  )
  return(rates_tbl)
}
