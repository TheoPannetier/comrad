#' Diversity-dependent model with constant-rate (c) speciation and
#' linear (l) dependence on extinction
#'
#' A list specifying a DD model with linear diversity-dependence on the
#' extinction rate; to be passed as argument
#' `dd_model` to [comrad::fit_dd_model_with_fossil()].
#'
#'\deqn{\lambda(N) = \lambda_{0}}
#'\deqn{\mu(N) = \mu_{0} + (\lambda_{0} - \mu_{0}) \frac{N}{K}}
#'
#' @author Theo Pannetier
#' @export
dd_model_cl <- function() {
  list(
    "name" = "cl",
    "speciation_func" = function(params, N) {
      pmax(
        params["lambda_0"],
        0
      )
    },
    "extinction_func" = function(params, N) {
      params["mu_0"] + (params["lambda_0"] - params["mu_0"]) * (N / params["k"])
    },
    "constraints" = list(
      function(params, ...) params["lambda_0"] > 0,
      function(params, ...) params["mu_0"] >= 0,
      function(params, ...) params["k"] >= 1,
      function(params, ...) params["lambda_0"] > params["mu_0"],
      function(params, N_max, ...) {
        kprime <- ceiling(params["k"] * params["lambda_0"] /
                            (params["lambda_0"] - params["mu_0"]))
        N_max <= kprime
      }
    ),
    "params_check" = function(params) {
      params_names <- names(params)
      if (!(length(params_names) == 3 &&
            all(params_names %in% c("lambda_0", "mu_0", "k"))
      )) {
        stop("params for ddmodel_cl should be \"lambda_0\", \"mu_0\" and \"k\" ")
      }
    },
    "DDD_name" = 3
  )
}
