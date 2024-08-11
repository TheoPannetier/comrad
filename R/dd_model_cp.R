#' Diversity-dependent model with constant-rate (c) speciation and
#' power (p) dependence on extinction
#'
#' A list specifying a DD model with constant-rate speciation
#' and power diversity-dependence on the extinction rate; to be
#' fed as argument `dd_model` to [comrad::fit_dd_model_with_fossil()].
#'
#'\deqn{\lambda(N) = \lambda_{0}}
#'\deqn{\mu(N) = \mu_{0} \times N^{\frac{log\Big(\frac{\lambda_0 - \mu_0}{\lambda_0}+1\Big)}{log(K)}}}
#'
#' @author Theo Pannetier
#' @export
dd_model_cp <- function() {
  list(
    "name" = "cp",
    "speciation_func" = function(params, N) {
      params["lambda_0"]
    },
    "extinction_func" = function(params, N) {
      x <- log(1 + (params["lambda_0"] - params["mu_0"]) / params["mu_0"]) / log(params["k"])
      params["mu_0"] * (N ^ x)
    },
    "constraints" = list(
      function(params, ...) params["lambda_0"] > 0,
      function(params, ...) params["mu_0"] >= 0,
      function(params, ...) params["k"] >= 1,
      function(params, ...) params["lambda_0"] > params["mu_0"],
    ),
    "params_check" = function(params) {
      params_names <- names(params)
      if (!(length(params_names) == 3 &&
            all(params_names %in% c("lambda_0", "mu_0", "k"))
      )) {
        stop("params for ddmodel_cp should be \"lambda_0\", \"mu_0\" and \"k\".")
      }
    },
    "DDD_name" = 4
  )
}
