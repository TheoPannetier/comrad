#' Diversity-dependent model with exponential (x) dependence on speciation and
#' constant (c) extinction – alternative formulation
#'
#' A list specifying a DD model with exponential diversity-dependence on the
#' speciation rate and constant-rate extinction; to be fed as argument
#' `dd_model` to [comrad::fit_dd_model()]. This second formulation of the model
#' corresponds to the exponential model found in `DDD` (`ddmodel = 2`)
#'
#' \deqn{\lambda(N) = \lambda_{0} \times N^{-\frac{log(\frac{\lambda_{0}}{\mu_{0}})}{log(K)}} \\ \mu(N) = \mu_{0}}
#'
#' @author Theo Pannetier
#' @export
dd_model_xc2 <- function() {
  list(
    "name" = "xc2",
    "speciation_func" = function (params, N) {
      params["lambda_0"] * N ^ (-log(params["lambda_0"] / params["mu_0"]) / log(params["k"]))
    },
    "extinction_func" = function (params, N) {
      params["mu_0"]
    },
    "constraints" = list(
      function(params, ...) params["lambda_0"] > 0,
      function(params, ...) params["mu_0"] > 0,
      function(params, ...) params["k"] >= 1,
      function(params, ...) params["lambda_0"] > params["mu_0"]
    ),
    "params_check" = function(params) {
      params_names <- names(params)
      if (!(length(params_names) == 3 &&
            all(params_names %in% c("lambda_0", "mu_0", "k"))
      )) {
        stop("params for ddmodel_xc should be \"lambda_0\", \"mu_0\" and \"k\".")
      }
    }
  )
}

