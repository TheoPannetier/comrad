#' Diversity-dependent model with exponential (x) dependence on speciation and
#' exponential (x) dependence on extinction – alternative formulation
#'
#' A list specifying a DD model with exponential diversity-dependence on both the
#' speciation rate and extinction rate; to be fed as argument `dd_model` to
#' [comrad::fit_dd_model()]. This formulation corresponds to the exponential model
#' found in `DDD` (`ddmodel = 2`) and elsewhere (`BAMM`).
#'
#'\deqn{\lambda(N) = \lambda_{0} \times N^{-\frac{log\Big(\frac{\lambda_0}{\alpha(\lambda_0 - \mu_0) + \mu_0}\Big)}{log(K)}}}
#'\deqn{\mu(N) = \mu_{0} \times N^{\frac{log\Big(\alpha\frac{\lambda_0 - \mu_0}{\lambda_0}+1\Big)}{log(K)}}}
#'
#' @author Theo Pannetier
#' @export
dd_model_xx2 <- function() {
  list(
    "name" = "xx2",
    "speciation_func" = function(params, N) {
      x <- log(params["lambda_0"] / (params["alpha"] * (params["lambda_0"] - params["mu_0"]) + params["mu_0"])) / log(params["k"])
      params["lambda_0"] * N ^ (-x)
    },
    "extinction_func" = function(params, N) {
      x <- log(1 + params["alpha"] * (params["lambda_0"] - params["mu_0"]) / params["mu_0"]) / log(params["k"])
      params["mu_0"] * (N ^ x)
    },
    "constraints" = list(
      function(params, ...) params["lambda_0"] > 0,
      function(params, ...) params["mu_0"] >= 0,
      function(params, ...) params["k"] >= 1,
      function(params, ...) params["lambda_0"] > params["mu_0"],
      function(params, ...) params["alpha"] >= 0 & params["alpha"] <= 1
    ),
    "params_check" = function(params) {
      params_names <- names(params)
      if (!(length(params_names) == 4 &&
            all(params_names %in% c("lambda_0", "mu_0", "k", "alpha"))
      )) {
        stop("params for ddmodel_ll should be \"lambda_0\", \"mu_0\", \"k\" and \"alpha\".")
      }
    }
  )
}
