#' Diversity-dependent model with linear (l) dependence on speciation and
#' exponential (x) dependence on extinction – alternative formulation
#'
#' A list specifying a DD model with linear diversity-dependence on the
#' speciation rate and exponential diversity-dependence on the extinction rate;
#' to be fed as argument `dd_model` to [comrad::fit_dd_model()].
#' This formulation corresponds to the exponential model
#' found in `DDD` (`ddmodel = 2`) and elsewhere (`BAMM`).
#'
#' \deqn{\lambda(N) = \lambda_{0} - (1 - \alpha)(\lambda_{0} - \mu_{0}) \frac{N}{K}}
#' \deqn{\mu(N) = \mu_{0}((1 - \alpha) + \alpha \frac{\lambda_{0}}{\mu_{0}})^{\frac{N}{K}}}
#'
#' @author Theo Pannetier
#' @export
dd_model_lx2 <- function() {
  list(
    "name" = "lx2",
    "speciation_func" = function(params, N) {
      pmax(
        params["lambda_0"] - (1 - params["alpha"]) * (params["lambda_0"] - params["mu_0"]) * (N / params["k"]),
        0
      )
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
      function(params, ...) params["alpha"] >= 0 & params["alpha"] <= 1,
      function(params, N_max, ...) {
        kprime <- ceiling(params["k"] * params["lambda_0"] /
                            (params["lambda_0"] - params["mu_0"]) / (1 - params["alpha"]))
        N_max <= kprime
      }
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
