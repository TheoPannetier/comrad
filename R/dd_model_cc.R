#' Diversity-dependent model with constant-rate (c) speciation and constant-rate
#' (c) extinction
#'
#' A list specifying a DD model with constant-rate
#' speciation and constant-rate extinction; to be fed as argument
#' `dd_model` to [comrad::fit_dd_model_with_fossil()].
#'
#' \deqn{\lambda(N) = \lambda_{0} \\ \mu(N) = \mu_{0}}
#'
#' @author Theo Pannetier
#' @export
dd_model_cc <- function() {
  list(
    "name" = "cc",
    "speciation_func" = function(params, N) {
      rep(params["lambda_0"], length(N))
    },
    "extinction_func" = function(params, N) {
      rep(params["mu_0"], length(N))
    },
    "constraints" = list(
      function(params, ...) params["lambda_0"] > 0,
      function(params, ...) params["mu_0"] >= 0,
      function(params, ...) params["lambda_0"] > params["mu_0"]
    ),
    "params_check" = function(params) {
      params_names <- names(params)
      if (!(length(params_names) == 2 &&
            all(params_names %in% c("lambda_0", "mu_0"))
      )) {
        stop("params for ddmodel_cc should be \"lambda_0\" and \"mu_0\".")
      }
    },
    "DDD_name" = 0
  )
}
