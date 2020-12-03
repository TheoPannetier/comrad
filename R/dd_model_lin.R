#' Linear diversity-dependent speciation and extinction rate
#'
#' @export
#'
dd_model_lin <- function(){
  list(
    "speciation_func" = function(params, N) {
      pmax(params["lambda_0"] * (1 - N / params["k"]), 0)
    },
    "extinction_func" = function(params, N) {
      pmax(params["mu_0"], 0)
    }
  )

}
