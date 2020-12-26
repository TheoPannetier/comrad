#' Draw a set of initial parameter values for a DD model
#'
#' @param phylos the list of `phylo`` class objects on which the DD model is going
#'  to be fit.
#' @param nb_sets integer, the number of sets desired.
#'
#' @return a list of length-3 vectors containing initial values for `lambda_0`,
#' `mu_0` and `K`.
#'
#' @author Theo Pannetier
#' @export
draw_init_params_dd_ml <- function(phylos, nb_sets) {

  ltt_tbl <- comrad::get_ltt_tbl(phylos[[1]]) %>%
    dplyr::mutate("time" = time - min(time))
  n_max <- phylos %>%
    purrr::map(ape::drop.fossil) %>%
    purrr::map_int(ape::Ntip) %>%
    max()
  t_max <- max(ltt_tbl$time)
  proto_lambda0 <- log(n_max) / t_max

  # Initial parameter values
  lambdas <- stats::runif(nb_sets, proto_lambda0 * 0.5, proto_lambda0 * 2)
  mus <- stats::runif(nb_sets, 0, 0.75 * lambdas)
  ks <- trunc(n_max + n_max * stats::rgamma(100, shape = 0.5, scale = 0.5))

  init_params <- purrr::pmap(list(lambdas, mus, ks), function(lambda, mu, k) {
    c("lambda_0" = lambda, "mu_0" = mu, "k" = k)
  })
  return(init_params)
}
