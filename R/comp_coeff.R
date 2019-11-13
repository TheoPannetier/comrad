#' Get the competition coefficient between two individuals
#'
#' Returns the competition coefficient \eqn{\alpha(zi, z_j)} between a focal
#' individual \eqn{i}, and a competitor \eqn{j} with trait values \eqn{z_i},
#' \eqn{z_j}.
#'
#' @inheritParams default_params_doc
#'
#' @details The equation is reported from equation (3) in Pontarp et al.
#' (2012).
#' \deqn{\alpha(z_i, z_j) = exp(-(z_i - z_j) ^ 2 / (2 * \sigma^2_\alpha))}
#'
#' @author Theo Pannetier
#' @export

comp_coeff <- function(
  trait_ind,
  trait_comp,
  sigma_comp = default_sigma_comp()) {

  testarg_num(trait_ind)
  testarg_num(trait_comp)
  testarg_num(sigma_comp)
  testarg_pos(sigma_comp) # is a variance
  testarg_not_this(sigma_comp, c(0, Inf)) # is the denominator
  if (trait_ind %in% c(Inf, -Inf) && trait_ind == trait_comp) {
    stop("'trait_ind' and 'trait_comp' cannot both be set to Inf or -Inf")
  } # causes a NaN

  exp(-((trait_ind - trait_comp) ^ 2 / (2 * sigma_comp)))
}
