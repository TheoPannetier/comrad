#' Get the competition coefficient between two individuals
#'
#' Returns the competition coefficient \eqn{\alpha(zi, z_j)} between a focal
#' individual \eqn{i}, and a competitor \eqn{j} with trait values \eqn{z_i},
#' \eqn{z_j}.
#'
#' @param trait_ind numeric, the trait value \eqn{z_i} of the focal individual.
#' @param trait_comp numeric, the trait value \eqn{z_j} of a competitor.
#' @param sigma_comp numeric > 0. Controls the competition intensity, i.e. the
#' strength of negative feedbacks the focal indiviudal experiences for a given
#' trait distance with the competitor.
#'
#' @details The equation is reported from equation (3) in Pontarp et al.
#' (2012).
#' \deqn{\alpha(z_i, z_j) = exp-((z_i - z_j)^2 / 2 * \sigma^2_\alpha)}
#'
#' @author Theo Pannetier
#' @export

comp_coeff <- function(trait_ind, trait_comp, sigma_comp) {
  targ_num(trait_ind, "trait_ind")
  targ_num(trait_comp, "trait_comp")
  targ_pos(sigma_comp, "sigma_comp") # is a variance
  targ_not_zero(sigma_comp, "sigma_comp") # is the denominator
  if (trait_ind %in% c(Inf, -Inf) && trait_ind == trait_comp) {
    stop("'trait_ind' and 'trait_comp' cannot both be set to Inf or -Inf")
  }

  exp(-(trait_ind - trait_comp) ^ 2 / (2 * sigma_comp))
}
