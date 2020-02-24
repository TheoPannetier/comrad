#' Get the competition coefficients between an individual and the rest of the
#' community
#'
#' Returns the competition coefficients \eqn{\alpha(z(i), z(j)}{\alpha(z_i, z_j}
#' between a focal individual \eqn{i}, and each individual \eqn{j} in the
#' community, including the focal individual itself.
#'
#' @inheritParams default_params_doc
#'
#' @return a numeric vector containing the competitive effect caused by each
#' individual in the community on the focal individual.
#'
#' @author Theo Pannetier
#' @export

get_comp_coeff_comm <- function(
  trait_ind,
  traits_comm,
  comp_width = default_comp_width()) {

  comrad::testarg_length(trait_ind, 1) # not vectorized!

  trait_dist <- (trait_ind - traits_comm) ^ 2

  coeffs <- exp(- (trait_dist / (2 * comp_width ^ 2)))

  # Solve possible NaN issues --------------------------------------------------
  # NaNs can arise if both terms in the division are equal to 0 or Inf
  if (comp_width == 0) {
    coeffs[which(trait_dist == 0)] <- 0 # as if trait_dist / comp_width = 0
  }
  coeffs
}
