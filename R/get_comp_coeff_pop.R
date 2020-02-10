#' Get the competition coefficients between an individual and the rest of the
#' population
#'
#' Returns the competition coefficients \eqn{\alpha(zi, z_j)} between a focal
#' individual \eqn{i}, and each individual \eqn{j} in the population, including
#' the focal individual itself.
#'
#' @inheritParams default_params_doc
#'
#' @details The equation is reported from equation (3) in Pontarp et al. (2012).
#' \deqn{\alpha(z_i, z_j) = exp(-(z_i - z_j) ^ 2 / (2 * \sigma^2_\alpha))}
#'
#' @return a numeric vector containing the competitive effect caused by each
#' individual in the population on the focal individual.
#'
#' @author Theo Pannetier
#' @export

get_comp_coeff_pop <- function(
  trait_ind,
  traits_pop,
  comp_width = default_comp_width()) {

  testarg_num(trait_ind)
  testarg_length(trait_ind, 1) # not vectorized!
  testarg_num(traits_pop)
  testarg_num(comp_width)
  testarg_pos(comp_width) # is a variance

  trait_dist <- (trait_ind - traits_pop) ^ 2

  if (trait_ind %in% c(Inf, -Inf)) { # Inf - Inf raises NaNs
    nans <- which(traits_pop == trait_ind)
    trait_dist[nans] <- 0 # I rule full competition in this case
  }

  coeffs <- exp(- (trait_dist / (2 * comp_width ^ 2)))

  testarg_length(coeffs, length(traits_pop))

  # Solve possible NaN issues --------------------------------------------------
  # NaNs can arise if both terms in the division are equal to 0 or Inf
  if (comp_width == 0) {
    nans <- which(trait_dist == 0) # I rule that comp_width has precedence
    coeffs[nans] <- 1 # as if trait_dist / comp_width = 0
  } else if (comp_width == Inf) {
    nans <- which(trait_dist == Inf) # I rule that comp_width has precedence
    coeffs[nans] <- 1  # as if trait_dist / carr_cap_var = Inf
  }

  testarg_num(coeffs)
  testarg_prop(coeffs)

  coeffs
}
