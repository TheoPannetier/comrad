#' Get the competition coefficient between two individuals
#'
#' Returns the competition coefficient \eqn{\alpha(zi, z_j)} between a focal
#' individual \eqn{i}, and a competitor \eqn{j} with trait values \eqn{z_i},
#' \eqn{z_j}.
#'
#' @inheritParams default_params_doc
#'
#' @details The equation is reported from equation (3) in Pontarp et al. (2012).
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

  trait_dist <- (trait_ind - trait_comp) ^ 2

  if (trait_ind %in% c(Inf, -Inf)) { # Inf - Inf raises NaNs
    nans <- which(trait_comp == trait_ind)
    trait_dist[nans] <- 0 # I rule full competition in this case
  }

  coeff <- exp(- (trait_dist / (2 * sigma_comp)))

  testarg_length(coeff, length(trait_comp))

  # Solve possible NaN issues --------------------------------------------------
  # NaNs can arise if both terms in the division are equal to 0 or Inf
  if (sigma_comp == 0) {
    nans <- which(trait_dist == 0) # I rule that sigma_comp has precedence
    coeff[nans] <- 1 # as if trait_dist / sigma_comp = 0
  } else if (sigma_comp == Inf) {
    nans <- which(trait_dist == Inf) # I rule that sigma_comp has precedence
    coeff[nans] <- 1  # as if trait_dist / carr_cap_var = Inf
  }

  testarg_num(coeff)
  testarg_prop(coeff)

  coeff
}
