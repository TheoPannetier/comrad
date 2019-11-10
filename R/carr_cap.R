#' Get the carrying capacity for a given trait value
#'
#' Computes the carrying capacity experienced by an individual with trait value
#' \code{trait}.
#'
#' @param trait numeric. Trait value \eqn{z}.
#' @param trait_opt numeric. Optimal value for the trait \eqn{z_{opt}}, at which
#' \eqn{K = K_{0}}
#' @param carr_cap_opt numeric. Maximum carrying capacity at \eqn{z = z_{opt}}.
#' @param carr_cap_var numeric. Variance of the carrying capacity
#' \eqn{\sigma^{2}_{K}}
#'
#' @author Theo Pannetier
#' @export

carr_cap <- function(
  trait,
  trait_opt = 0,
  carr_cap_opt = 1000,
  carr_cap_var = 0.2
  ) {
  targ_num(trait, "trait")
  targ_pos(trait_opt, "trait_opt")
  targ_num(carr_cap_opt, "carr_cap_opt")
  targ_num(carr_cap_var, "carr_cap_var")

  dist <-  (trait_opt - trait) ^ 2
  exponent <- -(dist / 2 * carr_cap_var)
  carr_cap <- carr_cap_opt * exp(exponent)

  carr_cap
}
