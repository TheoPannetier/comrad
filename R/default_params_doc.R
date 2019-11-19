#' This function does nothing. It is intended to inherit is parameters'
#' documentation.
#' @param trait_ind numeric. Trait value \eqn{z} of a focal individual.
#' @param trait_comp numeric, the trait value \eqn{z_j} of a competitor.
#' @param traits_pop numeric vector of variable length, the trait values of all
#' competitors in the population.
#' @param sigma_comp numeric > 0. Controls the competition intensity, i.e. the
#' strength of negative feedbacks the focal indiviudal experiences for a given
#' trait distance with the competitor.
#' @param trait_opt numeric. Optimal trait value \eqn{z_{opt}}, at which
#' \eqn{K = K_{0}}
#' @param carr_cap_opt numeric. Maximum carrying capacity at \eqn{z = z_{opt}}.
#' Note that this corresponds to the maximum number of **competitors** for that
#' trait value, as the focal individual does not contribute to the carrying
#' capacity (see also \code{\link{get_fitness}}).
#' @param carr_cap_var numeric. Variance of the carrying capacity
#' \eqn{\sigma^{2}_{K}}
#' @param growth_rate numeric \eqn{>= 0}, the baseline growth rate in the
#' absence of competition.
#' @param carr_cap_pars parameter vector with three elements: \enumerate{
#'    \item \code{trait_opt} numeric. Optimal trait value \eqn{z_{opt}}, at
#'    which \eqn{K = K_{0}}.
#'    \item carr_cap_opt numeric. Maximum carrying capacity at
#'    \eqn{z = z_{opt}}.
#' Note that this corresponds to the maximum number of **competitors** for that
#' trait value, as the focal individual does not contribute to the carrying
#' capacity (see also \code{\link{get_fitness}}).
#'    \item carr_cap_var numeric. Variance of the carrying capacity
#' \eqn{\sigma^{2}_{K}}
#' }
#'
#' @author Theo Pannetier, based on skeleton stolen from Richel J.C. Bilderbeek.

default_params_doc <- function(
  trait_ind,
  trait_comp,
  traits_pop,
  sigma_comp,
  trait_opt,
  carr_cap_opt,
  carr_cap_var,
  growth_rate,
  carr_cap_pars
) {
  # Nuffin
}
