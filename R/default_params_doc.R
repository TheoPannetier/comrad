#' Documentation for main parameters of the model
#'
#' This function does nothing. It is intended to inherit is parameters'
#' documentation.
#'
#' @param pop a tibble with one row per individual in the population and two
#' columns:
#'
#'  * `z` contains (numeric) traits values
#'  * `species` contains the species names (characters)
#'
#' @param trait_ind numeric. Trait value \eqn{z} of a focal individual.
#' @param traits_pop numeric vector of variable length, the trait values of
#' every individual in the population.
#' @param comp_width numeric \eqn{>= 0} 0. \eqn{\sigma^{2}_{\alpha}}. Controls
#' the intensity of competition given a distance between two trait values.
#' @param trait_opt numeric. Optimal trait value \eqn{z_{opt}}, such that
#' \code{get_carr_cap(trait_opt) = carr_cap_opt}.
#' @param carr_cap_opt numeric. Maximum carrying capacity at \eqn{z = z_{opt}}.
#' Note that this corresponds to the maximum number of **competitors** for that
#' trait value, as the focal individual does not contribute to the carrying
#' capacity (see also \code{\link{get_fitness}}).
#' @param carr_cap_width numeric \eqn{>= 0} 0. \eqn{\sigma^{2}_{K}}. Controls
#'  how fast the carrying capacity decays as the distance between a \code{trait}
#'  and \code{trait_opt} increases.
#' \eqn{\sigma^{2}_{K}}
#' @param growth_rate numeric \eqn{>= 0}, the baseline growth rate in the
#' absence of competition.
#' @param prob_mutation numeric between 0 and 1, the probability that any new
#' individual is sampled with a mutation.
#' @param mutation_sd numeric \eqn{>= 0}, the standard deviation of the normal
#' distrbution in which the mutations are drawn.
#' @param fitness numeric \eqn{>= 0} vector, containing the fitness values for
#' each individual as computed by \code{\link{get_fitness}}
#' @param nb_generations integer, the number of generations to run during the
#' simulation.
#' @param fitness_func, object. The function to use to compute fitness.
#' @param sim_tbl a tibble containing the output of a comrad simulation, as
#' produced by [run_simulation()] and read by [read_comrad_tbl()].
#'
#' @author Theo Pannetier, based on skeleton stolen from Richel J.C. Bilderbeek.

default_params_doc <- function(
  pop,
  trait_ind,
  traits_pop,
  comp_width,
  trait_opt,
  carr_cap_opt,
  carr_cap_width,
  growth_rate,
  prob_mutation,
  mutation_sd,
  fitness,
  nb_generations,
  fitness_func,
  sim_tbl
) {
  # Nuffin
}
