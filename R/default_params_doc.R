#' Documentation for main parameters of the model
#'
#' This function does nothing. It is intended to inherit is parameters'
#' documentation.
#'
#' @param comm a tibble with one row per individual in the community and three
#' columns:
#'
#'  * `z` contains (numeric) traits values
#'  * `species` contains species names (characters)
#'  * `ancestral_species` contains acnestral species names (characters)
#'
#' @param trait_ind numeric. Trait value \eqn{z} of a focal individual.
#' @param traits_comm numeric vector of variable length, the trait values of
#' every individual in the community.
#' @param comp_width numeric \eqn{>= 0}. Width of the competition kernel.
#' @param trait_opt numeric. The optimal trait value.
#' \code{get_carr_cap(trait_opt) = carr_cap_opt}.
#' @param carr_cap_opt numeric. Carrying capacity at the optimal trait value.
#' @param carr_cap_width numeric \eqn{>= 0}.
#' Width of the carrying capacity kernel.
#' @param growth_rate numeric \eqn{>= 0}, the baseline growth rate. Generations
#' being discrete, high values will cause chaos.
#' @param prob_mutation numeric between 0 and 1, the probability that any new
#' individual is sampled with a mutation.
#' @param mutation_sd numeric \eqn{>= 0}, the standard deviation of the normal
#' distrbution from which mutations are drawn.
#' @param nb_generations integer, the number of generations to run during the
#' simulation.
#' @param fitness_func, the name of the function used to compute fitness.
#' @param comrad_tbl a tibble containing the output of a `comrad` simulation,
#' as produced by [run_simulation()] or read by [read_comrad_tbl()].
#'
#' @author Theo Pannetier, based on skeleton stolen from Richel J.C. Bilderbeek.

default_params_doc <- function(
  comm,
  trait_ind,
  traits_comm,
  comp_width,
  trait_opt,
  carr_cap_opt,
  carr_cap_width,
  growth_rate,
  prob_mutation,
  mutation_sd,
  nb_generations,
  fitness_func,
  comrad_tbl
) {
  # Nuffin
}
