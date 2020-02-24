#' Get the fitness of an individual
#'
#' Computes the fitness of an individual or population based on its trait value
#' and the trait values of other individuals in the population. The fitness here
#' corresponds to the average number of offspring an individual can get in a
#' generation, as sampled in a Poisson distribution.
#'
#' @inheritParams default_params_doc
#'
#' @details The equation is a per-capita version of the Ricker model:
#' \deqn{G(z_i) = exp(r(1 - N_{eff} / K(z_i, z_opt)))}
#' where \eqn{N_{eff}} is the effective population size, i.e. sum of competitive
#' effects experienced by the focal individual for a given trait value:
#' \deqn{N_{eff} = sum_j(\alpha(z_i, z_j))}
#'
#' @author Theo Pannetier
#' @export

get_fitness <- function(
  traits_pop,
  growth_rate = default_growth_rate(),
  comp_width = default_comp_width(),
  trait_opt = default_trait_opt(),
  carr_cap_opt = default_carr_cap_opt(),
  carr_cap_width = default_carr_cap_width(),
  fitness_func = fitness_func_ricker
) {

  # Test argument type ---------------------------------------------------------
  comrad::testarg_num(traits_pop)
  comrad::testarg_num(growth_rate)
  comrad::testarg_pos(growth_rate)
  comrad::testarg_num(comp_width)
  comrad::testarg_pos(comp_width)
  comrad::testarg_num(trait_opt)
  comrad::testarg_num(carr_cap_opt)
  comrad::testarg_pos(carr_cap_opt)
  comrad::testarg_num(carr_cap_width)
  comrad::testarg_pos(carr_cap_width)

  # Compute effective population sizes -----------------------------------------
  n_eff <- comrad::get_n_eff(
    traits_pop = traits_pop,
    comp_width = comp_width
  ) # get the n_eff values experienced by each individual in the population

  # Compute k the carrying capacity --------------------------------------------
  carr_cap <- comrad::get_carr_cap(
    trait_ind = traits_pop,
    trait_opt = trait_opt,
    carr_cap_opt = carr_cap_opt,
    carr_cap_width = carr_cap_width
  )

  # Compute the fitness based on the Ricker model-------------------------------
  fitness <- fitness_func(
    growth_rate = growth_rate,
    n_eff = n_eff,
    carr_cap = carr_cap
  )
  comrad::testarg_num(fitness)
  comrad::testarg_length(fitness, length(traits_pop))

  fitness
}
