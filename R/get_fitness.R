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
#' @aliases get_fitness
#' @author Theo Pannetier
#' @export

get_fitness <- function(
                    traits_pop,
                    growth_rate = default_growth_rate(),
                    sigma_comp = default_sigma_comp(),
                    carr_cap_pars = default_carr_cap_pars()) {

  # Test argument type ---------------------------------------------------------
  testarg_num(traits_pop)
  testarg_num(growth_rate)
  testarg_pos(growth_rate)
  testarg_num(sigma_comp)
  testarg_pos(sigma_comp)
  testarg_not_this(sigma_comp, c(0, Inf)) # can cause NaNs in comp_coeff()
  testarg_num(carr_cap_pars)
  testarg_length(carr_cap_pars, 3)

  # Compute effective population sizes -----------------------------------------
  n_eff <- get_eff_pop_sizes(
    traits_pop = traits_pop,
    sigma_comp = sigma_comp
  ) # get the n_eff values experienced by each individual in the population

  # Compute k the carrying capacity --------------------------------------------
  k <- carr_cap(
    trait_ind = traits_pop,
    trait_opt = carr_cap_pars[1],
    carr_cap_opt = carr_cap_pars[2],
    carr_cap_var = carr_cap_pars[3]
  )

  # Compute the fitness based on the Ricker model-------------------------------
  fitness <- exp(growth_rate * (1 - n_eff / k)) # Ricker function

  testarg_num(fitness)
  testarg_length(fitness, length(traits_pop))

  fitness
}
