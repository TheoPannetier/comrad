#' Compute fitness values
#'
#' Fitness values are computed for each individual from their trait value,
#' the fitness landscape defined by the carrying capacity parameters, and the
#' trait values of all other individuals in the community.
#'
#' @inheritParams default_params_doc
#'
#' @author Theo Pannetier
#' @export

get_fitness <- function(
  traits_comm,
  growth_rate = default_growth_rate(),
  comp_width = default_comp_width(),
  trait_opt = default_trait_opt(),
  carr_cap_opt = default_carr_cap_opt(),
  carr_cap_width = default_carr_cap_width(),
  fitness_func = fitness_func_ricker) {

  # Test argument type ---------------------------------------------------------
  comrad::testarg_num(traits_comm)
  comrad::testarg_not_this(traits_comm, c(Inf, -Inf))
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
  n_eff <- comrad::get_n_eff_cpp(
    z = traits_comm,
    comp_width = comp_width
  ) # get the n_eff values experienced by each individual in the community

  # Compute k the carrying capacity --------------------------------------------
  carr_cap <- comrad::get_carr_cap(
    trait_ind = traits_comm,
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
  comrad::testarg_length(fitness, length(traits_comm))

  fitness
}
