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
  competition_sd = default_competition_sd(),
  trait_opt = default_trait_opt(),
  carrying_cap_opt = default_carrying_cap_opt(),
  carrying_cap_sd = default_carrying_cap_sd(),
  fitness_func = fitness_func_ricker) {

  # Test argument type ---------------------------------------------------------
  comrad::testarg_num(traits_comm)
  comrad::testarg_not_this(traits_comm, c(Inf, -Inf))
  comrad::testarg_num(growth_rate)
  comrad::testarg_pos(growth_rate)
  comrad::testarg_num(competition_sd)
  comrad::testarg_pos(competition_sd)
  comrad::testarg_num(trait_opt)
  comrad::testarg_num(carrying_cap_opt)
  comrad::testarg_pos(carrying_cap_opt)
  comrad::testarg_num(carrying_cap_sd)
  comrad::testarg_pos(carrying_cap_sd)

  # Compute effective population sizes -----------------------------------------
  n_eff <- comrad::get_n_eff(
    z = traits_comm,
    competition_sd = competition_sd
  ) # get the n_eff values experienced by each individual in the community

  # Compute k the carrying capacity --------------------------------------------
  carrying_cap <- comrad::get_carrying_cap(
    trait_ind = traits_comm,
    trait_opt = trait_opt,
    carrying_cap_opt = carrying_cap_opt,
    carrying_cap_sd = carrying_cap_sd
  )

  # Compute the fitness based on the Ricker model-------------------------------
  fitness <- fitness_func(
    growth_rate = growth_rate,
    n_eff = n_eff,
    carrying_cap = carrying_cap
  )
  comrad::testarg_num(fitness)
  comrad::testarg_length(fitness, length(traits_comm))

  fitness
}
