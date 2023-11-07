#' Draw a new community from the current one
#'
#' From the trait values of the current community, compute the fitness, draw
#' offspring, apply mutations and resolve speciation events.
#'
#' @inheritParams default_params_doc
#'
#' @export

draw_comm_next_gen <- function(
  comm,
  growth_rate = default_growth_rate(),
  competition_sd = default_competition_sd(),
  trait_opt = default_trait_opt(),
  carrying_cap_opt = default_carrying_cap_opt(),
  carrying_cap_sd = default_carrying_cap_sd(),
  mutation_sd = default_mutation_sd(),
  trait_dist_sp = default_trait_dist_sp()
) {

  # Test argument type ---------------------------------------------------------
  comrad::test_comrad_comm(comm)
  comrad::testarg_num(growth_rate)
  comrad::testarg_pos(growth_rate)
  comrad::testarg_num(competition_sd)
  comrad::testarg_pos(competition_sd)
  comrad::testarg_num(trait_opt)
  comrad::testarg_num(carrying_cap_opt)
  comrad::testarg_pos(carrying_cap_opt)
  comrad::testarg_num(carrying_cap_sd)
  comrad::testarg_pos(carrying_cap_sd)
  comrad::testarg_num(mutation_sd)
  comrad::testarg_pos(mutation_sd)

  # Compute fitness
  fitness_comm <- comrad::get_fitness(
    traits_comm = comm$z,
    growth_rate = growth_rate,
    competition_sd = competition_sd,
    trait_opt = trait_opt,
    carrying_cap_opt = carrying_cap_opt,
    carrying_cap_sd = carrying_cap_sd
  )
  comrad::testarg_not_this(fitness_comm, Inf)

  # Create next generation from parent fitness ---------------------------------
  nb_offspring_comm <- comrad::draw_nb_offspring(
    fitness = fitness_comm
  )
  comrad::testarg_length(nb_offspring_comm, length(comm$z))

  new_comm <- tibble::tibble(
    "z" = rep(comm$z, nb_offspring_comm),
    "species" = rep(comm$species, nb_offspring_comm),
    "ancestral_species" = rep(comm$ancestral_species, nb_offspring_comm)
    # new community inherits traits and species from parents
  )
  comrad::testarg_length(new_comm$z, sum(nb_offspring_comm))

  # Catch extinction -----------------------------------------------------------
  if (length(new_comm$species) < 1) {
    return(new_comm)
  }
  # Draw and apply mutations ---------------------------------------------------
  new_comm$z <- comrad::apply_mutations(
    traits_comm = new_comm$z,
    mutation_sd = mutation_sd
  )
  # Resolve speciation ---------------------------------------------------------
  new_comm <- comrad::apply_speciation(
    comm = new_comm,
    trait_dist_sp = trait_dist_sp
  )
  comrad::test_comrad_comm(new_comm)

  return(new_comm)
}
