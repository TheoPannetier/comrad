#' Draw a new community from the current one
#'
#' From the trait values of the current community, compute the fitness, draw
#' offspring, apply mutations and resolve speciation events.
#'
#' @inheritParams default_params_doc
#' @param seed integer \code{> 0}, the seed to set for the random number
#' generator. Defaults to an integer based on current day and time.
#'
#' @author Théo Pannetier
#' @export

draw_comm_next_gen <- function(
  comm,
  growth_rate = default_growth_rate(),
  comp_width = default_comp_width(),
  trait_opt = default_trait_opt(),
  carr_cap_opt = default_carr_cap_opt(),
  carr_cap_width = default_carr_cap_width(),
  prob_mutation = default_prob_mutation(),
  mutation_sd = default_mutation_sd(),
  trait_gap = default_trait_gap(),
  seed = comrad::default_seed()
) {

  # Test argument type ---------------------------------------------------------
  comrad::test_comrad_comm(comm)
  comrad::testarg_num(growth_rate)
  comrad::testarg_pos(growth_rate)
  comrad::testarg_num(comp_width)
  comrad::testarg_pos(comp_width)
  comrad::testarg_num(trait_opt)
  comrad::testarg_num(carr_cap_opt)
  comrad::testarg_pos(carr_cap_opt)
  comrad::testarg_num(carr_cap_width)
  comrad::testarg_pos(carr_cap_width)
  comrad::testarg_num(prob_mutation)
  comrad::testarg_prop(prob_mutation)
  comrad::testarg_num(mutation_sd)
  comrad::testarg_pos(mutation_sd)

  # Compute fitnesses
  fitness_comm <- comrad::get_fitness(
    traits_comm = comm$z,
    growth_rate = growth_rate,
    comp_width = comp_width,
    trait_opt = trait_opt,
    carr_cap_opt = carr_cap_opt,
    carr_cap_width = carr_cap_width
  )
  comrad::testarg_not_this(fitness_comm, Inf)

  # Create next generation from parent fitness ---------------------------------
  nb_offspring_comm <- comrad::draw_nb_offspring_cpp(
    fitness = fitness_comm,
    seed = seed
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
    prob_mutation = prob_mutation,
    mutation_sd = mutation_sd
  )
  # Resolve speciation ---------------------------------------------------------
  new_comm <- comrad::apply_speciation(
    comm = new_comm,
    trait_gap = trait_gap
  )
  comrad::test_comrad_comm(new_comm)

  new_comm
}
